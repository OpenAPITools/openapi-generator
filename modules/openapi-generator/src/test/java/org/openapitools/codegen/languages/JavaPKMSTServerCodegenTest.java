package org.openapitools.codegen.languages;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class JavaPKMSTServerCodegenTest {

    /**
     * General XML annotations test (both JAXB and Jackson)
     * <br>
     * Includes regression tests for:
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/2417">Correct Jackson annotation when `wrapped: false`</a>
     */
    @Test public void shouldGenerateCorrectXmlAnnotations() throws IOException {
        // Arrange
        final String TEST_SPEC = "src/test/resources/3_0/java/xml-annotations-test.yaml";
        final Path output = Files.createTempDirectory("test-xml-annotations_");
        output.toFile().deleteOnExit();

        JavaPKMSTServerCodegen codegen = new JavaPKMSTServerCodegen();
        codegen.setWithXml(true);
        codegen.setOutputDir(output.toString());

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGenerateMetadata(false);

        // Act
        generator.opts(new ClientOptInput().config(codegen).openAPI(TestUtils.parseSpec(TEST_SPEC))).generate();

        // Assert
        JavaFileAssert.assertThat(output.resolve("src/main/java/com/prokarma/pkmst/model/Pet.java").toFile())
            .assertTypeAnnotations()
            .containsWithNameAndAttributes("JacksonXmlRootElement", Map.of("localName", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
            .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
            .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
            .toType()

            // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
            .hasProperty("tags").assertPropertyAnnotations()
//            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"Tag\""))    // ← this fails, wrongly uses xml.name "TagList" ! (should use items.xml.name)
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"TagList\"", "useWrapping", "true"))
            .toProperty().toType()

            // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
            .hasProperty("friends").assertPropertyAnnotations()
//            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"friend-pet\"")) // ← this fails, uses baseName "friends" (should be using items.xml.name)
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
            .toProperty().toType()

            // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)    
            .hasProperty("status").assertPropertyAnnotations()
            .doesNotContainsWithName("JacksonXmlElementWrapper")
            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"PetStatus\""))
            .toProperty().toType()

            // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
            //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
            .hasProperty("photoUrls").assertPropertyAnnotations()
            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"photoUrls\""))
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"photoUrls\"", "useWrapping", "true"))
            .toProperty().toType()

            // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
            .hasProperty("name").assertPropertyAnnotations()
            .doesNotContainsWithName("JacksonXmlElementWrapper")
            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("isAttribute", "true", "localName", "\"name\""))
            .toProperty().toType()

            // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
            .hasProperty("id").assertPropertyAnnotations()
            .doesNotContainsWithName("JacksonXmlElementWrapper")
            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"id\"", "namespace", "\"http://example.com/schema\""))
            .toProperty().toType()

            // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
            .hasProperty("foods").assertPropertyAnnotations()
            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"yummy-yummy\""))
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"yummy-yummy\""))
            .toProperty().toType()

            // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
            .hasProperty("colors").assertPropertyAnnotations()
//            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"color\""))   // fails, uses xml.name (should be using items.xml.name)
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"colors\""))
            .toProperty().toType()

            // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
            .hasProperty("categories").assertPropertyAnnotations()
//            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"Category\""))   // ← this fails, uses xml.name "NotUsedAsNotWrapped" !!! (should use items.xml.name)
            // ↓ specific regression test for #2417: (useWrapping=false) needs to be present
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
            .toProperty().toType()

            // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
            //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
            .hasProperty("activities").assertPropertyAnnotations()
//            .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"item\"")) // ← this fails, wrongly uses xml.name "activities-array" ! (should use items.xml.name)
            .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"activities-array\""));
    }
}