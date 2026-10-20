package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.newTempFolder;

public class JavaPKMSTServerCodegenTest {

    /**
     * General XML annotations test (both JAXB and Jackson)
     * <br>
     * Includes regression tests for:
     * - <a href="https://github.com/OpenAPITools/openapi-generator/issues/2417">Correct Jackson annotation when `wrapped: false`</a>
     */
    @Test
    public void shouldGenerateCorrectXmlAnnotations() {
        // Arrange
        final CodegenConfigurator config = new CodegenConfigurator()
                .addAdditionalProperty(CodegenConstants.WITH_XML, true)
                .addGlobalProperty(CodegenConstants.MODELS, "Pet")
                .addGlobalProperty(CodegenConstants.MODEL_DOCS, null)
                .addGlobalProperty(CodegenConstants.MODEL_TESTS, null)
                .setGeneratorName("java-pkmst")
                .setInputSpec("src/test/resources/3_0/java/xml-annotations-test.yaml")
                .setOutputDir(newTempFolder().toString());

        // Act
        final List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();

        // Assert
        JavaFileAssert.assertThat(files.get(0))
                .assertTypeAnnotations()
                .containsWithNameAndAttributes("JacksonXmlRootElement", Map.of("localName", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlRootElement", Map.of("name", "\"Pet\"", "namespace", "\"urn:jacksonxml\""))
                .containsWithNameAndAttributes("XmlAccessorType", Map.of("value", "XmlAccessType.FIELD"))
                .toType()

                // ↓ test custom-name on wrapper element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                .assertProperty("tags").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"Tag\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"TagList\"", "useWrapping", "true"))
                .toProperty().toType()

                // ↓ custom internal xml-array element name, non-wrapped (1st example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("friends").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"friend-pet\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toProperty().toType()

                // ↓ test custom element name (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Change%20Element%20Names)
                .assertProperty("status").assertPropertyAnnotations()
                .doesNotContainWithName("JacksonXmlElementWrapper")
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"PetStatus\""))
                .toProperty().toType()

                // ↓ test same-name wrapping element (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Wrapping%20Arrays)
                //   maps to 3rd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("photoUrls").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"photoUrls\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"photoUrls\"", "useWrapping", "true"))
                .toProperty().toType()

                // ↓ test attribute generation (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Convert%20Property%20to%20an%20Attribute)
                .assertProperty("name").assertPropertyAnnotations()
                .doesNotContainWithName("JacksonXmlElementWrapper")
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("isAttribute", "true", "localName", "\"name\""))
                .toProperty().toType()

                // ↓ test XML namespace and prefix (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Prefixes%20and%20Namespaces)
                .assertProperty("id").assertPropertyAnnotations()
                .doesNotContainWithName("JacksonXmlElementWrapper")
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"id\"", "namespace", "\"http://example.com/schema\""))
                .toProperty().toType()

                // ↓ external xml-array element name only (last example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("foods").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"yummy-yummy\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"yummy-yummy\""))
                .toProperty().toType()

                // ↓ internal xml-array element name (4th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("colors").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"color\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"colors\""))
                .toProperty().toType()

                // ↓ ignored external xml-array element name, non-wrapped (2nd example in https://spec.openapis.org/oas/v3.0.0#xml-arrays)
                .assertProperty("categories").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"Category\""))
                // ↓ specific regression test for #2417: (useWrapping=false) needs to be present
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("useWrapping", "false"))
                .toProperty().toType()

                // ↓ test custom-name on wrapper AND children (https://swagger.io/docs/specification/data-models/representing-xml/#:~:text=Use%20xml/name%20to%20give%20different%20names)
                //   maps to 5th example in https://spec.openapis.org/oas/v3.0.0#xml-arrays
                .assertProperty("activities").assertPropertyAnnotations()
                .containsWithNameAndAttributes("JacksonXmlProperty", Map.of("localName", "\"item\""))
                .containsWithNameAndAttributes("JacksonXmlElementWrapper", Map.of("localName", "\"activities-array\""));
    }
}
