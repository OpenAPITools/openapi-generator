package org.openapitools.codegen.java.jaxrs;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public abstract class JavaJaxrsBaseTest {

    protected AbstractJavaJAXRSServerCodegen codegen;

    @Test
    public void generateJsonAnnotationForPolymorphism() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();


        String jsonTypeInfo = "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"className\", visible = true)";
        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = Dog.class, name = \"Dog\"),\n" +
                "  @JsonSubTypes.Type(value = Cat.class, name = \"Cat\"),\n" +
                "  @JsonSubTypes.Type(value = BigDog.class, name = \"BigDog\"),\n" +
                "})";
        checkFileContains(generator, outputPath + "/src/gen/java/org/openapitools/model/Animal.java", jsonTypeInfo, jsonSubType);
    }

    private void checkFileContains(MockDefaultGenerator generator, String path, String... lines) {
        String file = linearize(generator.getFiles().get(path));
        assertNotNull(file);
        for (String line : lines)
            assertTrue(file.contains(linearize(line)));
    }

    private String linearize(String target) {
        return target.replaceAll("\r?\n", "").replaceAll("\\s+", "\\s");
    }

    @Test
    public void doNotGenerateJsonAnnotationForPolymorphismIfJsonExclude() throws IOException {
        codegen.additionalProperties().put("jackson", false);
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();


        String jsonTypeInfo = "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"className\", visible = true)";
        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = Dog.class, name = \"Dog\"),\n" +
                "  @JsonSubTypes.Type(value = Cat.class, name = \"Cat\"),\n" +
                "})";
        checkFileNotContains(generator, outputPath + "/src/gen/java/org/openapitools/model/Animal.java",  jsonTypeInfo, jsonSubType);
    }

    private void checkFileNotContains(MockDefaultGenerator generator, String path, String... lines) {
        String file = linearize(generator.getFiles().get(path));
        assertNotNull(file);
        for (String line : lines)
            assertFalse(file.contains(linearize(line)));
    }

    @Test
    public void doNotAddDefaultValueDocumentationForContainers() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/arrayParameter.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileNotContains(generator, outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java",  "DefaultValue");
    }

    @Test
    public void addDefaultValueDocumentationForNonContainers() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/arrayParameter.yaml", null, new ParseOptions()).getOpenAPI();

        openAPI.getComponents().getParameters().get("operationsQueryParam").setSchema(new StringSchema()._default("default"));
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        checkFileContains(generator, outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java",  "DefaultValue");
    }
}
