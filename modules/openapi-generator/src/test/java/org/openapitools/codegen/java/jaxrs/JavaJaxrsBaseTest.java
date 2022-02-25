package org.openapitools.codegen.java.jaxrs;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.nio.file.Paths;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.AbstractJavaCodegen.JACKSON;

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

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        String jsonTypeInfo = "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"className\", visible = true)";
        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = BigDog.class, name = \"BigDog\"),\n" +
                "  @JsonSubTypes.Type(value = Cat.class, name = \"Cat\"),\n" +
                "  @JsonSubTypes.Type(value = Dog.class, name = \"Dog\"),\n" +
                "})";
        assertFileContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/model/Animal.java"), jsonTypeInfo, jsonSubType);
    }


    @Test
    public void doNotGenerateJsonAnnotationForPolymorphismIfJsonExclude() throws IOException {
        codegen.additionalProperties().put(JACKSON, false);
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/generic.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();


        String jsonTypeInfo = "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = \"className\", visible = true)";
        String jsonSubType = "@JsonSubTypes({\n" +
                "  @JsonSubTypes.Type(value = Cat.class, name = \"Cat\"),\n" +
                "  @JsonSubTypes.Type(value = Dog.class, name = \"Dog\"),\n" +
                "})";
        assertFileNotContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/model/Animal.java"),  jsonTypeInfo, jsonSubType);
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

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        assertFileNotContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java"),  "DefaultValue");
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

        ClientOptInput input = new ClientOptInput()
        .openAPI(openAPI)
        .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java"),  "DefaultValue");
    }

    @Test
    public void testAddOperationToGroupUseTagsTrue() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/tags.yaml");
        codegen.setUseTags(true);
        codegen.setOutputDir(output.getAbsolutePath());

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        MockDefaultGenerator.WrittenTemplateBasedFile tag0File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Tag0Api.java");
        Assert.assertEquals(tag0File.getTemplateData().get("baseName"), "Tag0");
        Assert.assertEquals(tag0File.getTemplateData().get("commonPath"), "");
        List<CodegenOperation> tag0 = getOperationsList(tag0File.getTemplateData());
        Assert.assertEquals(tag0.size(), 2);
        assertOperation(tag0.get(0), "Tag0", "/", false);
        assertOperation(tag0.get(1), "Tag0", "/{id}", true);

        MockDefaultGenerator.WrittenTemplateBasedFile tag1File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Tag1Api.java");
        Assert.assertEquals(tag1File.getTemplateData().get("baseName"), "Tag1");
        Assert.assertEquals(tag1File.getTemplateData().get("commonPath"), "/group1/op1");
        List<CodegenOperation> tag1 = getOperationsList(tag1File.getTemplateData());
        Assert.assertEquals(tag1.size(), 1);
        assertOperation(tag1.get(0), "Tag1", "", false);

        MockDefaultGenerator.WrittenTemplateBasedFile tag2File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Tag2Api.java");
        Assert.assertEquals(tag2File.getTemplateData().get("baseName"), "Tag2");
        Assert.assertEquals(tag2File.getTemplateData().get("commonPath"), "");
        List<CodegenOperation> tag2 = getOperationsList(tag2File.getTemplateData());
        Assert.assertEquals(tag2.size(), 2);
        assertOperation(tag2.get(0), "Tag2", "/group1/op2", true);
        assertOperation(tag2.get(1), "Tag2", "/group2/op3", true);

        MockDefaultGenerator.WrittenTemplateBasedFile defaultFile = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/DefaultApi.java");
        Assert.assertEquals(defaultFile.getTemplateData().get("baseName"), "Default");
        Assert.assertEquals(defaultFile.getTemplateData().get("commonPath"), "/group3/op4");
        List<CodegenOperation> noTag = getOperationsList(defaultFile.getTemplateData());
        Assert.assertEquals(noTag.size(), 1);
        assertOperation(noTag.get(0), "Default", "", false);

        MockDefaultGenerator.WrittenTemplateBasedFile group4File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group4Api.java");
        Assert.assertEquals(group4File.getTemplateData().get("baseName"), "Group4");
        Assert.assertEquals(group4File.getTemplateData().get("commonPath"), "/group4");
        List<CodegenOperation> group4 = getOperationsList(group4File.getTemplateData());
        Assert.assertEquals(group4.size(), 2);
        assertOperation(group4.get(0), "Group4", "/op5", true);
        assertOperation(group4.get(1), "Group4", "/op6", true);

        MockDefaultGenerator.WrittenTemplateBasedFile group5File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group5Api.java");
        Assert.assertEquals(group5File.getTemplateData().get("baseName"), "Group5");
        Assert.assertEquals(group5File.getTemplateData().get("commonPath"), "");
        List<CodegenOperation> group5 = getOperationsList(group5File.getTemplateData());
        Assert.assertEquals(group5.size(), 2);
        assertOperation(group5.get(0), "Group5", "/group5/op7", true);
        assertOperation(group5.get(1), "Group5", "/group6/op8", true);
    }

    @Test
    public void testAddOperationToGroupUseTagsFalse() throws Exception {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/tags.yaml");
        codegen.setUseTags(false);
        codegen.setOutputDir(output.getAbsolutePath());

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        MockDefaultGenerator.WrittenTemplateBasedFile tag0File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/DefaultApi.java");
        Assert.assertEquals(tag0File.getTemplateData().get("baseName"), "default");
        Assert.assertEquals(tag0File.getTemplateData().get("commonPath"), "");
        List<CodegenOperation> tag0 = getOperationsList(tag0File.getTemplateData());
        Assert.assertEquals(tag0.size(), 2);
        assertOperation(tag0.get(0), "default", "/", false);
        assertOperation(tag0.get(1), "default", "/{id}", true);

        MockDefaultGenerator.WrittenTemplateBasedFile group1File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group1Api.java");
        Assert.assertEquals(group1File.getTemplateData().get("baseName"), "group1");
        Assert.assertEquals(group1File.getTemplateData().get("commonPath"), "/group1");
        List<CodegenOperation> group1 = getOperationsList(group1File.getTemplateData());
        Assert.assertEquals(group1.size(), 2);
        assertOperation(group1.get(0), "group1", "/op1", true);
        assertOperation(group1.get(1), "group1", "/op2", true);

        MockDefaultGenerator.WrittenTemplateBasedFile group2File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group2Api.java");
        Assert.assertEquals(group2File.getTemplateData().get("baseName"), "group2");
        Assert.assertEquals(group2File.getTemplateData().get("commonPath"), "/group2/op3");
        List<CodegenOperation> group2 = getOperationsList(group2File.getTemplateData());
        Assert.assertEquals(group2.size(), 1);
        assertOperation(group2.get(0), "group2", "", false);

        MockDefaultGenerator.WrittenTemplateBasedFile group3File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group3Api.java");
        Assert.assertEquals(group3File.getTemplateData().get("baseName"), "group3");
        Assert.assertEquals(group3File.getTemplateData().get("commonPath"), "/group3/op4");
        List<CodegenOperation> group3 = getOperationsList(group3File.getTemplateData());
        Assert.assertEquals(group3.size(), 1);
        assertOperation(group3.get(0), "group3", "", false);

        MockDefaultGenerator.WrittenTemplateBasedFile group4File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group4Api.java");
        Assert.assertEquals(group4File.getTemplateData().get("baseName"), "group4");
        Assert.assertEquals(group4File.getTemplateData().get("commonPath"), "/group4");
        List<CodegenOperation> group4 = getOperationsList(group4File.getTemplateData());
        Assert.assertEquals(group4.size(), 2);
        assertOperation(group4.get(0), "group4", "/op5", true);
        assertOperation(group4.get(1), "group4", "/op6", true);

        MockDefaultGenerator.WrittenTemplateBasedFile group5File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group5Api.java");
        Assert.assertEquals(group5File.getTemplateData().get("baseName"), "group5");
        Assert.assertEquals(group5File.getTemplateData().get("commonPath"), "/group5/op7");
        List<CodegenOperation> group5 = getOperationsList(group5File.getTemplateData());
        Assert.assertEquals(group5.size(), 1);
        assertOperation(group5.get(0), "group5", "", false);

        MockDefaultGenerator.WrittenTemplateBasedFile group6File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group6Api.java");
        Assert.assertEquals(group6File.getTemplateData().get("baseName"), "group6");
        Assert.assertEquals(group6File.getTemplateData().get("commonPath"), "/group6/op8");
        List<CodegenOperation> group6 = getOperationsList(group6File.getTemplateData());
        Assert.assertEquals(group6.size(), 1);
        assertOperation(group6.get(0), "group6", "", false);
    }

    private void assertOperation(CodegenOperation op, String expectedBasename, String expectedPath, boolean expectedSubResourceOp) {
        Assert.assertEquals(op.path, expectedPath);
        Assert.assertEquals(op.baseName, expectedBasename);
        Assert.assertEquals(op.subresourceOperation, expectedSubResourceOp);
    }

    @SuppressWarnings("unchecked")
    private List<CodegenOperation> getOperationsList(Map<String, Object> templateData) {
        Assert.assertTrue(templateData.get("operations") instanceof Map);
        Map<String, Object> operations = (Map<String, Object>) templateData.get("operations");
        Assert.assertTrue(operations.get("operation") instanceof List);
        return (List<CodegenOperation>) operations.get("operation");
    }
}
