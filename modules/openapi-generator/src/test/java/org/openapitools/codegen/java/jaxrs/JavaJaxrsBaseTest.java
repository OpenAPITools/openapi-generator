package org.openapitools.codegen.java.jaxrs;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

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
        assertFileNotContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/model/Animal.java"), jsonTypeInfo, jsonSubType);
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

        assertFileNotContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java"), "DefaultValue");
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

        assertFileContains(Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java"), "DefaultValue");
    }

    @Test
    public void testAddOperationToGroupUseTagsTrue() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/tags.yaml");
        codegen.setUseTags(true);
        codegen.setOutputDir(output.getAbsolutePath());

        DefaultGenerator generator = new DefaultGenerator(true);
        generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen));
        var dryRunTMan = ((DryRunTemplateManager) generator.getTemplateProcessor()).enableTemplateDataCapturing();
        generator.generate();

        final var tag0ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Tag0Api.java"));
        Assert.assertEquals(tag0ApiTemplateData.get("baseName"), "Tag0");
        Assert.assertEquals(tag0ApiTemplateData.get("commonPath"), "");
        List<CodegenOperation> tag0 = getOperationsList(tag0ApiTemplateData);
        Assert.assertEquals(tag0.size(), 2);
        assertOperation(tag0.get(0), "Tag0", "/", false);
        assertOperation(tag0.get(1), "Tag0", "/{id}", true);

        final var tag1ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Tag1Api.java"));
        Assert.assertEquals(tag1ApiTemplateData.get("baseName"), "Tag1");
        Assert.assertEquals(tag1ApiTemplateData.get("commonPath"), "/group1/op1");
        List<CodegenOperation> tag1 = getOperationsList(tag1ApiTemplateData);
        Assert.assertEquals(tag1.size(), 1);
        assertOperation(tag1.get(0), "Tag1", "", false);

        final var tag2ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Tag2Api.java"));
        Assert.assertEquals(tag2ApiTemplateData.get("baseName"), "Tag2");
        Assert.assertEquals(tag2ApiTemplateData.get("commonPath"), "");
        List<CodegenOperation> tag2 = getOperationsList(tag2ApiTemplateData);
        Assert.assertEquals(tag2.size(), 2);
        assertOperation(tag2.get(0), "Tag2", "/group1/op2", true);
        assertOperation(tag2.get(1), "Tag2", "/group2/op3", true);

        final var defaultApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/DefaultApi.java"));
        Assert.assertEquals(defaultApiTemplateData.get("baseName"), "Default");
        Assert.assertEquals(defaultApiTemplateData.get("commonPath"), "/group3/op4");
        List<CodegenOperation> noTag = getOperationsList(defaultApiTemplateData);
        Assert.assertEquals(noTag.size(), 1);
        assertOperation(noTag.get(0), "Default", "", false);

        final var group4ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group4Api.java"));
        Assert.assertEquals(group4ApiTemplateData.get("baseName"), "Group4");
        Assert.assertEquals(group4ApiTemplateData.get("commonPath"), "/group4");
        List<CodegenOperation> group4 = getOperationsList(group4ApiTemplateData);
        Assert.assertEquals(group4.size(), 2);
        assertOperation(group4.get(0), "Group4", "/op5", true);
        assertOperation(group4.get(1), "Group4", "/op6", true);

        final var group5ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group5Api.java"));
        Assert.assertEquals(group5ApiTemplateData.get("baseName"), "Group5");
        Assert.assertEquals(group5ApiTemplateData.get("commonPath"), "");
        List<CodegenOperation> group5 = getOperationsList(group5ApiTemplateData);
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

        DefaultGenerator generator = new DefaultGenerator(true);
        generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen));
        var dryRunTMan = ((DryRunTemplateManager) generator.getTemplateProcessor()).enableTemplateDataCapturing();
        generator.generate();

        final var defaultApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/DefaultApi.java"));
        Assert.assertEquals(defaultApiTemplateData.get("baseName"), "default");
        Assert.assertEquals(defaultApiTemplateData.get("commonPath"), "");
        List<CodegenOperation> tag0 = getOperationsList(defaultApiTemplateData);
        Assert.assertEquals(tag0.size(), 2);
        assertOperation(tag0.get(0), "default", "/", false);
        assertOperation(tag0.get(1), "default", "/{id}", true);

        final var group1ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group1Api.java"));
        Assert.assertEquals(group1ApiTemplateData.get("baseName"), "group1");
        Assert.assertEquals(group1ApiTemplateData.get("commonPath"), "/group1");
        List<CodegenOperation> group1 = getOperationsList(group1ApiTemplateData);
        Assert.assertEquals(group1.size(), 2);
        assertOperation(group1.get(0), "group1", "/op1", true);
        assertOperation(group1.get(1), "group1", "/op2", true);

        final var group2ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group2Api.java"));
        Assert.assertEquals(group2ApiTemplateData.get("baseName"), "group2");
        Assert.assertEquals(group2ApiTemplateData.get("commonPath"), "/group2/op3");
        List<CodegenOperation> group2 = getOperationsList(group2ApiTemplateData);
        Assert.assertEquals(group2.size(), 1);
        assertOperation(group2.get(0), "group2", "", false);

        final var group3ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group3Api.java"));
        Assert.assertEquals(group3ApiTemplateData.get("baseName"), "group3");
        Assert.assertEquals(group3ApiTemplateData.get("commonPath"), "/group3/op4");
        List<CodegenOperation> group3 = getOperationsList(group3ApiTemplateData);
        Assert.assertEquals(group3.size(), 1);
        assertOperation(group3.get(0), "group3", "", false);

        final var group4ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group4Api.java"));
        Assert.assertEquals(group4ApiTemplateData.get("baseName"), "group4");
        Assert.assertEquals(group4ApiTemplateData.get("commonPath"), "/group4");
        List<CodegenOperation> group4 = getOperationsList(group4ApiTemplateData);
        Assert.assertEquals(group4.size(), 2);
        assertOperation(group4.get(0), "group4", "/op5", true);
        assertOperation(group4.get(1), "group4", "/op6", true);

        final var group5ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group5Api.java"));
        Assert.assertEquals(group5ApiTemplateData.get("baseName"), "group5");
        Assert.assertEquals(group5ApiTemplateData.get("commonPath"), "/group5/op7");
        List<CodegenOperation> group5 = getOperationsList(group5ApiTemplateData);
        Assert.assertEquals(group5.size(), 1);
        assertOperation(group5.get(0), "group5", "", false);

        final var group6ApiTemplateData = dryRunTMan.getCapturedTemplateData(output.toPath().resolve("src/gen/java/org/openapitools/api/Group6Api.java"));
        Assert.assertEquals(group6ApiTemplateData.get("baseName"), "group6");
        Assert.assertEquals(group6ApiTemplateData.get("commonPath"), "/group6/op8");
        List<CodegenOperation> group6 = getOperationsList(group6ApiTemplateData);
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

    @Test
    public void testExtraAnnotations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue_11772.yml");

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        TestUtils.assertExtraAnnotationFiles(outputPath + "/src/gen/java/org/openapitools/model");

    }

    @Test(description = "Validate that the generated equals()/hashCode() methods call super.equals() and super.hasCode()")
    public void testClassInheritanceEqualsHashCode() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        codegen.setOutputDir(output.getAbsolutePath());

        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/allOf_no_fields.yaml");
        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input)
                .generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        // Assert that the base class does not call super.equals() or super.hashCode()
        JavaFileAssert.assertThat(files.get("BaseClass.java")).assertMethod("equals").bodyNotContainsLines("super");
        JavaFileAssert.assertThat(files.get("BaseClass.java")).assertMethod("hashCode").bodyNotContainsLines("super");

        // Assert that the child class does call the super.equals and super.hashCode method
        assertCallsSuperInEqualsAndHashcode(files.get("ChildWithProperties.java"));
        assertCallsSuperInEqualsAndHashcode(files.get("ChildWithoutProperties.java"));
    }

    private static void assertCallsSuperInEqualsAndHashcode(File toCheck) {
        JavaFileAssert.assertThat(toCheck).assertMethod("equals").bodyContainsLines("super.equals");
        JavaFileAssert.assertThat(toCheck).assertMethod("hashCode").bodyContainsLines("super.hashCode");
    }
}
