package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaJerseyServerCodegen;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

public class JavaJerseyServerCodegenTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void before() {
        codegen = new JavaJerseyServerCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(JavaJerseyServerCodegen.SERVER_PORT), "8082");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xx.yyyyyyyy.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        codegen.additionalProperties().put("serverPort", "8088");
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(JavaJerseyServerCodegen.SERVER_PORT), "8088");
    }

    @Test
    public void testAddOperationToGroupUseTagsFalse() throws Exception {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/tags.yaml");
        ((JavaJerseyServerCodegen) codegen).setUseTags(false);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        WrittenTemplateBasedFile group1File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group1Api.java");
        Assert.assertEquals(group1File.getTemplateData().get("baseName"), "group1");
        Assert.assertEquals(group1File.getTemplateData().get("commonPath"), "group1");
        List<CodegenOperation> group1 = getOperationsList(group1File.getTemplateData());
        Assert.assertEquals(group1.size(), 2);
        Assert.assertEquals(group1.get(0).path, "/op1");
        Assert.assertEquals(group1.get(0).baseName, "group1");
        Assert.assertEquals(group1.get(0).subresourceOperation, true);
        Assert.assertEquals(group1.get(1).path, "/op2");
        Assert.assertEquals(group1.get(1).baseName, "group1");
        Assert.assertEquals(group1.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile group2File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group2Api.java");
        Assert.assertEquals(group2File.getTemplateData().get("baseName"), "group2");
        Assert.assertEquals(group2File.getTemplateData().get("commonPath"), "group2");
        List<CodegenOperation> group2 = getOperationsList(group2File.getTemplateData());
        Assert.assertEquals(group2.size(), 1);
        Assert.assertEquals(group2.get(0).path, "/op3");
        Assert.assertEquals(group2.get(0).baseName, "group2");
        Assert.assertEquals(group2.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile group3File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group3Api.java");
        Assert.assertEquals(group3File.getTemplateData().get("baseName"), "group3");
        Assert.assertEquals(group3File.getTemplateData().get("commonPath"), "group3");
        List<CodegenOperation> group3 = getOperationsList(group3File.getTemplateData());
        Assert.assertEquals(group3.size(), 1);
        Assert.assertEquals(group3.get(0).path, "/op4");
        Assert.assertEquals(group3.get(0).baseName, "group3");
        Assert.assertEquals(group3.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile group4File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group4Api.java");
        Assert.assertEquals(group4File.getTemplateData().get("baseName"), "group4");
        Assert.assertEquals(group4File.getTemplateData().get("commonPath"), "group4");
        List<CodegenOperation> group4 = getOperationsList(group4File.getTemplateData());
        Assert.assertEquals(group4.size(), 2);
        Assert.assertEquals(group4.get(0).path, "/op5");
        Assert.assertEquals(group4.get(0).baseName, "group4");
        Assert.assertEquals(group4.get(0).subresourceOperation, true);
        Assert.assertEquals(group4.get(1).path, "/op6");
        Assert.assertEquals(group4.get(1).baseName, "group4");
        Assert.assertEquals(group4.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile group5File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group5Api.java");
        Assert.assertEquals(group5File.getTemplateData().get("baseName"), "group5");
        Assert.assertEquals(group5File.getTemplateData().get("commonPath"), "group5");
        List<CodegenOperation> group5 = getOperationsList(group5File.getTemplateData());
        Assert.assertEquals(group5.size(), 1);
        Assert.assertEquals(group5.get(0).path, "/op7");
        Assert.assertEquals(group5.get(0).baseName, "group5");
        Assert.assertEquals(group5.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile group6File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group6Api.java");
        Assert.assertEquals(group6File.getTemplateData().get("baseName"), "group6");
        Assert.assertEquals(group6File.getTemplateData().get("commonPath"), "group6");
        List<CodegenOperation> group6 = getOperationsList(group6File.getTemplateData());
        Assert.assertEquals(group6.size(), 1);
        Assert.assertEquals(group6.get(0).path, "/op8");
        Assert.assertEquals(group6.get(0).baseName, "group6");
        Assert.assertEquals(group6.get(0).subresourceOperation, true);
    }

    @Test
    public void testAddOperationToGroupUseTagsTrue() throws Exception {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/tags.yaml");
        ((JavaJerseyServerCodegen) codegen).setUseTags(true);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        WrittenTemplateBasedFile tag1File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Tag1Api.java");
        Assert.assertEquals(tag1File.getTemplateData().get("baseName"), "Tag1");
        Assert.assertEquals(tag1File.getTemplateData().get("commonPath"), null);
        List<CodegenOperation> tag1List = getOperationsList(tag1File.getTemplateData());
        Assert.assertEquals(tag1List.size(), 1);
        Assert.assertEquals(tag1List.get(0).path, "/group1/op1");
        Assert.assertEquals(tag1List.get(0).baseName, null);
        Assert.assertEquals(tag1List.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile tag2File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Tag2Api.java");
        Assert.assertEquals(tag2File.getTemplateData().get("baseName"), "Tag2");
        Assert.assertEquals(tag2File.getTemplateData().get("commonPath"), null);
        List<CodegenOperation> tag2List = getOperationsList(tag2File.getTemplateData());
        Assert.assertEquals(tag2List.size(), 2);
        Assert.assertEquals(tag2List.get(0).path, "/group1/op2");
        Assert.assertEquals(tag2List.get(0).baseName, null);
        Assert.assertEquals(tag2List.get(0).subresourceOperation, true);
        Assert.assertEquals(tag2List.get(1).path, "/group2/op3");
        Assert.assertEquals(tag2List.get(1).baseName, null);
        Assert.assertEquals(tag2List.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile defaultFile = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/DefaultApi.java");
        Assert.assertEquals(defaultFile.getTemplateData().get("baseName"), "Default");
        Assert.assertEquals(defaultFile.getTemplateData().get("commonPath"), null);
        List<CodegenOperation> defaultList = getOperationsList(defaultFile.getTemplateData());
        Assert.assertEquals(defaultList.size(), 1);
        Assert.assertEquals(defaultList.get(0).path, "/group3/op4");
        Assert.assertEquals(defaultList.get(0).baseName, null);
        Assert.assertEquals(defaultList.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile group4File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group4Api.java");
        Assert.assertEquals(group4File.getTemplateData().get("baseName"), "Group4");
        Assert.assertEquals(group4File.getTemplateData().get("commonPath"), "group4");
        List<CodegenOperation> group4List = getOperationsList(group4File.getTemplateData());
        Assert.assertEquals(group4List.size(), 2);
        Assert.assertEquals(group4List.get(0).path, "/op5");
        Assert.assertEquals(group4List.get(0).baseName, "group4");
        Assert.assertEquals(group4List.get(0).subresourceOperation, true);
        Assert.assertEquals(group4List.get(1).path, "/op6");
        Assert.assertEquals(group4List.get(1).baseName, "group4");
        Assert.assertEquals(group4List.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile group5File = TestUtils.getTemplateBasedFile(generator, output, "src/gen/java/org/openapitools/api/Group5Api.java");
        Assert.assertEquals(group5File.getTemplateData().get("baseName"), "Group5");
        Assert.assertEquals(group5File.getTemplateData().get("commonPath"), null);
        List<CodegenOperation> group5List = getOperationsList(group5File.getTemplateData());
        Assert.assertEquals(group5List.size(), 2);
        Assert.assertEquals(group5List.get(0).path, "/group5/op7");
        Assert.assertEquals(group5List.get(0).baseName, null);
        Assert.assertEquals(group5List.get(0).subresourceOperation, true);
        Assert.assertEquals(group5List.get(1).path, "/group6/op8");
        Assert.assertEquals(group5List.get(1).baseName, null);
        Assert.assertEquals(group5List.get(1).subresourceOperation, true);
    }

    @SuppressWarnings("unchecked")
    private List<CodegenOperation> getOperationsList(Map<String, Object> templateData) {
        Assert.assertTrue(templateData.get("operations") instanceof Map);
        Map<String, Object> operations = (Map<String, Object>) templateData.get("operations");
        Assert.assertTrue(operations.get("operation") instanceof List);
        return (List<CodegenOperation>) operations.get("operation");
    }
}
