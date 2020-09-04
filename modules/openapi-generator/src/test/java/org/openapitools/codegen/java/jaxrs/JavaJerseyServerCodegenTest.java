package org.openapitools.codegen.java.jaxrs;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.core.models.ParseOptions;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaJerseyServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

    // Helper function, intended to reduce boilerplate @ copied from ../spring/SpringCodegenTest.java
    private Map<String, String> generateFiles(DefaultCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        return generator.getFiles().entrySet().stream().collect(Collectors.toMap(e -> e.getKey().replace(outputPath, ""), Map.Entry::getValue));
    }

    // almost same test as issue #3139 on Spring
    @Test
    public void testMultipartJerseyServer() throws Exception {

        final Map<String, String> files = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");

        // Check files and parameters if List<> or Single
        final String[] fileS = new String[] {
                                        "/src/gen/java/org/openapitools/api/MultipartSingleApi.java",
                                        "/src/gen/java/org/openapitools/api/MultipartSingleApiService.java",
                                        "/src/main/java/org/openapitools/api/impl/MultipartSingleApiServiceImpl.java"};
        for (String f : fileS) {
           final String contents = files.get(f);
           Assert.assertTrue(contents.contains("FormDataBodyPart file"));
        }

        final String[] fileA = new String[] {
                                        "/src/gen/java/org/openapitools/api/MultipartArrayApiService.java",
                                        "/src/gen/java/org/openapitools/api/MultipartArrayApi.java",
                                        "/src/main/java/org/openapitools/api/impl/MultipartArrayApiServiceImpl.java"};
        for (String f : fileA) {
           final String contents = files.get(f);
           Assert.assertTrue(contents.contains("List<FormDataBodyPart> files"));
        }

    }

}
