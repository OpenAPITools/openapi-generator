package org.openapitools.codegen.kotlin.spring;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.languages.SpringCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

public class KotlinSpringServerCodegenTest {

    @Test(description = "test embedded enum array")
    public void embeddedEnumArrayTest() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/issue______kotlinArrayEnumEmbedded.yaml");
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "zz.yyyy.model.xxxx");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        String filePath = new File(output, "src/main/kotlin/zz/yyyy/model/xxxx/EmbeddedEnumArray.kt").getAbsolutePath().replace("\\", "/");
        String kotlinEmbArray = generator.getFiles().get(filePath);
        System.out.println("=====================");
        System.out.println(kotlinEmbArray);
        System.out.println("=====================");



    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.processOpts();

        final OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        openAPI.setInfo(new Info());
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.getLibrary(), KotlinSpringServerCodegen.SPRING_BOOT);
        Assert.assertTrue(codegen.supportedLibraries().containsKey(KotlinSpringServerCodegen.SPRING_BOOT));

        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assert.assertEquals(codegen.getBasePackage(), "org.openapitools");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.BASE_PACKAGE), "org.openapitools");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools");
        Assert.assertEquals(codegen.getServerPort(), "8080");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVER_PORT), "8080");
    }


    @Test
    public void testSettersForConfigValues() throws Exception {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setBasePackage("xx.yyyyyyyy.base");
        codegen.setServerPort("8181");
        codegen.setExceptionHandler(false);
        codegen.setGradleBuildFile(false);
        codegen.setSwaggerAnnotations(true);
        codegen.setServiceInterface(true);
        codegen.setServiceImplementation(true);
        codegen.setUseBeanValidation(false);
        codegen.setReactive(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        Assert.assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        Assert.assertEquals(codegen.getBasePackage(), "xx.yyyyyyyy.base");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.BASE_PACKAGE), "xx.yyyyyyyy.base");
        Assert.assertEquals(codegen.getServerPort(), "8181");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVER_PORT), "8181");
        Assert.assertFalse(codegen.getExceptionHandler());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.EXCEPTION_HANDLER), false);
        Assert.assertFalse(codegen.getGradleBuildFile());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.GRADLE_BUILD_FILE), false);
        Assert.assertTrue(codegen.getSwaggerAnnotations());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SWAGGER_ANNOTATIONS), true);
        Assert.assertTrue(codegen.getServiceInterface());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_INTERFACE), true);
        Assert.assertTrue(codegen.getServiceImplementation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION), true);
        Assert.assertFalse(codegen.getUseBeanValidation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.USE_BEANVALIDATION), false);
        Assert.assertFalse(codegen.isReactive());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.REACTIVE), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.BASE_PACKAGE, "xyz.yyyyy.bbbb.base");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVER_PORT, "8088");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.EXCEPTION_HANDLER, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.GRADLE_BUILD_FILE, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SWAGGER_ANNOTATIONS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_INTERFACE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_BEANVALIDATION, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, false);
        codegen.processOpts();

        final OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        openAPI.setInfo(new Info());
        openAPI.getInfo().setTitle("Some test API");
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.aaaaa.api");
        Assert.assertEquals(codegen.getBasePackage(), "xyz.yyyyy.bbbb.base");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.BASE_PACKAGE), "xyz.yyyyy.bbbb.base");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.TITLE), "someTest");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVER_PORT), "8088");
        Assert.assertFalse(codegen.getExceptionHandler());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.EXCEPTION_HANDLER), false);
        Assert.assertFalse(codegen.getGradleBuildFile());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.GRADLE_BUILD_FILE), false);
        Assert.assertTrue(codegen.getSwaggerAnnotations());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SWAGGER_ANNOTATIONS), true);
        Assert.assertTrue(codegen.getServiceInterface());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_INTERFACE), true);
        Assert.assertTrue(codegen.getServiceImplementation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION), true);
        Assert.assertFalse(codegen.getUseBeanValidation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.USE_BEANVALIDATION), false);
        Assert.assertFalse(codegen.isReactive());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.REACTIVE), false);
    }

    @Test
    public void testSettingInvokerPackageToBasePackage() {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.bbbb.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.bbbb.invoker");
        Assert.assertEquals(codegen.getBasePackage(), "xyz.yyyyy.bbbb.invoker");
    }

    @Test
    public void testDelegatePattern() {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.DELEGATE_PATTERN), true);
        Assert.assertEquals(codegen.additionalProperties().get("isDelegate"), "true");
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SWAGGER_ANNOTATIONS), false);
        Assert.assertTrue(codegen.getSwaggerAnnotations());

        Assert.assertEquals(codegen.apiTemplateFiles().get("apiController.mustache"), "Controller.kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiDelegate.mustache"), "Delegate.kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiInterface.mustache"), ".kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiInterface.mustache"), ".kt");

        Assert.assertTrue(codegen.supportingFiles().stream().anyMatch(supportingFile -> supportingFile.templateFile.equals("apiUtil.mustache")));
    }
}
