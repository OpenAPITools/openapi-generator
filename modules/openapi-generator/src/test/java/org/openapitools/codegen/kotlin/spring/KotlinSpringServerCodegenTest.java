package org.openapitools.codegen.kotlin.spring;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.io.FileUtils;
import org.assertj.core.api.Assertions;
import org.jetbrains.annotations.NotNull;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.kotlin.KotlinTestUtils;
import org.openapitools.codegen.kotlin.assertions.KotlinFileAssert;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DocumentationProvider;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.KotlinSpringServerCodegen.*;
import static org.openapitools.codegen.languages.SpringCodegen.REACTIVE;
import static org.openapitools.codegen.languages.SpringCodegen.SPRING_BOOT;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.ANNOTATION_LIBRARY;
import static org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DOCUMENTATION_PROVIDER;

public class KotlinSpringServerCodegenTest {

    @Test(description = "test embedded enum array")
    public void embeddedEnumArrayTest() throws Exception {
        String baseModelPackage = "zz";
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile(); //may be move to /build
        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue______kotlinArrayEnumEmbedded.yaml");
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, baseModelPackage + ".yyyy.model.xxxx");
        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();
        File resultSourcePath = new File(output, "src/main/kotlin");
        File outputModel = Files.createTempDirectory("test").toFile().getCanonicalFile();
        FileUtils.copyDirectory(new File(resultSourcePath, baseModelPackage), new File(outputModel, baseModelPackage));
        //no exception
        KotlinTestUtils.buildModule(Collections.singletonList(outputModel.getAbsolutePath()), Thread.currentThread().getContextClassLoader());
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
    public void testNoRequestMappingAnnotation_spring_cloud_default() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary("spring-cloud");

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/feat13488_use_kotlinSpring_with_springCloud.yaml"))
                        .config(codegen))
                .generate();

        // Check that the @RequestMapping annotation is not generated in the Api file
        assertFileNotContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "@RequestMapping(\"\\${api.base-path"
        );
    }

    @Test
    public void testNoRequestMappingAnnotationNone() throws IOException {
        File output = generatePetstoreWithRequestMappingMode(KotlinSpringServerCodegen.RequestMappingMode.none);

        // Check that the @RequestMapping annotation is not generated in the Api file
        assertFileNotContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApi.kt"),
                "@RequestMapping(\"\\${"
        );
        // Check that the @RequestMapping annotation is not generated in the ApiController file
        assertFileNotContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                "@RequestMapping(\"\\${"
        );
    }

    @Test
    public void testNoRequestMappingAnnotationController() throws IOException {
        File output = generatePetstoreWithRequestMappingMode(KotlinSpringServerCodegen.RequestMappingMode.controller);

        // Check that the @RequestMapping annotation is not generated in the Api file
        assertFileNotContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApi.kt"),
                "@RequestMapping(\"\\${"
        );
        // Check that the @RequestMapping annotation is generated in the ApiController file
        // Note: We use simple ${api.base-path:<default>} syntax because Spring's @RequestMapping
        // doesn't properly resolve nested ${outer:${inner:default}} property placeholder syntax
        assertFileContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                "@RequestMapping(\"\\${api.base-path:/v2}\")",
                "    companion object {\n"
                + "    //for your own safety never directly reuse these path definitions in tests\n"
                + "        const val BASE_PATH: String = \"/v2\"\n"
                + "    }"
        );
    }

    @Test
    public void testNoRequestMappingAnnotationApiInterface() throws IOException {
        File output = generatePetstoreWithRequestMappingMode(KotlinSpringServerCodegen.RequestMappingMode.api_interface);

        // Check that the @RequestMapping annotation is generated in the Api file
        // Note: We use simple ${api.base-path:<default>} syntax because Spring's @RequestMapping
        // doesn't properly resolve nested ${outer:${inner:default}} property placeholder syntax
        assertFileContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApi.kt"),
                "@RequestMapping(\"\\${api.base-path:/v2}\")",
                "    companion object {\n"
                + "        //for your own safety never directly reuse these path definitions in tests\n"
                + "        const val BASE_PATH: String = \"/v2\""
        );
        // Check that the @RequestMapping annotation is not generated in the ApiController file
        assertFileNotContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                "@RequestMapping(\"\\${"
        );
    }

    private static @NotNull File generatePetstoreWithRequestMappingMode(KotlinSpringServerCodegen.RequestMappingMode requestMappingMode) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REQUEST_MAPPING_OPTION, requestMappingMode);

        new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                                .config(codegen)
                )
                .generate();
        return output;
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
        codegen.setServiceInterface(true);
        codegen.setServiceImplementation(true);
        codegen.setUseBeanValidation(false);
        codegen.setReactive(false);
        codegen.setSerializableModel(true);
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
        Assert.assertTrue(codegen.getServiceInterface());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_INTERFACE), true);
        Assert.assertTrue(codegen.getServiceImplementation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION), true);
        Assert.assertFalse(codegen.getUseBeanValidation());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.USE_BEANVALIDATION), false);
        Assert.assertFalse(codegen.isReactive());
        Assert.assertEquals(codegen.additionalProperties().get(KotlinSpringServerCodegen.REACTIVE), false);
        Assert.assertTrue(codegen.isSerializableModel());
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.SERIALIZABLE_MODEL), true);
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

        Assert.assertEquals(codegen.apiTemplateFiles().get("apiController.mustache"), "Controller.kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiDelegate.mustache"), "Delegate.kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiInterface.mustache"), ".kt");
        Assert.assertEquals(codegen.apiTemplateFiles().get("apiInterface.mustache"), ".kt");

        Assert.assertTrue(codegen.supportingFiles().stream().anyMatch(supportingFile -> supportingFile.getTemplateFile().equals("apiUtil.mustache")));
    }

    @Test(description = "test delegate with tags")
    public void delegateWithTags() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile(); //may be move to /build
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue5497-use-tags-kotlin.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV2ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt")
        );
    }

    @Test(description = "test delegate reactive with tags")
    public void delegateReactiveWithTags() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile(); //may be move to /build
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue7325-use-delegate-reactive-tags-kotlin.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV2Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV2ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV3Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV3ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt")
        );

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "suspend fun");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "exchange");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "suspend fun");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "ApiUtil");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2Api.kt"),
                "import kotlinx.coroutines.flow.Flow", "ResponseEntity<Flow<kotlin.String>>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2Api.kt"),
                "exchange");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                "import kotlinx.coroutines.flow.Flow", "ResponseEntity<Flow<kotlin.String>>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                "suspend fun", "ApiUtil");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV3Api.kt"),
                "import kotlinx.coroutines.flow.Flow", "requestBody: Flow<kotlin.Long>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV3Api.kt"),
                "exchange");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt"),
                "import kotlinx.coroutines.flow.Flow", "suspend fun", "requestBody: Flow<kotlin.Long>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt"),
                "ApiUtil");
    }

    @Test
    public void testNullableMultipartFile() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/feat-multipartfile_nullable.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/NullableMultipartfileApiController.kt"),
                "file: org.springframework.web.multipart.MultipartFile?"
                + "    )");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/NullableMultipartfileArrayApiController.kt"),
                "files: Array<org.springframework.web.multipart.MultipartFile>?"
                + "    )");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/NonNullableMultipartfileApiController.kt"),
                "file: org.springframework.web.multipart.MultipartFile"
                + "    )");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/NonNullableMultipartfileArrayApiController.kt"),
                "files: Array<org.springframework.web.multipart.MultipartFile>"
                + "    )");
    }

    @Test
    public void arrayItemsCanBeNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/array-nullable-items.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/ArrayWithNullableItemsModel.kt"), "List<kotlin.String?>");
    }

    @Test
    public void doNotGenerateRequestParamForObjectQueryParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/objectQueryParam.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileNotContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PonyApiController.kt"), "@RequestParam");
    }

    @Test
    public void doGenerateRequestParamForSimpleParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_3248.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/MonkeysApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/ElephantsApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/ZebrasApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/BearsApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/CamelsApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PandasApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/CrocodilesApiController.kt"), "@RequestParam");
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PolarBearsApiController.kt"), "@RequestParam");
    }

    @Test
    public void generateFormatForDateAndDateTimeQueryParam() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_2053.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.opts(input).generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/ElephantsApiController.kt"),
                "@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/ZebrasApiController.kt"),
                "@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE_TIME)"
        );
    }

    @Test(description = "test bean qualifiers")
    public void beanQualifiers() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.BEAN_QUALIFIERS, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/bean-qualifiers.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PingApiController.kt"),
                "@RestController(\"org.openapitools.api.PingApiController\")"
        );
    }

    @Test(description = "test skip default interface")
    public void skipDefaultInterface() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SKIP_DEFAULT_INTERFACE, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/skip-default-interface.yaml"))
                        .config(codegen))
                .generate();

        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PingApi.kt"),
                "return "
        );
    }

    @Test(description = "test cookie parameter generation on interface apis")
    public void cookieParameterGenerationApis() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SKIP_DEFAULT_INTERFACE, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-fake-endpoints-for-testing-with-cookie.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/FakeApi.kt"),
                "@CookieValue"
        );
    }

    @Test(description = "test cookie parameter generation on controllers")
    public void cookieParameterGenerationControllers() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-fake-endpoints-for-testing-with-cookie.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/FakeApiController.kt"),
                "@CookieValue"
        );
    }

    @Test(description = "use Spring boot 3 & jakarta extension")
    public void useSpringBoot3() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT3, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/feat13578_use_springboot3_jakarta_extension.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/ApiUtil.kt"),
                "import jakarta.servlet.http.HttpServletResponse"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/Exceptions.kt"),
                "import jakarta.validation.ConstraintViolationException"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PingApiController.kt"),
                "import jakarta.validation.Valid"
        );
    }

    @Test(description = "multi-line descriptions should be supported for operations")
    public void multiLineOperationDescription() throws IOException {
        testMultiLineOperationDescription(false);
    }

    @Test(description = "multi-line descriptions should be supported for operations (interface-only)")
    public void multiLineOperationDescriptionInterfaceOnly() throws IOException {
        testMultiLineOperationDescription(true);
    }

    private static void testMultiLineOperationDescription(final boolean isInterfaceOnly) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY,
                isInterfaceOnly);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue4111-multiline-operation-description.yaml"))
                        .config(codegen))
                .generate();

        final String pingApiFileName;
        if (isInterfaceOnly) {
            pingApiFileName = "PingApi.kt";
        } else {
            pingApiFileName = "PingApiController.kt";
        }
        assertFileContains(
                Paths.get(
                        outputPath + "/src/main/kotlin/org/openapitools/api/" + pingApiFileName),
                "description = \"\"\"# Multi-line descriptions\n"
                + "\n"
                + "This is an example of a multi-line description.\n"
                + "\n"
                + "It:\n"
                + "- has multiple lines\n"
                + "- uses Markdown (CommonMark) for rich text representation\"\"\""
        );
    }

    @Test(description = "use get Annotation use-site target on kotlin interface attributes")
    public void useTargetOnInterfaceAnnotations() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue3596-use-correct-get-annotation-target.yaml"))
                        .config(codegen))
                .generate();

        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@Schema(example = \"null\", description = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(example = \"null\", description = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")"
        );
    }

    @Test(description = "use get Annotation use-site target on kotlin interface attributes (swagger1)")
    public void useTargetOnInterfaceAnnotationsWithSwagger1() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.SWAGGER1.toCliOptValue());
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.SPRINGFOX.toCliOptValue());

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue3596-use-correct-get-annotation-target.yaml"))
                        .config(codegen))
                .generate();

        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@ApiModelProperty(example = \"null\", value = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(example = \"null\", value = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@ApiModelProperty(example = \"null\", required = true, value = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(example = \"null\", required = true, value = \"\")"
        );
    }

    @Test
    public void useBeanValidationGenerateAnnotationsForRequestBody() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13932.yml", null, new ParseOptions()).getOpenAPI();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        assertFileContains(
                Paths.get(files.get("AddApi.kt").getAbsolutePath()),
                "@Min(value=2)"
        );
    }

    @Test
    public void contractWithoutEnumDoesNotContainEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/generic.yaml");

        assertThat(output).doesNotContainKey("EnumConverterConfiguration.kt");
    }

    @Test
    public void contractWithEnumContainsEnumConverter() throws IOException {
        Map<String, File> output = generateFromContract("src/test/resources/3_0/enum.yaml");

        File enumConverterFile = output.get("EnumConverterConfiguration.kt");
        assertThat(enumConverterFile).isNotNull();
        assertFileContains(enumConverterFile.toPath(), "fun typeConverter(): Converter<kotlin.String, Type> {");
        assertFileContains(enumConverterFile.toPath(), "return object: Converter<kotlin.String, Type> {");
        assertFileContains(enumConverterFile.toPath(), "override fun convert(source: kotlin.String): Type = Type.forValue(source)");
    }

    @Test
    public void contractWithResolvedInnerEnumContainsEnumConverter() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/inner_enum.yaml",
                new HashMap<>(),
                new HashMap<>(),
                configurator -> configurator.addInlineSchemaOption("RESOLVE_INLINE_ENUMS", "true")
        );

        File enumConverterFile = files.get("EnumConverterConfiguration.kt");
        assertThat(enumConverterFile).isNotNull();
        assertFileContains(enumConverterFile.toPath(), "fun ponyTypeConverter(): Converter<kotlin.String, PonyType> {");
        assertFileContains(enumConverterFile.toPath(), "return object: Converter<kotlin.String, PonyType> {");
        assertFileContains(enumConverterFile.toPath(), "override fun convert(source: kotlin.String): PonyType = PonyType.forValue(source)");
    }

    @Test
    public void givenMultipartFormArray_whenGenerateDelegateAndService_thenParameterIsCreatedAsListOfMultipartFile() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/kotlin/petstore-with-tags.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setDelegatePattern(true);
        codegen.setServiceInterface(true);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path delegateFile = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PetApiDelegate.kt");
        assertFileContains(delegateFile, "additionalMetadata: kotlin.String?");
        assertFileContains(delegateFile, "images: Array<org.springframework.web.multipart.MultipartFile>");

        Path controllerFile = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PetApi.kt");
        assertFileContains(controllerFile, "images: Array<org.springframework.web.multipart.MultipartFile>");

        Path serviceFile = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PetApiService.kt");
        assertFileContains(serviceFile, "images: Array<org.springframework.web.multipart.MultipartFile>");
    }

    @Test
    public void givenOctetStreamResponseType_whenGenerateServer_thenReturnTypeIsResource() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/kotlin/petstore-with-tags.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path outputFilepath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PetApiController.kt");

        assertFileContains(outputFilepath, "): ResponseEntity<org.springframework.core.io.Resource>");
    }

    @Test
    public void givenMultipartForm_whenGenerateReactiveServer_thenParameterAreCreatedAsRequestPart() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/kotlin/petstore-with-tags.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path outputFilepath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PetApiController.kt");

        assertFileContains(outputFilepath,
                "@Parameter(description = \"Additional data to pass to server\") @Valid @RequestParam(value = \"additionalMetadata\", required = false) additionalMetadata: kotlin.String?");
        assertFileContains(outputFilepath,
                "@Parameter(description = \"image to upload\") @Valid @RequestPart(\"image\", required = false) image: org.springframework.web.multipart.MultipartFile");

    }

    @Test
    public void overridePropertyFunction() throws IOException {

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/bugs/issue_20228.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pony.kt"),
                "override val nameOpt", "override val nameReq"
        );
    }

    @Test
    public void generateSerializableModel() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt");
        assertFileContains(
                path,
                ") : java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    @Test
    public void generateSerializableModelWithXimplements() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Dog.kt");
        assertFileContains(
                path,
                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                ") : Pet, com.some.pack.Fetchable, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    @Test
    public void generateSerializableModelWithXimplementsSkip() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);
        codegen.additionalProperties().put(X_KOTLIN_IMPLEMENTS_SKIP, List.of("com.some.pack.Fetchable"));
        codegen.additionalProperties().put(X_KOTLIN_IMPLEMENTS_FIELDS_SKIP, Map.of("Dog", List.of("likesFetch")));

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Dog.kt");
        assertFileContains(
                path,
                "@get:JsonProperty(\"likesFetch\", required = true) val likesFetch: kotlin.Boolean,",
                ") : Pet, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    @Test
    public void generateSerializableModelWithSchemaImplements() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SCHEMA_IMPLEMENTS, Map.of(
                "Pet", "com.some.pack.WithId",
                "Category", List.of("com.some.pack.CategoryInterface"),
                "Dog", List.of("com.some.pack.Canine")
        ));
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SCHEMA_IMPLEMENTS_FIELDS, Map.of(
                "Pet", List.of("id"),
                "Category", List.of("name", "id"),
                "Dog", List.of("bark", "breed")
        ));

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path dog = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Dog.kt");
        assertFileContains(
                dog,
                "@get:JsonProperty(\"bark\", required = true) override val bark: kotlin.Boolean,",
                "@get:JsonProperty(\"breed\", required = true) override val breed: Dog.Breed,",
                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                "@get:JsonProperty(\"name\", required = true) override val name: kotlin.String,",
                "@get:JsonProperty(\"photoUrls\", required = true) override val photoUrls: kotlin.collections.List<kotlin.String>,",
                "@get:JsonProperty(\"petType\", required = true) override val petType: kotlin.String,",
                "@get:JsonProperty(\"id\") override val id: kotlin.Long? = null,",
                "@get:JsonProperty(\"category\") override val category: Category? = null,",
                "@get:JsonProperty(\"tags\") override val tags: kotlin.collections.List<Tag>? = null,",
                "@get:JsonProperty(\"color\") override val color: Color? = null",
                ") : Pet, com.some.pack.Canine, com.some.pack.Fetchable, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );

        Path pet = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt");
        assertFileContains(
                pet,
                "interface Pet : com.some.pack.Named, com.some.pack.WithCategory, com.some.pack.WithDefaultMethods, com.some.pack.WithId, com.some.pack.WithPhotoUrls, java.io.Serializable {",
                "override val name: kotlin.String",
                "val photoUrls: kotlin.collections.List<kotlin.String>",
                "val petType: kotlin.String",
                "override val id: kotlin.Long?",
                "override val category: Category?",
                "val tags: kotlin.collections.List<Tag>?",
                "val color: Color?",
                "private const val serialVersionUID: kotlin.Long = 1"
        );

        Path category = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Category.kt");
        assertFileContains(
                category,
                "@get:JsonProperty(\"id\") override val id: kotlin.Long? = null,",
                "@get:JsonProperty(\"name\") override val name: kotlin.String? = null",
                ") : com.some.pack.CategoryInterface, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    @Test
    public void generateSerializableModelWithXimplementsSkipAndSchemaImplements() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.SERIALIZABLE_MODEL, true);
        //remove the old interface with likesFetch attribute here
        codegen.additionalProperties().put(X_KOTLIN_IMPLEMENTS_SKIP, List.of("com.some.pack.Fetchable"));
        codegen.additionalProperties().put(X_KOTLIN_IMPLEMENTS_FIELDS_SKIP, Map.of("Dog", List.of("likesFetch")));
        //and add a new one that again should mark likesFetch as override
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SCHEMA_IMPLEMENTS, Map.of("Dog", List.of("com.some.different.pack.MyOwnFetchable")
        ));
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SCHEMA_IMPLEMENTS_FIELDS, Map.of("Dog", List.of("likesFetch")));

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Dog.kt");
        assertFileContains(
                path,
                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                ") : Pet, com.some.different.pack.MyOwnFetchable, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithReactorResponseEntity() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, true);
        codegen.additionalProperties().put(DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, true);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "none");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        assertFileContains(
                path,
                "import reactor.core.publisher.Flux\n"
                + "import reactor.core.publisher.Mono",
                "    @HttpExchange(\n"
                + "        // \"/store/inventory\"\n"
                + "        url = PATH_GET_INVENTORY,\n"
                + "        method = \"GET\"\n"
                + "    )\n"
                + "    fun getInventory(\n"
                + "    ): Mono<ResponseEntity<Map<String, kotlin.Int>>>",
                "    @HttpExchange(\n"
                + "        // \"/store/order/{orderId}\"\n"
                + "        url = PATH_DELETE_ORDER,\n"
                + "        method = \"DELETE\"\n"
                + "    )\n"
                + "    fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): Mono<ResponseEntity<Unit>>",
                "    @HttpExchange(\n"
                + "        // \"/store/order\"\n"
                + "        url = PATH_PLACE_ORDER,\n"
                + "        method = \"POST\"\n"
                + "    )\n"
                + "    fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): Mono<ResponseEntity<Order>>",
                "    companion object {\n"
                + "        //for your own safety never directly reuse these path definitions in tests\n"
                + "        const val BASE_PATH: String = \"/v2\"\n"
                + "        const val PATH_DELETE_ORDER: String = \"/store/order/{orderId}\"\n"
                + "        const val PATH_GET_INVENTORY: String = \"/store/inventory\"\n"
                + "        const val PATH_GET_ORDER_BY_ID: String = \"/store/order/{orderId}\"\n"
                + "        const val PATH_PLACE_ORDER: String = \"/store/order\"\n"
                + "    }"
        );
        assertFileNotContains(
                path,
                "suspend",
                "@HttpExchange(BASE_PATH)" // this should not be present since "requestMappingMode" is set to "none"
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithCoroutinesResponseEntity() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, true);
        codegen.additionalProperties().put(DECLARATIVE_INTERFACE_REACTIVE_MODE, "coroutines");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, true);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "none");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        assertFileContains(
                path,
                "    suspend fun getInventory(\n"
                + "    ): ResponseEntity<Map<String, kotlin.Int>>",
                "    suspend fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): ResponseEntity<Unit>",
                "    suspend fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): ResponseEntity<Order>"
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithReactor() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, true);
        codegen.additionalProperties().put(DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, false);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "none");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        assertFileContains(
                path,
                "import reactor.core.publisher.Flux\n"
                + "import reactor.core.publisher.Mono",
                "    fun getInventory(\n"
                + "    ): Mono<Map<String, kotlin.Int>>",
                "    fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): Mono<Unit>",
                "    fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): Mono<Order>"
        );
        assertFileNotContains(
                path,
                "suspend"
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithCoroutines() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, true);
        codegen.additionalProperties().put(DECLARATIVE_INTERFACE_REACTIVE_MODE, "coroutines");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, false);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "none");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        assertFileContains(
                path,
                "    suspend fun getInventory(\n"
                + "    ): Map<String, kotlin.Int>",
                "    suspend fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): Unit",
                "    suspend fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): Order"
        );
    }

    @Test
    public void generateHttpInterfaceResponseEntity() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, false);
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, true);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "none");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        assertFileContains(
                path,
                "    fun getInventory(\n"
                + "    ): ResponseEntity<Map<String, kotlin.Int>>",
                "    fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): ResponseEntity<Unit>",
                "    fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): ResponseEntity<Order>"
        );
        assertFileNotContains(
                path,
                "suspend"
        );
    }

    @Test
    public void generateHttpInterface() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(REACTIVE, false);
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, false);
        codegen.additionalProperties().put(REQUEST_MAPPING_OPTION, "api_interface");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/StoreApi.kt");
        // Note: We cannot use property placeholders as HttpServiceProxyFactory does not resolve them by default.
        assertFileContains(
                path,
                "import org.openapitools.api.StoreApi.Companion.BASE_PATH",
                "@HttpExchange(BASE_PATH) // Generate with 'requestMappingMode' set to 'none' to skip the base path on the interface", // this should be present since "requestMappingMode" is set to "api_interface"
                "    fun getInventory(\n"
                + "    ): Map<String, kotlin.Int>",
                "    fun deleteOrder(\n"
                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                + "    ): Unit",
                "    fun placeOrder(\n"
                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                + "    ): Order"
        );
        assertFileNotContains(
                path,
                "suspend"
        );
    }

    @Test
    public void generateNonSerializableModelWithXimplements() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml"))
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Dog.kt");
        assertFileContains(
                path,
                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                ") : Pet, com.some.pack.Fetchable {"
        );
        assertFileNotContains(
                path,
                "import java.io.Serializable",
                "Serializable",
                ") : Pet, java.io.Serializable,  com.some.pack.Fetchable {",
                ") : Pet, java.io.Serializable {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );
    }

    private Path generateApiSources(
            Map<String, Object> additionalProperties,
            Map<String, String> generatorPropertyDefaults
    ) throws Exception {
        File outputDir = Files.createTempDirectory("test").toFile().getCanonicalFile();
        outputDir.deleteOnExit();
        String outputPath = outputDir.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(outputDir.getAbsolutePath());
        codegen.additionalProperties().putAll(additionalProperties);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/petstore.yaml"))
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        for (var entry : generatorPropertyDefaults.entrySet()) {
            generator.setGeneratorPropertyDefault(entry.getKey(), entry.getValue());
        }
        generator.opts(input).generate();

        return Paths.get(outputPath);
    }

    private void verifyGeneratedFilesContain(Map<Path, List<String>> expectedSnippetsByPathsToFiles) {
        for (var expectedSnippetsByPathToFile : expectedSnippetsByPathsToFiles.entrySet()) {
            assertFileContains(expectedSnippetsByPathToFile.getKey(), expectedSnippetsByPathToFile.getValue().toArray(new String[0]));
        }
    }

    private void verifyGeneratedFilesNotContain(Map<Path, List<String>> unexpectedSnippetsByPathsToFiles) {
        for (var unexpectedSnippetsByPathToFile : unexpectedSnippetsByPathsToFiles.entrySet()) {
            assertFileNotContains(unexpectedSnippetsByPathToFile.getKey(), unexpectedSnippetsByPathToFile.getValue().toArray(new String[0]));
        }
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwaggerNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange)")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwagger1NoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextControllerImplAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwaggerNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwagger1NoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextControllerImplAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1NoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1NoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegateWithApiTests() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.API_TESTS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/test/kotlin/org/openapitools/api/PetApiTest.kt"), List.of(
                                "val request: javax.servlet.http.HttpServletRequest = TODO()",
                                "api.deletePet(petId, apiKey, request)"),
                        root.resolve("src/test/kotlin/org/openapitools/api/UserApiTest.kt"), List.of(
                                "val request: javax.servlet.http.HttpServletRequest = TODO()",
                                "api.logoutUser(request)")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegateWithApiTests() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.API_TESTS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange)"),
                        root.resolve("src/test/kotlin/org/openapitools/api/PetApiTest.kt"), List.of(
                                "val exchange: org.springframework.web.server.ServerWebExchange = TODO()",
                                "api.deletePet(petId, apiKey, exchange)"),
                        root.resolve("src/test/kotlin/org/openapitools/api/UserApiTest.kt"), List.of(
                                "val exchange: org.springframework.web.server.ServerWebExchange = TODO()",
                                "api.logoutUser(exchange)")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwaggerDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwagger1Delegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextControllerImplAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwaggerDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwagger1Delegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "         @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "         @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "         @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "         @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "         @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "fun deletePet(\n"
                                + "         @PathVariable(\"petId\") petId: kotlin.Long,\n"
                                + "         @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,\n"
                                + "        request: javax.servlet.http.HttpServletRequest\n"
                                + "    ): ResponseEntity<Unit> {",
                                "fun getPetById(\n"
                                + "         @PathVariable(\"petId\") petId: kotlin.Long,\n"
                                + "        request: javax.servlet.http.HttpServletRequest\n"
                                + "    ): ResponseEntity<Pet> {"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextControllerImplAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, false,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1Delegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1Delegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, true,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.INCLUDE_HTTP_REQUEST_CONTEXT, false,
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutResponseEntity() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.BAD_REQUEST)",
                                "suspend fun deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): Unit",
                                "@ResponseStatus(HttpStatus.OK)",
                                "suspend fun getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): Pet"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.OK)",
                                "suspend fun logoutUser(): Unit"
                        ),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.OK)",
                                "suspend fun getInventory(): Map<String, kotlin.Int>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutResponseEntity() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.BAD_REQUEST)",
                                "fun deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): Unit",
                                "@ResponseStatus(HttpStatus.OK)",
                                "fun getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): Pet"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.OK)",
                                "fun logoutUser(): Unit"
                        ),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of(
                                "@ResponseStatus(HttpStatus.OK)",
                                "fun getInventory(): Map<String, kotlin.Int>")
                )
        );

        verifyGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("suspend"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("suspend"),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("suspend")
                )
        );
    }

    @Test
    public void reactiveWithResponseEntity() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "suspend fun deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "suspend fun getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "suspend fun logoutUser(): ResponseEntity<Unit>"
                        ),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of(
                                "suspend fun getInventory(): ResponseEntity<Map<String, kotlin.Int>>")
                )
        );

        verifyGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("@ResponseStatus(HttpStatus.")
                )
        );
    }

    @Test
    public void nonReactiveWithResponseEntity() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        verifyGeneratedFilesContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "fun deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "fun getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "fun logoutUser(): ResponseEntity<Unit>"
                        ),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of(
                                "fun getInventory(): ResponseEntity<Map<String, kotlin.Int>>")
                )
        );

        verifyGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus.")
                )
        );
    }

    @Test
    public void reactiveWithoutFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiService.kt")
        );

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "Flow<kotlin.String>");
    }

    @Test
    public void reactiveWithFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiService.kt")
        );

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                "List<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "Flow<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "Flow<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "Flow<kotlin.String>");
    }

    @Test
    public void reactiveWithDefaultValueFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        // should use default 'true' instead
        // codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiService.kt")
        );

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                "List<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "Flow<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "Flow<kotlin.String>");

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "List<kotlin.String>");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "Flow<kotlin.String>");
    }

    @Test
    public void nonReactiveWithoutFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiService.kt")
        );

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "Flow<kotlin.String>");
    }

    @Test
    public void nonReactiveWithFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_TAGS, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/TestV1ApiService.kt")
        );

        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                "Flow<kotlin.String>");

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "List<kotlin.String>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"),
                "Flow<kotlin.String>");
    }

    @Test
    public void testValidationsInQueryParams_issue21238_Controller() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue21238_queryParam_validation.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/UserApiController.kt")
        );

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                "@NotNull", "@Valid");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/UserApiController.kt"),
                "@NotNull", "@Valid",
                "@Pattern(regexp=\"^[a-zA-Z0-9]+[a-zA-Z0-9\\\\.\\\\-_]*[a-zA-Z0-9]+$\")",
                "@Parameter(description = \"The user name for login\", required = true)",
                "@Parameter(description = \"The password for login in clear text\", required = true)");
    }

    @Test
    public void testValidationsInQueryParams_issue21238_Api_Delegate() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DELEGATE_PATTERN, true);

        List<File> files = new DefaultGenerator()
                .opts(
                        new ClientOptInput()
                                .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue21238_queryParam_validation.yaml"))
                                .config(codegen)
                )
                .generate();

        Assertions.assertThat(files).contains(
                new File(output, "src/main/kotlin/org/openapitools/api/PetApi.kt"),
                new File(output, "src/main/kotlin/org/openapitools/api/UserApi.kt")
        );

        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApi.kt"),
                "@NotNull", "@Valid");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/UserApi.kt"),
                "@NotNull", "@Valid", "@Pattern(regexp=\"^[a-zA-Z0-9]+[a-zA-Z0-9\\\\.\\\\-_]*[a-zA-Z0-9]+$\")");
    }

    @DataProvider
    public Object[][] issue17997DocumentationProviders() {
        return new Object[][] {
                { DocumentationProviderFeatures.DocumentationProvider.SPRINGDOC.name(),
                        (Consumer<Path>) outputPath ->
                                assertFileContains(
                                        outputPath,
                                        "allowableValues = [\"0\", \"1\"], defaultValue = \"0\"",
                                        "@PathVariable"
                                ),
                        (Consumer<Path>) outputPath ->
                                assertFileContains(
                                        outputPath,
                                        "allowableValues = [\"sleeping\", \"awake\"]", "@PathVariable",
                                        "@PathVariable"
                                )
                },
                { DocumentationProviderFeatures.DocumentationProvider.SPRINGFOX.name(),
                        (Consumer<Path>) outputPath ->
                                assertFileContains(
                                        outputPath,
                                        "allowableValues = \"0, 1\", defaultValue = \"0\"",
                                        "@PathVariable"
                                ),
                        (Consumer<Path>) outputPath ->
                                assertFileContains(
                                        outputPath,
                                        "allowableValues = \"sleeping, awake\"", "@PathVariable",
                                        "@PathVariable"
                                )
                }
        };
    }

    @Test(dataProvider = "issue17997DocumentationProviders")
    public void testDocumentationAnnotationInPathParams_Issue17997(
            String documentProvider,
            Consumer<Path> intEnumAssertFunction,
            Consumer<Path> stringEnumAssertFunction
    ) throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(DOCUMENTATION_PROVIDER, documentProvider);

        Map<String, String> generatorPropertyDefaults = new HashMap<>();
        generatorPropertyDefaults.put(CodegenConstants.MODEL_TESTS, "false");
        generatorPropertyDefaults.put(CodegenConstants.MODEL_DOCS, "false");
        generatorPropertyDefaults.put(CodegenConstants.APIS, "true");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/issue_6762.yaml",
                additionalProperties,
                generatorPropertyDefaults
        );

        Stream.of(
                "ZebrasApiController.kt",
                "GiraffesApiController.kt"
        ).forEach(filename -> {
            File file = files.get(filename);
            assertThat(file).isNotNull();
            intEnumAssertFunction.accept(file.toPath());
        });

        Stream.of(
                "BearsApiController.kt",
                "CamelsApiController.kt"
        ).forEach(filename -> {
            File file = files.get(filename);
            assertThat(file).isNotNull();
            stringEnumAssertFunction.accept(file.toPath());
        });
    }

    @Test
    public void testXSizeMessage_length() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/error-message-for-size-max-min.yaml");
        KotlinFileAssert.assertThat(files.get("TestApiController.kt"))
                .assertClass("TestApiController")
                .assertMethod("lengthTest")
                .assertParameter("word")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "max", "10",
                        "message", "\"Must be max 10 characters\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "min", "1",
                        "message", "\"Must not be empty\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientId")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "min", "3",
                        "max", "5",
                        "message", "\"Must be between 3 and 5 characters\""
                ));
        KotlinFileAssert.assertThat(files.get("LengthTest.kt"))
                .assertClass("LengthTest")
                .assertPrimaryConstructorParameter("field1")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "max", "10",
                        "message", "\"Must be max 10 characters\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field2")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "min", "1",
                        "message", "\"Must not be empty\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field3")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "min", "3",
                        "max", "5",
                        "message", "\"Must be between 3 and 5 characters\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field4")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field5")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field6")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"));
    }

    @Test
    public void testXSizeMessage_size() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/error-message-for-size-max-min.yaml");
        KotlinFileAssert.assertThat(files.get("TestApiController.kt"))
                .assertClass("TestApiController")
                .assertMethod("sizeTest")
                .assertParameter("values")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "max", "10",
                        "message", "\"Must be max 10 elements\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("tokens")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "min", "1",
                        "message", "\"Must not be empty\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientIds")
                .assertParameterAnnotation("Size")
                .hasAttributes(ImmutableMap.of(
                        "min", "3",
                        "max", "5",
                        "message", "\"Must be between 3 and 5 elements\""
                ));
        KotlinFileAssert.assertThat(files.get("SizeTest.kt"))
                .assertClass("SizeTest")
                .assertPrimaryConstructorParameter("field1")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "max", "10",
                        "message", "\"Must be max 10 elements\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field2")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "min", "1",
                        "message", "\"Must not be empty\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field3")
                .assertParameterAnnotation("Size", "get")
                .hasAttributes(ImmutableMap.of(
                        "min", "3",
                        "max", "5",
                        "message", "\"Must be between 3 and 5 elements\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field4")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field5")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field6")
                .assertParameterAnnotation("Size", "get")
                .hasNotAttributes(List.of("message"));
    }

    @Test
    public void testXMinimumMessageAndXMaximumMessage_decimal() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/error-message-for-size-max-min.yaml");
        KotlinFileAssert.assertThat(files.get("TestApiController.kt"))
                .assertClass("TestApiController")
                .assertMethod("minmaxNumberTest")
                .assertParameter("number")
                .assertParameterAnnotation("DecimalMin")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"0.1\"",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("DecimalMax")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"99.9\"",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotation("DecimalMin")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"0.1\"",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("DecimalMax")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"99.9\"",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientNumber")
                .assertParameterAnnotation("DecimalMin")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"0.1\"",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("DecimalMax")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"99.9\"",
                        "message", "\"Must be less than 100\""
                ));
        KotlinFileAssert.assertThat(files.get("NumberTest.kt"))
                .assertClass("NumberTest")
                .assertPrimaryConstructorParameter("field1")
                .assertParameterAnnotation("DecimalMin", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"0.1\"",
                        "message", "\"Must be positive\""
                ))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("DecimalMax", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "\"99.9\"",
                        "message", "\"Must be less than 100\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field2")
                .assertParameterAnnotation("DecimalMin", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("DecimalMax", "get")
                .hasNotAttributes(List.of("message"));
    }

    @Test
    public void testXMinimumMessageAndXMaximumMessage_integer() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/error-message-for-size-max-min.yaml");
        KotlinFileAssert.assertThat(files.get("TestApiController.kt"))
                .assertClass("TestApiController")
                .assertMethod("minmaxIntegerTest")
                .assertParameter("number")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientNumber")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99",
                        "message", "\"Must be less than 100\""
                ));
        KotlinFileAssert.assertThat(files.get("IntegerTest.kt"))
                .assertClass("IntegerTest")
                .assertPrimaryConstructorParameter("field1")
                .assertParameterAnnotation("Min", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "1",
                        "message", "\"Must be positive\""
                ))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("Max", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "99",
                        "message", "\"Must be less than 100\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field2")
                .assertParameterAnnotation("Min", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("Max", "get")
                .hasNotAttributes(List.of("message"));
    }

    @Test
    public void testXMinimumMessageAndXMaximumMessage_long() throws IOException {
        final Map<String, File> files = generateFromContract("src/test/resources/3_0/error-message-for-size-max-min.yaml");
        KotlinFileAssert.assertThat(files.get("TestApiController.kt"))
                .assertClass("TestApiController")
                .assertMethod("minmaxLongTest")
                .assertParameter("number")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1L",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99L",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("token")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1L",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99L",
                        "message", "\"Must be less than 100\""
                ))
                .toParameter()
                .toMethod()
                .assertParameter("clientNumber")
                .assertParameterAnnotation("Min")
                .hasAttributes(ImmutableMap.of(
                        "value", "1L",
                        "message", "\"Must be positive\""
                ))
                .toParameter()
                .assertParameterAnnotation("Max")
                .hasAttributes(ImmutableMap.of(
                        "value", "99L",
                        "message", "\"Must be less than 100\""
                ));
        KotlinFileAssert.assertThat(files.get("LongTest.kt"))
                .assertClass("LongTest")
                .assertPrimaryConstructorParameter("field1")
                .assertParameterAnnotation("Min", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "1L",
                        "message", "\"Must be positive\""
                ))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("Max", "get")
                .hasAttributes(ImmutableMap.of(
                        "value", "99L",
                        "message", "\"Must be less than 100\""
                ))
                .toPrimaryConstructorParameter()
                .toClass()
                .assertPrimaryConstructorParameter("field2")
                .assertParameterAnnotation("Min", "get")
                .hasNotAttributes(List.of("message"))
                .toPrimaryConstructorParameter()
                .assertParameterAnnotation("Max", "get")
                .hasNotAttributes(List.of("message"));
    }

    @Test
    public void springPaginatedWithSpringDoc() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
        assertFileContains(petApi.toPath(), "@Parameter(hidden = true) pageable: Pageable");
    }

    @Test
    public void springPaginatedWithSpringDocAndSpringBoot3() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(USE_SPRING_BOOT3, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
    }

    @Test
    public void springPaginatedWithSpringFox() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springfox");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "import springfox.documentation.annotations.ApiIgnore");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
        assertFileContains(petApi.toPath(), "@ApiParam(hidden = true) pageable: Pageable");
    }

    @Test
    public void springPaginatedQueryParamsRemoved() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that page, size, and sort query params are removed but other params remain
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "tags: kotlin.collections.List<kotlin.String>");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
        assertFileNotContains(petApi.toPath(), "page:");
        assertFileNotContains(petApi.toPath(), "sort:");
        // Header param size should remain, query param size should be removed
        assertFileContains(petApi.toPath(), "@RequestHeader(value = \"size\"");
    }

    @Test
    public void springPaginatedWithReactive() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(REACTIVE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that pageable works in reactive mode
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
    }

    @Test
    public void springPaginatedWithIncludeHttpRequestContext() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(INCLUDE_HTTP_REQUEST_CONTEXT, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that pageable comes after request parameter
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "request: javax.servlet.http.HttpServletRequest");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
    }

    @Test
    public void springPaginatedWithReactiveAndIncludeHttpRequestContext() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(REACTIVE, "true");
        additionalProperties.put(INCLUDE_HTTP_REQUEST_CONTEXT, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that pageable comes after exchange parameter in reactive mode
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "exchange: org.springframework.web.server.ServerWebExchange");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
    }

    @Test
    public void springPaginatedWithDelegate() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(DELEGATE_PATTERN, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that pageable is in delegate interface
        File petApiDelegate = files.get("PetApiDelegate.kt");
        assertFileContains(petApiDelegate.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApiDelegate.toPath(), "pageable: Pageable");
    }

    @Test
    public void customPageableSchemaNotOverridden_issue13052() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/bugs/issue_13052.yaml", additionalProperties);

        // Custom Pageable model should be used instead of Spring's Pageable
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.openapitools.model.Pageable");
        assertFileNotContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileNotContains(petApi.toPath(), "import org.springdoc.core.annotations.ParameterObject");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
    }

    @Test
    public void springPaginatedWithNoDocumentationProvider() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "none");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Pageable should be added but no annotation imports
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
        assertFileNotContains(petApi.toPath(), "import springfox.documentation.annotations.ApiIgnore");
        assertFileNotContains(petApi.toPath(), "import org.springdoc.api.annotations.ParameterObject");
        assertFileNotContains(petApi.toPath(), "@ApiIgnore pageable");
        assertFileNotContains(petApi.toPath(), "@ParameterObject pageable");
    }

    @Test
    public void springPaginatedWithSwagger1AnnotationLibrary() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springfox");
        additionalProperties.put(ANNOTATION_LIBRARY, "swagger1");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test with swagger1 annotations
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");
        assertFileContains(petApi.toPath(), "import springfox.documentation.annotations.ApiIgnore");
        assertFileContains(petApi.toPath(), "@ApiParam(hidden = true) pageable: Pageable");
    }

    @Test
    public void springPaginatedWithSpringDocUsesPageableAsQueryParam() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Verify @PageableAsQueryParam annotation is present at method level
        assertFileContains(petApi.toPath(), "import org.springdoc.core.converters.models.PageableAsQueryParam");
        assertFileContains(petApi.toPath(), "@PageableAsQueryParam");

        // Verify Pageable parameter has @Parameter(hidden = true)
        assertFileContains(petApi.toPath(), "@Parameter(hidden = true) pageable: Pageable");

        // Verify the annotation appears before @RequestMapping for findPetsByStatus
        int findPetsByStatusStart = content.indexOf("fun findPetsByStatus(");
        Assert.assertTrue(findPetsByStatusStart > 0, "findPetsByStatus method should exist");

        String methodBlock = content.substring(Math.max(0, findPetsByStatusStart - 1000), findPetsByStatusStart);
        int pageableAsQueryParamPos = methodBlock.lastIndexOf("@PageableAsQueryParam");
        int requestMappingPos = methodBlock.lastIndexOf("@RequestMapping");

        Assert.assertTrue(pageableAsQueryParamPos > 0, "@PageableAsQueryParam should be present before method");
        Assert.assertTrue(requestMappingPos > pageableAsQueryParamPos,
            "@PageableAsQueryParam should appear before @RequestMapping");

        // Verify page, size, sort parameters are NOT in the method signature
        String methodSignature = content.substring(findPetsByStatusStart,
            content.indexOf("): ResponseEntity", findPetsByStatusStart));
        Assert.assertFalse(methodSignature.contains("page:"),
            "page parameter should be removed from method signature");
        Assert.assertFalse(methodSignature.contains("size:") && methodSignature.contains("@RequestParam"),
            "size query parameter should be removed from method signature");
        Assert.assertFalse(methodSignature.contains("sort:"),
            "sort parameter should be removed from method signature");
    }

    @Test
    public void springPaginatedNoParamsNoContext() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test operation listAllPets which has no parameters except pageable
        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "fun listAllPets(@Parameter(hidden = true) pageable: Pageable)");
    }

    @Test
    public void springPaginatedWithSpringDocPrependsToExistingAnnotation() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Verify that both annotations are imported
        assertFileContains(petApi.toPath(), "import org.springdoc.core.converters.models.PageableAsQueryParam");
        assertFileContains(petApi.toPath(), "import org.springframework.validation.annotation.Validated");

        // Find the listAllPets method
        int listAllPetsStart = content.indexOf("fun listAllPets(");
        Assert.assertTrue(listAllPetsStart > 0, "listAllPets method should exist");

        // Check the annotations appear before the method in the correct order
        String methodBlock = content.substring(Math.max(0, listAllPetsStart - 1000), listAllPetsStart);

        int pageableAsQueryParamPos = methodBlock.lastIndexOf("@PageableAsQueryParam");
        int validatedPos = methodBlock.lastIndexOf("@org.springframework.validation.annotation.Validated");
        int requestMappingPos = methodBlock.lastIndexOf("@RequestMapping");

        Assert.assertTrue(pageableAsQueryParamPos > 0, "@PageableAsQueryParam should be present before listAllPets method");
        Assert.assertTrue(validatedPos > 0, "@Validated should be present before listAllPets method");

        // Verify @PageableAsQueryParam comes before @Validated (prepended)
        Assert.assertTrue(pageableAsQueryParamPos < validatedPos,
            "@PageableAsQueryParam should be prepended (appear before) existing @Validated annotation");

        // Verify both annotations come before @RequestMapping
        Assert.assertTrue(validatedPos < requestMappingPos,
            "Both annotations should appear before @RequestMapping");

        // Verify the Pageable parameter still has @Parameter(hidden = true)
        assertFileContains(petApi.toPath(), "@Parameter(hidden = true) pageable: Pageable");
    }

    @Test
    public void springPaginatedWithSpringDocPrependsToExistingAnnotationArray() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Verify that PageableAsQueryParam is imported
        assertFileContains(petApi.toPath(), "import org.springdoc.core.converters.models.PageableAsQueryParam");

        // Find the findPetsByStatus method
        int findPetsByStatusStart = content.indexOf("fun findPetsByStatus(");
        Assert.assertTrue(findPetsByStatusStart > 0, "findPetsByStatus method should exist");

        // Check the annotations appear before the method in the correct order
        String methodBlock = content.substring(Math.max(0, findPetsByStatusStart - 1500), findPetsByStatusStart);

        int pageableAsQueryParamPos = methodBlock.lastIndexOf("@PageableAsQueryParam");
        int validatedPos = methodBlock.lastIndexOf("@org.springframework.validation.annotation.Validated");
        int preAuthorizePos = methodBlock.lastIndexOf("@org.springframework.security.access.prepost.PreAuthorize");
        int requestMappingPos = methodBlock.lastIndexOf("@RequestMapping");

        Assert.assertTrue(pageableAsQueryParamPos > 0, "@PageableAsQueryParam should be present before findPetsByStatus method");
        Assert.assertTrue(validatedPos > 0, "@Validated should be present before findPetsByStatus method");
        Assert.assertTrue(preAuthorizePos > 0, "@PreAuthorize should be present before findPetsByStatus method");

        // Verify @PageableAsQueryParam comes first (prepended to the array)
        Assert.assertTrue(pageableAsQueryParamPos < validatedPos,
            "@PageableAsQueryParam should be prepended (appear before) @Validated annotation");

        // Verify the original array order is preserved after @PageableAsQueryParam
        Assert.assertTrue(validatedPos < preAuthorizePos,
            "@Validated should appear before @PreAuthorize (original array order preserved)");

        // Verify all annotations come before @RequestMapping
        Assert.assertTrue(preAuthorizePos < requestMappingPos,
            "All annotations should appear before @RequestMapping");

        // Verify the Pageable parameter still has @Parameter(hidden = true)
        assertFileContains(petApi.toPath(), "@Parameter(hidden = true) pageable: Pageable");
    }

    @Test
    public void springPaginatedMixedOperations() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        
        // Operation with x-spring-paginated should have Pageable
        assertFileContains(petApi.toPath(), "fun findPetsByStatus(");
        assertFileContains(petApi.toPath(), "pageable: Pageable");
        
        // Operation without x-spring-paginated should NOT have Pageable
        assertFileContains(petApi.toPath(), "fun addPet(");
        // Verify addPet doesn't have pageable (it has body param only)
        String content = Files.readString(petApi.toPath());
        String addPetMethod = content.substring(
            content.indexOf("fun addPet("),
            content.indexOf(")", content.indexOf("fun addPet(")) + 1
        );
        Assert.assertFalse(addPetMethod.contains("pageable"), 
            "addPet should not have pageable parameter");
    }

    @Test
    public void springPaginatedWithServiceInterface() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(SERVICE_INTERFACE, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Test that pageable is in service interface
        File petService = files.get("PetService.kt");
        if (petService != null) {
            assertFileContains(petService.toPath(), "import org.springframework.data.domain.Pageable");
            assertFileContains(petService.toPath(), "pageable: Pageable");
        }
    }

    @Test
    public void springPaginatedParameterOrdering() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(INCLUDE_HTTP_REQUEST_CONTEXT, "true");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Verify exact parameter ordering: allParams -> request -> pageable
        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Find findPetsByStatus method
        int methodStart = content.indexOf("fun findPetsByStatus(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);
        
        // Verify order: status param comes before request, request comes before pageable
        int statusPos = methodSignature.indexOf("status:");
        int requestPos = methodSignature.indexOf("request:");
        int pageablePos = methodSignature.indexOf("pageable:");
        
        Assert.assertTrue(statusPos > 0, "status parameter should exist");
        Assert.assertTrue(requestPos > statusPos, "request should come after status");
        Assert.assertTrue(pageablePos > requestPos, "pageable should come after request");
    }

    @Test
    public void springPaginatedDelegateCallPassesPageable() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(DELEGATE_PATTERN, "true");
        additionalProperties.put(INTERFACE_ONLY, "false");
        
        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml", additionalProperties);

        // Verify that interface method calls delegate with pageable parameter
        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Check for delegate call pattern with pageable
        if (content.contains("getDelegate().findPetsByStatus")) {
            assertFileContains(petApi.toPath(), "getDelegate().findPetsByStatus(");
            assertFileContains(petApi.toPath(), "pageable)");
        }
    }

    private Map<String, File> generateFromContract(String url) throws IOException {
        return generateFromContract(url, new HashMap<>(), new HashMap<>());
    }

    private Map<String, File> generateFromContract(String url, Map<String, Object> additionalProperties) throws IOException {
        return generateFromContract(url, additionalProperties, new HashMap<>());
    }

    private Map<String, File> generateFromContract(
            String url,
            Map<String, Object> additionalProperties,
            Map<String, String> generatorPropertyDefaults
    ) throws IOException {
        return generateFromContract(url, additionalProperties, generatorPropertyDefaults, codegen -> {
        });
    }

    /**
     * Generate the contract with additional configuration.
     * <p>
     * use CodegenConfigurator instead of CodegenConfig for easier configuration like in JavaClientCodeGenTest
     */
    private Map<String, File> generateFromContract(
            String url,
            Map<String, Object> additionalProperties,
            Map<String, String> generatorPropertyDefaults,
            Consumer<CodegenConfigurator> consumer
    ) throws IOException {

        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin-spring")
                .setAdditionalProperties(additionalProperties)
                .setValidateSpec(false)
                .setInputSpec(url)
                .setLibrary(SPRING_BOOT)
                .setOutputDir(output.getAbsolutePath());

        consumer.accept(configurator);

        ClientOptInput input = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generatorPropertyDefaults.forEach(generator::setGeneratorPropertyDefault);

        return generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));
    }

    // ========== AUTO X-SPRING-PAGINATED TESTS ==========

    @Test
    public void autoXSpringPaginatedDetectsAllThreeParams() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation with all three params (page, size, sort) should have Pageable auto-detected
        assertFileContains(petApi.toPath(), "fun findPetsWithAutoDetect(");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Pageable");

        // Extract findPetsWithAutoDetect method
        int methodStart = content.indexOf("fun findPetsWithAutoDetect(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        // Should have pageable parameter
        Assert.assertTrue(methodSignature.contains("pageable: Pageable"),
            "findPetsWithAutoDetect should have pageable parameter when autoXSpringPaginated is enabled");

        // Should NOT have page, size, sort query params (they should be removed)
        Assert.assertFalse(methodSignature.contains("page:"),
            "page query param should be removed when pageable is added");
        Assert.assertFalse(methodSignature.contains("size:"),
            "size query param should be removed when pageable is added");
        Assert.assertFalse(methodSignature.contains("sort:"),
            "sort query param should be removed when pageable is added");

        // Should still have the status parameter
        Assert.assertTrue(methodSignature.contains("status:"),
            "status parameter should remain");
    }

    @Test
    public void autoXSpringPaginatedNoDetectionWhenMissingPage() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation missing 'page' param should NOT have Pageable
        int methodStart = content.indexOf("fun findPetsMissingPage(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsMissingPage should NOT have pageable when 'page' param is missing");

        // Should still have the other params
        Assert.assertTrue(methodSignature.contains("size:"),
            "size param should remain");
        Assert.assertTrue(methodSignature.contains("sort:"),
            "sort param should remain");
    }

    @Test
    public void autoXSpringPaginatedNoDetectionWhenMissingSize() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation missing 'size' param should NOT have Pageable
        int methodStart = content.indexOf("fun findPetsMissingSize(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsMissingSize should NOT have pageable when 'size' param is missing");

        // Should still have the other params
        Assert.assertTrue(methodSignature.contains("page:"),
            "page param should remain");
        Assert.assertTrue(methodSignature.contains("sort:"),
            "sort param should remain");
    }

    @Test
    public void autoXSpringPaginatedNoDetectionWhenMissingSort() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation missing 'sort' param should NOT have Pageable
        int methodStart = content.indexOf("fun findPetsMissingSort(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsMissingSort should NOT have pageable when 'sort' param is missing");

        // Should still have the other params
        Assert.assertTrue(methodSignature.contains("page:"),
            "page param should remain");
        Assert.assertTrue(methodSignature.contains("size:"),
            "size param should remain");
    }

    @Test
    public void autoXSpringPaginatedManualFalseTakesPrecedence() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation with x-spring-paginated: false should NOT have Pageable (manual override)
        int methodStart = content.indexOf("fun findPetsManualFalse(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsManualFalse should NOT have pageable when x-spring-paginated is explicitly set to false");

        // Should still have all three params
        Assert.assertTrue(methodSignature.contains("page:"),
            "page param should remain when x-spring-paginated: false");
        Assert.assertTrue(methodSignature.contains("size:"),
            "size param should remain when x-spring-paginated: false");
        Assert.assertTrue(methodSignature.contains("sort:"),
            "sort param should remain when x-spring-paginated: false");
    }

    @Test
    public void autoXSpringPaginatedCaseSensitiveMatching() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation with Page, Size, Sort (capitalized) should NOT match
        int methodStart = content.indexOf("fun findPetsCaseSensitive(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsCaseSensitive should NOT have pageable with capitalized param names (case-sensitive)");

        // Should still have all three params with capital letters
        Assert.assertTrue(methodSignature.contains("page:") || methodSignature.contains("Page:"),
            "Page param should remain");
    }

    @Test
    public void autoXSpringPaginatedOnlyForSpringBoot() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        // Test with spring-cloud library (should NOT auto-detect)
        Map<String, File> files = generateFromContract(
            "src/test/resources/3_0/spring/petstore-auto-paginated.yaml",
            additionalProperties,
            new HashMap<>(),
            configurator -> configurator.setLibrary("spring-cloud")
        );

        File petApi = files.get("PetApiClient.kt");
        if (petApi != null) {
            String content = Files.readString(petApi.toPath());

            // For spring-cloud, should NOT have Pageable even with auto-detect enabled
            Assert.assertFalse(content.contains("pageable: Pageable"),
                "spring-cloud library should NOT auto-detect pageable (needs actual query params for HTTP)");

            // Should have all three query params
            int methodStart = content.indexOf("fun findPetsWithAutoDetect(");
            if (methodStart >= 0) {
                int methodEnd = content.indexOf("): ", methodStart);
                String methodSignature = content.substring(methodStart, methodEnd);

                Assert.assertTrue(methodSignature.contains("page") || methodSignature.contains("@Query"),
                    "spring-cloud should keep query parameters");
            }
        }
    }

    @Test
    public void autoXSpringPaginatedDisabledByDefault() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        // NOT setting AUTO_X_SPRING_PAGINATED (should default to false)

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Without AUTO_X_SPRING_PAGINATED, should NOT auto-detect
        int methodStart = content.indexOf("fun findPetsWithAutoDetect(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "Should NOT have pageable when autoXSpringPaginated is not enabled (default: false)");

        // Should have all three query params
        Assert.assertTrue(methodSignature.contains("page:"),
            "page param should remain when auto-detect is disabled");
        Assert.assertTrue(methodSignature.contains("size:"),
            "size param should remain when auto-detect is disabled");
        Assert.assertTrue(methodSignature.contains("sort:"),
            "sort param should remain when auto-detect is disabled");
    }

    @Test
    public void autoXSpringPaginatedWorksWithManualTrue() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation with manual x-spring-paginated: true should still work
        int methodStart = content.indexOf("fun findPetsManualTrue(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertTrue(methodSignature.contains("pageable: Pageable"),
            "findPetsManualTrue should have pageable (manual x-spring-paginated: true)");

        // Query params should be removed
        Assert.assertFalse(methodSignature.contains("page:"),
            "page param should be removed");
        Assert.assertFalse(methodSignature.contains("size:"),
            "size param should be removed");
        Assert.assertFalse(methodSignature.contains("sort:"),
            "sort param should be removed");
    }

    @Test
    public void autoXSpringPaginatedNoParamsDoesNotDetect() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-auto-paginated.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // Operation with no params should NOT have Pageable
        int methodStart = content.indexOf("fun findPetsNoParams(");
        int methodEnd = content.indexOf("): ResponseEntity", methodStart);
        String methodSignature = content.substring(methodStart, methodEnd);

        Assert.assertFalse(methodSignature.contains("pageable: Pageable"),
            "findPetsNoParams should NOT have pageable when there are no pagination params");
    }

    @Test
    public void testSealedResponseInterfaces() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/sealed-response-interfaces.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.openapitools.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(USE_SEALED_RESPONSE_INTERFACES, "true");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        // Verify sealed interfaces are declared in the model package
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/SealedResponseInterfaces.kt"),
                "sealed interface CreateUserResponse",
                "sealed interface GetUserResponse");

        // Verify API file imports the sealed interfaces
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/UsersApi.kt"),
                "import org.openapitools.model.CreateUserResponse",
                "import org.openapitools.model.GetUserResponse");

        // Verify API methods use sealed interfaces as return types
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/UsersApi.kt"),
                "fun createUser(",
                "): ResponseEntity<CreateUserResponse>",
                "fun getUser(",
                "): ResponseEntity<GetUserResponse>");

        // Verify User model implements both sealed interfaces
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/User.kt"),
                "data class User(",
                ": CreateUserResponse, GetUserResponse");

        // Verify ConflictResponse implements CreateUserResponse
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/ConflictResponse.kt"),
                "data class ConflictResponse(",
                ": CreateUserResponse");

        // Verify ErrorResponse implements both sealed interfaces
        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/ErrorResponse.kt"),
                "data class ErrorResponse(",
                ": CreateUserResponse, GetUserResponse");
    }

    @Test
    public void testSealedResponseInterfacesDisabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/sealed-response-interfaces.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.openapitools.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(USE_SEALED_RESPONSE_INTERFACES, "false");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        // Verify sealed interfaces file is NOT generated when feature is disabled
        File sealedInterfacesFile = new File(outputPath + "/src/main/kotlin/org/openapitools/model/SealedResponseInterfaces.kt");
        Assert.assertFalse(sealedInterfacesFile.exists(), "SealedResponseInterfaces.kt should not exist when feature is disabled");

        // Verify models do NOT implement sealed interfaces when disabled
        assertFileNotContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/User.kt"),
                ": CreateUserResponse",
                ": GetUserResponse");

        assertFileNotContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/ConflictResponse.kt"),
                ": CreateUserResponse");

        assertFileNotContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/ErrorResponse.kt"),
                ": CreateUserResponse",
                ": GetUserResponse");
    }

    @Test
    public void testSealedResponseInterfacesNoDuplicates() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/sealed-response-interfaces-duplicates.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.openapitools.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(USE_SEALED_RESPONSE_INTERFACES, "true");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        // Verify Order model does NOT have duplicate sealed interface implementations
        Path orderFile = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Order.kt");
        String orderContent = Files.readString(orderFile);

        // Count occurrences of "PlaceOrderResponse" in the class declaration line
        // Should appear exactly once, not multiple times
        String classDeclaration = orderContent.lines()
                .filter(line -> line.contains("data class Order") || line.contains(") : "))
                .collect(Collectors.joining("\n"));

        // Count how many times PlaceOrderResponse appears
        long placeOrderCount = classDeclaration.chars()
                .filter(c -> c == ',')
                .count() + 1; // Number of interfaces = number of commas + 1

        // Should have PlaceOrderResponse and GetOrderByIdResponse, not duplicates
        Assert.assertTrue(classDeclaration.contains("PlaceOrderResponse"),
                "Order should implement PlaceOrderResponse");
        Assert.assertTrue(classDeclaration.contains("GetOrderByIdResponse"),
                "Order should implement GetOrderByIdResponse");

        // Check for duplicate imports
        long importCount = orderContent.lines()
                .filter(line -> line.contains("import org.openapitools.model.PlaceOrderResponse"))
                .count();
        Assert.assertEquals(importCount, 1L, "PlaceOrderResponse should be imported exactly once, not " + importCount);

        // Verify no duplicate interface implementations
        // The pattern should be ") : InterfaceA, InterfaceB {" not ") : InterfaceA, InterfaceA, InterfaceB, InterfaceB {"
        Assert.assertFalse(classDeclaration.matches(".*PlaceOrderResponse.*,\\s*PlaceOrderResponse.*"),
                "PlaceOrderResponse should not appear twice in implements list");
        Assert.assertFalse(classDeclaration.matches(".*GetOrderByIdResponse.*,\\s*GetOrderByIdResponse.*"),
                "GetOrderByIdResponse should not appear twice in implements list");
    }

    @Test
    public void testSealedResponseInterfacesVoidResponse() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/sealed-response-interfaces-void-response.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.openapitools.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(USE_SEALED_RESPONSE_INTERFACES, "true");
        codegen.additionalProperties().put(INTERFACE_ONLY, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        // Read generated SealedResponseInterfaces.kt
        Path sealedInterfacesPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/SealedResponseInterfaces.kt");
        String sealedInterfacesContent = new String(Files.readAllBytes(sealedInterfacesPath), StandardCharsets.UTF_8);

        // CreateUserResponse should NOT be generated (void response operation)
        Assert.assertFalse(sealedInterfacesContent.contains("sealed interface CreateUserResponse"),
                "CreateUserResponse should not be generated for operations with no response content");

        // CreatePetResponse should be generated (has response content)
        Assert.assertTrue(sealedInterfacesContent.contains("sealed interface CreatePetResponse"),
                "CreatePetResponse should be generated for operations with response content");

        // Read generated Pet.kt
        Path petPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt");
        String petContent = new String(Files.readAllBytes(petPath), StandardCharsets.UTF_8);

        // Pet should implement CreatePetResponse
        Assert.assertTrue(petContent.contains("import org.openapitools.model.CreatePetResponse"),
                "Pet should import CreatePetResponse");
        Assert.assertTrue(petContent.contains(") : CreatePetResponse {") || petContent.contains(") : CreatePetResponse"),
                "Pet should implement CreatePetResponse");
    }
}


