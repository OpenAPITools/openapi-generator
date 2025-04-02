package org.openapitools.codegen.kotlin.spring;

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
import org.openapitools.codegen.kotlin.KotlinTestUtils;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DocumentationProvider;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
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
        assertFileContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                "@RequestMapping(\"\\${"
        );
    }

    @Test
    public void testNoRequestMappingAnnotationApiInterface() throws IOException {
        File output = generatePetstoreWithRequestMappingMode(KotlinSpringServerCodegen.RequestMappingMode.api_interface);

        // Check that the @RequestMapping annotation is generated in the Api file
        assertFileContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/api/PetApi.kt"),
                "@RequestMapping(\"\\${"
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
                "@Min(2)"
        );
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

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt"),
                "import java.io.Serializable",
                ") : Serializable{",
                "private const val serialVersionUID: kotlin.Long = 1"
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
}
