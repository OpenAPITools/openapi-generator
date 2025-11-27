package org.openapitools.codegen.kotlin.spring;

import com.google.common.collect.ImmutableMap;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.io.FileUtils;
import org.assertj.core.api.Assertions;
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
    public void testNoRequestMappingAnnotation_spring_cloud_default() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/feat13488_use_kotlinSpring_with_springCloud.yaml",
                Map.of(CodegenConstants.LIBRARY, "spring-cloud"),
                Map.of());
        assertGeneratedFilesNotContain(
                Map.of( // Check that the @RequestMapping annotation is not generated in the Api file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("@RequestMapping(\"\\${api.base-path")
                )
        );
    }

    @Test
    public void testNoRequestMappingAnnotationNone() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                DELEGATE_PATTERN, true,
                USE_TAGS, true,
                REQUEST_MAPPING_OPTION, "none"
        ), Map.of());
        assertGeneratedFilesNotContain(
                Map.of( // Check that the @RequestMapping annotation is not generated in the Api file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("@RequestMapping(\"\\${"),
                        // Check that the @RequestMapping annotation is not generated in the ApiController file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of("@RequestMapping(\"\\${")
                )
        );
    }

    @Test
    public void testNoRequestMappingAnnotationController() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                DELEGATE_PATTERN, true,
                USE_TAGS, true,
                REQUEST_MAPPING_OPTION, "controller"
        ), Map.of());
        assertGeneratedFilesNotContain(
                Map.of( // Check that the @RequestMapping annotation is not generated in the Api file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("@RequestMapping(\"\\${")
                )
        );
        assertGeneratedFilesContain(
                Map.of( // Check that the @RequestMapping annotation is generated in the ApiController file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of("@RequestMapping(\"\\${openapi.openAPIPetstore.base-path:\\${api.base-path:$BASE_PATH}}\")",
                                "    companion object {\n"
                                + "    //for your own safety never directly reuse these path definitions in tests\n"
                                + "        const val BASE_PATH: String = \"/v2\"\n"
                                + "    }")
                )
        );
    }

    @Test
    public void testNoRequestMappingAnnotationApiInterface() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                DELEGATE_PATTERN, true,
                USE_TAGS, true,
                REQUEST_MAPPING_OPTION, "api_interface"
        ), Map.of());

        assertGeneratedFilesContain(
                Map.of(
                        // Check that the @RequestMapping annotation is generated in the Api file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "@RequestMapping(\"\\${openapi.openAPIPetstore.base-path:\\${api.base-path:$BASE_PATH}}\")",
                                "    companion object {\n"
                                + "        //for your own safety never directly reuse these path definitions in tests\n"
                                + "        const val BASE_PATH: String = \"/v2\"")
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(
                        // Check that the @RequestMapping annotation is not generated in the ApiController file
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "@RequestMapping(\"\\${")
                )
        );
    }

    @Test
    public void testSettersForConfigValues() {
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
    public void testAdditionalPropertiesPutForConfigValues() {
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
    public void delegateWithTags() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/issue5497-use-tags-kotlin.yaml", Map.of(
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true,
                KotlinSpringServerCodegen.USE_TAGS, true
        ), Map.of());

        assertGeneratedFilesExist(List.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiController.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt")
        ));
    }

    @Test(description = "test delegate reactive with tags")
    public void delegateReactiveWithTags() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/issue7325-use-delegate-reactive-tags-kotlin.yaml", Map.of(
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true,
                KotlinSpringServerCodegen.REACTIVE, true,
                KotlinSpringServerCodegen.USE_TAGS, true
        ), Map.of());

        assertGeneratedFilesExist(List.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2Api.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiController.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3Api.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3ApiController.kt"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt")
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("suspend fun"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("suspend fun"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2Api.kt"), List.of("import kotlinx.coroutines.flow.Flow", "ResponseEntity<Flow<kotlin.String>>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"), List.of("import kotlinx.coroutines.flow.Flow", "ResponseEntity<Flow<kotlin.String>>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3Api.kt"), List.of("import kotlinx.coroutines.flow.Flow", "requestBody: Flow<kotlin.Long>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt"), List.of("import kotlinx.coroutines.flow.Flow", "suspend fun", "requestBody: Flow<kotlin.Long>"
                        )
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("exchange"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("ApiUtil"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2Api.kt"), List.of("exchange"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"), List.of("suspend fun", "ApiUtil"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3Api.kt"), List.of("exchange"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV3ApiDelegate.kt"), List.of("ApiUtil"
                        )
                )
        );
    }

    @Test
    public void testNullableMultipartFile() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/feat-multipartfile_nullable.yaml",
                Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/NullableMultipartfileApiController.kt"), List.of(
                                "file: org.springframework.web.multipart.MultipartFile?"
                                + "    )"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/NullableMultipartfileArrayApiController.kt"), List.of(
                                "files: Array<org.springframework.web.multipart.MultipartFile>?"
                                + "    )"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/NonNullableMultipartfileApiController.kt"), List.of(
                                "file: org.springframework.web.multipart.MultipartFile"
                                + "    )"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/NonNullableMultipartfileArrayApiController.kt"), List.of(
                                "files: Array<org.springframework.web.multipart.MultipartFile>"
                                + "    )"
                        )
                )
        );
    }

    @Test
    public void arrayItemsCanBeNullable() {
        Path apiSources = generateApiSources("src/test/resources/3_0/array-nullable-items.yaml",
                Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"),
                Map.of(
                        CodegenConstants.MODELS, "true",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                ));
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/model/ArrayWithNullableItemsModel.kt"), List.of("List<kotlin.String?>"))
        );
    }

    @Test
    public void doNotGenerateRequestParamForObjectQueryParam() {
        Path apiSources = generateApiSources("src/test/resources/3_0/objectQueryParam.yaml",
                Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                ));
        assertGeneratedFilesNotContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/api/PonyApiController.kt"), List.of("@RequestParam"))
        );
    }

    @Test
    public void doGenerateRequestParamForSimpleParam() {
        Path apiSources = generateApiSources("src/test/resources/3_0/issue_3248.yaml",
                Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/MonkeysApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/ElephantsApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/ZebrasApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/BearsApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/CamelsApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PandasApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/CrocodilesApiController.kt"), List.of("@RequestParam"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PolarBearsApiController.kt"), List.of("@RequestParam")
                )
        );
    }

    @Test
    public void generateFormatForDateAndDateTimeQueryParam() {
        Path apiSources = generateApiSources("src/test/resources/3_0/issue_2053.yaml",
                Map.of(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true"),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/ElephantsApiController.kt"),
                        List.of("@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/ZebrasApiController.kt"),
                        List.of("@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE_TIME)")
                )
        );
    }

    @Test(description = "test bean qualifiers")
    public void beanQualifiers() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/bean-qualifiers.yaml", Map.of(KotlinSpringServerCodegen.BEAN_QUALIFIERS, true), Map.of());
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/api/PingApiController.kt"), List.of("@RestController(\"org.openapitools.api.PingApiController\")"))
        );
    }

    @Test(description = "test skip default interface")
    public void skipDefaultInterface() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/skip-default-interface.yaml", Map.of(
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.SKIP_DEFAULT_INTERFACE, true
        ), Map.of());
        assertGeneratedFilesNotContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/api/PingApi.kt"), List.of("return "))
        );
    }

    @Test(description = "test cookie parameter generation on interface apis")
    public void cookieParameterGenerationApis() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-fake-endpoints-for-testing-with-cookie.yaml", Map.of(
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.SKIP_DEFAULT_INTERFACE, true
        ), Map.of());
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/api/FakeApi.kt"), List.of("@CookieValue")));
    }

    @Test(description = "test cookie parameter generation on controllers")
    public void cookieParameterGenerationControllers() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-fake-endpoints-for-testing-with-cookie.yaml", Map.of(), Map.of());
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/api/FakeApiController.kt"), List.of("@CookieValue")));
    }

    @Test(description = "use Spring boot 3 & jakarta extension")
    public void useSpringBoot3() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/feat13578_use_springboot3_jakarta_extension.yaml", Map.of(
                KotlinSpringServerCodegen.USE_SPRING_BOOT3, true
        ), Map.of());
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/ApiUtil.kt"), List.of("import jakarta.servlet.http.HttpServletResponse"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/Exceptions.kt"), List.of("import jakarta.validation.ConstraintViolationException"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PingApiController.kt"), List.of("import jakarta.validation.Valid")
                )
        );
    }

    @Test(description = "multi-line descriptions should be supported for operations")
    public void multiLineOperationDescription() {
        testMultiLineOperationDescription(false);
    }

    @Test(description = "multi-line descriptions should be supported for operations (interface-only)")
    public void multiLineOperationDescriptionInterfaceOnly() {
        testMultiLineOperationDescription(true);
    }

    private void testMultiLineOperationDescription(final boolean isInterfaceOnly) {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/issue4111-multiline-operation-description.yaml", Map.of(
                KotlinSpringServerCodegen.INTERFACE_ONLY, isInterfaceOnly
        ), Map.of());

        final String pingApiFileName;
        if (isInterfaceOnly) {
            pingApiFileName = "PingApi.kt";
        } else {
            pingApiFileName = "PingApiController.kt";
        }

        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/" + pingApiFileName),
                        List.of(
                                "description = \"\"\"# Multi-line descriptions\n"
                                + "\n"
                                + "This is an example of a multi-line description.\n"
                                + "\n"
                                + "It:\n"
                                + "- has multiple lines\n"
                                + "- uses Markdown (CommonMark) for rich text representation\"\"\"")
                )
        );
    }

    @Test(description = "use get Annotation use-site target on kotlin interface attributes")
    public void useTargetOnInterfaceAnnotations() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/issue3596-use-correct-get-annotation-target.yaml", Map.of(), Map.of());
        Path animalDto = apiSources.resolve("src/main/kotlin/org/openapitools/model/Animal.kt");
        assertGeneratedFilesContain(
                Map.of(
                        animalDto, List.of("@get:Schema(example = \"null\", description = \"\")", "@get:Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")")
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(
                        animalDto, List.of("@Schema(example = \"null\", description = \"\")", "@Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")")
                )
        );
    }

    @Test(description = "use get Annotation use-site target on kotlin interface attributes (swagger1)")
    public void useTargetOnInterfaceAnnotationsWithSwagger1() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/issue3596-use-correct-get-annotation-target.yaml", Map.of(
                ANNOTATION_LIBRARY, AnnotationLibrary.SWAGGER1.toCliOptValue(),
                DOCUMENTATION_PROVIDER, DocumentationProvider.SPRINGFOX.toCliOptValue()
        ), Map.of());
        Path animalDto = apiSources.resolve("src/main/kotlin/org/openapitools/model/Animal.kt");
        assertGeneratedFilesContain(
                Map.of(animalDto, List.of("@get:ApiModelProperty(example = \"null\", value = \"\")", "@get:ApiModelProperty(example = \"null\", required = true, value = \"\")"))
        );
        assertGeneratedFilesNotContain(
                Map.of(animalDto, List.of("@ApiModelProperty(example = \"null\", value = \"\")", "@ApiModelProperty(example = \"null\", required = true, value = \"\")"))
        );
    }

    @Test
    public void useBeanValidationGenerateAnnotationsForRequestBody() {
        Path apiSources = generateApiSources("src/test/resources/bugs/issue_13932.yml", Map.of(
                KotlinSpringServerCodegen.INTERFACE_ONLY, "true",
                KotlinSpringServerCodegen.USE_BEANVALIDATION, "true",
                CodegenConstants.MODEL_PACKAGE, "xyz.model",
                CodegenConstants.API_PACKAGE, "xyz.controller"
        ), Map.of());
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/xyz/controller/AddApi.kt"), List.of("@Min(value=2)"))
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
    public void givenMultipartFormArray_whenGenerateDelegateAndService_thenParameterIsCreatedAsListOfMultipartFile() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-tags.yaml", Map.of(
                KotlinSpringServerCodegen.SERVICE_INTERFACE, true,
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of("additionalMetadata: kotlin.String?", "images: Array<org.springframework.web.multipart.MultipartFile>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("images: Array<org.springframework.web.multipart.MultipartFile>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiService.kt"), List.of("images: Array<org.springframework.web.multipart.MultipartFile>")
                )
        );
    }

    @Test
    public void givenOctetStreamResponseType_whenGenerateServer_thenReturnTypeIsResource() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-tags.yaml", Map.of(), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of("): ResponseEntity<org.springframework.core.io.Resource>")
                )
        );
    }

    @Test
    public void givenMultipartForm_whenGenerateReactiveServer_thenParameterAreCreatedAsRequestPart() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-tags.yaml", Map.of(), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"),
                        List.of("@Parameter(description = \"Additional data to pass to server\") @Valid @RequestParam(value = \"additionalMetadata\", required = false) additionalMetadata: kotlin.String?",
                                "@Parameter(description = \"image to upload\") @Valid @RequestPart(\"image\", required = false) image: org.springframework.web.multipart.MultipartFile")
                )
        );
    }

    @Test
    public void overridePropertyFunction() {
        Path apiSources = generateApiSources("src/test/resources/bugs/issue_20228.yaml", Map.of(
                CodegenConstants.SERIALIZABLE_MODEL, true
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "false",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(apiSources.resolve("src/main/kotlin/org/openapitools/model/Pony.kt"), List.of("override val nameOpt", "override val nameReq")));
    }

    @Test
    public void generateSerializableModel() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.SERIALIZABLE_MODEL, true
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "false",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/model/Pet.kt"),
                        List.of("import java.io.Serializable",
                                ") : Serializable {",
                                "private const val serialVersionUID: kotlin.Long = 1")
                )
        );
    }

    @Test
    public void generateSerializableModelWithXimplements() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml", Map.of(
                CodegenConstants.SERIALIZABLE_MODEL, true
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "false",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/model/Dog.kt"),
                        List.of("import java.io.Serializable",
                                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                                ") : Pet, Serializable,  com.some.pack.Fetchable {",
                                "private const val serialVersionUID: kotlin.Long = 1")
                )
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithReactorResponseEntity() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, true,
                DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor",
                USE_RESPONSE_ENTITY, true,
                REQUEST_MAPPING_OPTION, "none",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path storeApiClient = apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt");
        assertGeneratedFilesContain(
                Map.of(
                        storeApiClient,
                        List.of(
                                "import reactor.core.publisher.Flux\n"
                                + "import reactor.core.publisher.Mono",
                                "    @HttpExchange(\n"
                                + "        url = PATH_GET_INVENTORY /* \"/store/inventory\" */,\n"
                                + "        method = \"GET\"\n"
                                + "    )\n"
                                + "    fun getInventory(\n"
                                + "    ): Mono<ResponseEntity<Map<String, kotlin.Int>>>",
                                "    @HttpExchange(\n"
                                + "        url = PATH_DELETE_ORDER /* \"/store/order/{orderId}\" */,\n"
                                + "        method = \"DELETE\"\n"
                                + "    )\n"
                                + "    fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): Mono<ResponseEntity<Unit>>",
                                "    @HttpExchange(\n"
                                + "        url = PATH_PLACE_ORDER /* \"/store/order\" */,\n"
                                + "        method = \"POST\"\n"
                                + "    )\n"
                                + "    fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): Mono<ResponseEntity<Order>>",
                                "    companion object {\n"
                                + "        //for your own safety never directly reuse these path definitions in tests\n"
                                + "        const val PATH_DELETE_ORDER: String = \"/store/order/{orderId}\"\n"
                                + "        const val PATH_GET_INVENTORY: String = \"/store/inventory\"\n"
                                + "        const val PATH_GET_ORDER_BY_ID: String = \"/store/order/{orderId}\"\n"
                                + "        const val PATH_PLACE_ORDER: String = \"/store/order\"\n"
                                + "    }")
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(storeApiClient, List.of("suspend")));
    }

    @Test
    public void generateHttpInterfaceReactiveWithCoroutinesResponseEntity() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, true,
                DECLARATIVE_INTERFACE_REACTIVE_MODE, "coroutines",
                USE_RESPONSE_ENTITY, true,
                REQUEST_MAPPING_OPTION, "none",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt"),
                        List.of("    suspend fun getInventory(\n"
                                + "    ): ResponseEntity<Map<String, kotlin.Int>>",
                                "    suspend fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): ResponseEntity<Unit>",
                                "    suspend fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): ResponseEntity<Order>")
                )
        );
    }

    @Test
    public void generateHttpInterfaceReactiveWithReactor() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, true,
                DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor",
                USE_RESPONSE_ENTITY, false,
                REQUEST_MAPPING_OPTION, "none",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path storeApiClient = apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt");
        assertGeneratedFilesContain(
                Map.of(
                        storeApiClient,
                        List.of("import reactor.core.publisher.Flux\n"
                                + "import reactor.core.publisher.Mono",
                                "    fun getInventory(\n"
                                + "    ): Mono<Map<String, kotlin.Int>>",
                                "    fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): Mono<Unit>",
                                "    fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): Mono<Order>")
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(storeApiClient, List.of("suspend")));
    }

    @Test
    public void generateHttpInterfaceReactiveWithCoroutines() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, true,
                DECLARATIVE_INTERFACE_REACTIVE_MODE, "coroutines",
                USE_RESPONSE_ENTITY, false,
                REQUEST_MAPPING_OPTION, "api_interface",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt"),
                        List.of("@HttpExchange(\n"
                                + "\"\\${openapi.openAPIPetstore.base-path:\\${api.base-path:$BASE_PATH}}\"\n"
                                + ")",
                                "    suspend fun getInventory(\n"
                                + "    ): Map<String, kotlin.Int>",
                                "    suspend fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): Unit",
                                "    suspend fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): Order")
                )
        );
    }

    @Test
    public void generateHttpInterfaceResponseEntity() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, false,
                USE_RESPONSE_ENTITY, true,
                REQUEST_MAPPING_OPTION, "none",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path storeApiClient = apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt");
        assertGeneratedFilesContain(
                Map.of(
                        storeApiClient,
                        List.of("    fun getInventory(\n"
                                + "    ): ResponseEntity<Map<String, kotlin.Int>>",
                                "    fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): ResponseEntity<Unit>",
                                "    fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): ResponseEntity<Order>")
                )
        );
        assertGeneratedFilesNotContain(Map.of(storeApiClient, List.of("suspend")));
    }

    @Test
    public void generateHttpInterface() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
                CodegenConstants.LIBRARY, "spring-declarative-http-interface",
                REACTIVE, false,
                USE_RESPONSE_ENTITY, false,
                REQUEST_MAPPING_OPTION, "none",
                USE_FLOW_FOR_ARRAY_RETURN_TYPE, false
        ), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path storeApiClient = apiSources.resolve("src/main/kotlin/org/openapitools/api/StoreApiClient.kt");
        assertGeneratedFilesContain(
                Map.of(
                        storeApiClient, List.of(
                                "    fun getInventory(\n"
                                + "    ): Map<String, kotlin.Int>",
                                "    fun deleteOrder(\n"
                                + "        @Parameter(description = \"ID of the order that needs to be deleted\", required = true) @PathVariable(\"orderId\") orderId: kotlin.String\n"
                                + "    ): Unit",
                                "    fun placeOrder(\n"
                                + "        @Parameter(description = \"order placed for purchasing the pet\", required = true) @Valid @RequestBody order: Order\n"
                                + "    ): Order")
                )
        );
        assertGeneratedFilesNotContain(Map.of(storeApiClient, List.of("suspend")));
    }

    @Test
    public void generateNonSerializableModelWithXimplements() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore-with-x-kotlin-implements.yaml", Map.of(), Map.of(
                CodegenConstants.MODELS, "true",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "false",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path dogDto = apiSources.resolve("src/main/kotlin/org/openapitools/model/Dog.kt");
        assertGeneratedFilesContain(
                Map.of(
                        dogDto, List.of(
                                "@get:JsonProperty(\"likesFetch\", required = true) override val likesFetch: kotlin.Boolean,",
                                ") : Pet, com.some.pack.Fetchable {")
                )
        );
        assertGeneratedFilesNotContain(
                Map.of(
                        dogDto, List.of(
                                "import java.io.Serializable",
                                ") : Pet, Serializable,  com.some.pack.Fetchable {",
                                ") : Pet, Serializable {",
                                "private const val serialVersionUID: kotlin.Long = 1")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwaggerNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange)")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwagger1NoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextControllerImplAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwaggerNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwagger1NoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextControllerImplAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1NoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1NoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegateWithApiTests() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/test/kotlin/org/openapitools/api/PetApiTest.kt"), List.of(
                                "val request: javax.servlet.http.HttpServletRequest = TODO()",
                                "api.deletePet(petId, apiKey, request)"),
                        apiSources.resolve("src/test/kotlin/org/openapitools/api/UserApiTest.kt"), List.of(
                                "val request: javax.servlet.http.HttpServletRequest = TODO()",
                                "api.logoutUser(request)")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneNoDelegateWithApiTests() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange)"),
                        apiSources.resolve("src/test/kotlin/org/openapitools/api/PetApiTest.kt"), List.of(
                                "val exchange: org.springframework.web.server.ServerWebExchange = TODO()",
                                "api.deletePet(petId, apiKey, exchange)"),
                        apiSources.resolve("src/test/kotlin/org/openapitools/api/UserApiTest.kt"), List.of(
                                "val exchange: org.springframework.web.server.ServerWebExchange = TODO()",
                                "api.logoutUser(exchange)")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneNoDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwaggerDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationSwagger1Delegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextControllerImplAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextControllerImplAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwaggerDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationSwagger1Delegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "         @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "         @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "         @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "         @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "         @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextControllerImplAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "fun deletePet(\n"
                                + "         @PathVariable(\"petId\") petId: kotlin.Long,\n"
                                + "         @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,\n"
                                + "        request: javax.servlet.http.HttpServletRequest\n"
                                + "    ): ResponseEntity<Unit> {",
                                "fun getPetById(\n"
                                + "         @PathVariable(\"petId\") petId: kotlin.Long,\n"
                                + "        request: javax.servlet.http.HttpServletRequest\n"
                                + "    ): ResponseEntity<Pet> {"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextControllerImplAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1Delegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        exchange: org.springframework.web.server.ServerWebExchange"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>",
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwaggerDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @Parameter(description = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(description = \"\", `in` = ParameterIn.HEADER) @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @Parameter(description = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@Parameter(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationSwagger1Delegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @ApiParam(value = \"Pet id to delete\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(value = \"\") @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @ApiParam(value = \"ID of pet to return\", required = true) @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(@ApiParam(hidden = true) request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        request: javax.servlet.http.HttpServletRequest"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>",
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "request: javax.servlet.http.HttpServletRequest): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void nonReactiveWithoutHttpRequestContextInterfaceOnlyAnnotationNoneDelegate() {
        Path apiSources = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
                Map.of(
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of(
                                "deletePet("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long,"
                                + "        @RequestHeader(value = \"api_key\", required = false) apiKey: kotlin.String?"
                                + "    ): ResponseEntity<Unit>",
                                "getPetById("
                                + "        @PathVariable(\"petId\") petId: kotlin.Long"
                                + "    ): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "logoutUser(): ResponseEntity<Unit>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "apiKey: kotlin.String?): ResponseEntity<Unit>",
                                "petId: kotlin.Long): ResponseEntity<Pet>"),
                        apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiDelegate.kt"), List.of(
                                "(): ResponseEntity<Unit>")
                )
        );
    }

    @Test
    public void reactiveWithoutResponseEntity() throws Exception {
        Path root = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml",
                Map.of(
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
        assertGeneratedFilesContain(
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
        Path root = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml",
                Map.of(
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
        assertGeneratedFilesContain(
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

        assertGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("suspend"),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("suspend"),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("suspend")
                )
        );
    }

    @Test
    public void reactiveWithResponseEntity() throws Exception {
        Path root = generateApiSources(
                "src/test/resources/3_0/kotlin/petstore.yaml",
                Map.of(
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
        assertGeneratedFilesContain(
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

        assertGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("@ResponseStatus(HttpStatus.")
                )
        );
    }

    @Test
    public void nonReactiveWithResponseEntity() throws Exception {
        Path root = generateApiSources("src/test/resources/3_0/kotlin/petstore.yaml", Map.of(
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
        assertGeneratedFilesContain(
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

        assertGeneratedFilesNotContain(
                Map.of(
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus."),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of("suspend", "@ResponseStatus(HttpStatus.")
                )
        );
    }

    @Test
    public void reactiveWithoutFlow() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml",
                Map.of(
                        KotlinSpringServerCodegen.REACTIVE, true,
                        KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false,
                        KotlinSpringServerCodegen.USE_TAGS, true,
                        KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true,
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("List<kotlin.String>")
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("Flow<kotlin.String>")
        ));
    }

    @Test
    public void reactiveWithFlow() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml",
                Map.of(
                        KotlinSpringServerCodegen.REACTIVE, true,
                        KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true,
                        KotlinSpringServerCodegen.USE_TAGS, true,
                        KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true,
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("Flow<kotlin.String>")
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("List<kotlin.String>")
        ));
    }

    @Test
    public void reactiveWithDefaultValueFlow() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml",
                Map.of(
                        KotlinSpringServerCodegen.REACTIVE, true,
                        KotlinSpringServerCodegen.USE_TAGS, true,
                        KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true,
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("Flow<kotlin.String>")
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("List<kotlin.String>")
        ));
    }

    @Test
    public void nonReactiveWithoutFlow() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml",
                Map.of(
                        KotlinSpringServerCodegen.REACTIVE, false,
                        KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false,
                        KotlinSpringServerCodegen.USE_TAGS, true,
                        KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true,
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("List<kotlin.String>")
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("Flow<kotlin.String>")
        ));
    }

    @Test
    public void nonReactiveWithFlow() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue16130-add-useFlowForArrayReturnType-param.yaml",
                Map.of(
                        KotlinSpringServerCodegen.REACTIVE, false,
                        KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true,
                        KotlinSpringServerCodegen.USE_TAGS, true,
                        KotlinSpringServerCodegen.SERVICE_IMPLEMENTATION, true,
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("List<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("List<kotlin.String>")
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiController.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1Api.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiDelegate.kt"), List.of("Flow<kotlin.String>"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/TestV1ApiService.kt"), List.of("Flow<kotlin.String>")
        ));
    }

    @Test
    public void testValidationsInQueryParams_issue21238_Controller() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue21238_queryParam_validation.yaml",
                Map.of(),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApiController.kt"), List.of("@NotNull", "@Valid"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApiController.kt"), List.of(
                        "@NotNull",
                        "@Valid",
                        "@Pattern(regexp=\"^[a-zA-Z0-9]+[a-zA-Z0-9\\\\.\\\\-_]*[a-zA-Z0-9]+$\")",
                        "@Parameter(description = \"The user name for login\", required = true)",
                        "@Parameter(description = \"The password for login in clear text\", required = true)"
                )
        ));
    }

    @Test
    public void testValidationsInQueryParams_issue21238_Api_Delegate() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/issue21238_queryParam_validation.yaml",
                Map.of(KotlinSpringServerCodegen.DELEGATE_PATTERN, true),
                Map.of(
                        CodegenConstants.MODELS, "false",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt"), List.of("@NotNull", "@Valid"),
                apiSources.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                        "@NotNull",
                        "@Valid",
                        "@Pattern(regexp=\"^[a-zA-Z0-9]+[a-zA-Z0-9\\\\.\\\\-_]*[a-zA-Z0-9]+$\")"
                )
        ));
    }


    @Test
    public void testDollarsAndQuotesSwagger1() {
        Path apiSources = generateApiSources(
                "src/test/resources/3_0/kotlin/petstore-with-tags.yaml",
                Map.of(
                        KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "springfox",
                        KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger1",
                        KotlinSpringServerCodegen.DELEGATE_PATTERN, true
                ),
                Map.of(
                        CodegenConstants.MODELS, "true",
                        CodegenConstants.MODEL_TESTS, "false",
                        CodegenConstants.MODEL_DOCS, "false",
                        CodegenConstants.APIS, "true",
                        CodegenConstants.SUPPORTING_FILES, "false"
                )
        );
        assertGeneratedFilesContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/itemsApi.kt"),
                List.of("value = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\"",
                        "@PathVariable(\"item\\$Id\")",
                        "@PathVariable(\"item\\$SubId\")",
                        "@RequestParam(value = \"filter\\$Type\"",
                        "@RequestParam(value = \"filter\\$SubType\"",
                        "@CookieValue(name = \"session\\$Token\"",
                        "@CookieValue(name = \"session\\$TokenTwo\"",
                        "@RequestParam(value = \"form\\$Name\"",
                        "@RequestParam(value = \"form\\$Value\"",
                        "PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET: String = \"/items/{item\\$Id}/something/{item\\$SubId}\"",
                        "/* \"/items/{item$Id}/something/{item$SubId}\" */"
                        ),
                apiSources.resolve("src/main/kotlin/org/openapitools/model/ItemsItemIdSomethingItemSubIdGet200Response.kt"),
                List.of(
                        "@ApiModelProperty(example = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\", value = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\")",
                        "@get:JsonProperty(\"item\\$Id\") val itemDollarId: kotlin.String? = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\"",
                        "@get:JsonProperty(\"name\\$Value\") val nameDollarValue: kotlin.String? = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\"",
                        "@get:JsonProperty(\"details\\$Info\")",
                        "@param itemDollarId SQ = \"; SBS = \\; DBS = \\\\; SD = $some",
                        "@param nameDollarValue SQ = \"; SBS = \\; DBS = \\\\; SD = $some"
                        ),
                apiSources.resolve("src/main/kotlin/org/openapitools/model/ItemWithDollarAttributesAndExamples.kt"),
                List.of(
                        "@ApiModelProperty(example = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\", value = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\")",
                        "@get:JsonProperty(\"\\$id\") val dollarId: kotlin.String? = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\"",
                        "@get:JsonProperty(\"\\$name\") val dollarName: kotlin.String? = \"SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = \\$some\"",
                        "* SQ = \"; SBS = \\; DBS = \\\\; SD = $some",
                        "* @param dollarId SQ = \"; SBS = \\; DBS = \\\\; SD = $some",
                        "* @param dollarName SQ = \"; SBS = \\; DBS = \\\\; SD = $some"
                )
        ));
        assertGeneratedFilesNotContain(Map.of(
                apiSources.resolve("src/main/kotlin/org/openapitools/api/itemsApi.kt"),
                List.of(
                        "SQ = \\\\\";",
                        "SBS = \\\\\\\\;",
                        "DBS = \\\\\\\\\\\\\\\\;",
                        "SD = \\\\$some",
                        "@PathVariable(\"item$Id\")",
                        "@PathVariable(\"item$SubId\")",
                        "@RequestParam(value = \"filter$Type\"",
                        "@RequestParam(value = \"filter$SubType\"",
                        "@CookieValue(name = \"session$Token\"",
                        "@CookieValue(name = \"session$TokenTwo\"",
                        "@RequestParam(value = \"form$Name\"",
                        "@RequestParam(value = \"form$Value\"",
                        "PATH_ITEMS_ITEM_ID_SOMETHING_ITEM_SUB_ID_GET: String = \"/items/{item$Id}/something/{item$SubId}\"",
                        "/* \"/items/{item\\$Id}/something/{item\\$SubId}\" */"
                        ),
                apiSources.resolve("src/main/kotlin/org/openapitools/model/ItemsItemIdSomethingItemSubIdGet200Response.kt"),
                List.of(
                        "SQ = \\\\\";",
                        "SBS = \\\\\\\\;",
                        "DBS = \\\\\\\\\\\\\\\\;",
                        "SD = \\\\$some",
                        "item$Id",
                        "name$Value",
                        "details$Info"
                ),
                apiSources.resolve("src/main/kotlin/org/openapitools/model/ItemWithDollarAttributesAndExamples.kt"),
                List.of(
                        "SQ = \\\\\";",
                        "SBS = \\\\\\\\;",
                        "DBS = \\\\\\\\\\\\\\\\;",
                        "SD = \\\\$some",
                        "\"$id\"",
                        "\"$name\""
                )
        ));
    }

    private Path generateApiSources(
            String specFilePath,
            Map<String, Object> additionalProperties,
            Map<String, String> generatorPropertyDefaults
    ) {
        File outputDir;
        try {
            outputDir = Files.createTempDirectory("test").toFile().getCanonicalFile();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        outputDir.deleteOnExit();
        String outputPath = outputDir.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(outputDir.getAbsolutePath());
        codegen.additionalProperties().putAll(additionalProperties);

        ClientOptInput input = new ClientOptInput()
                .openAPI(TestUtils.parseSpec(specFilePath))
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        for (var entry : generatorPropertyDefaults.entrySet()) {
            generator.setGeneratorPropertyDefault(entry.getKey(), entry.getValue());
        }
        generator.opts(input).generate();

        return Paths.get(outputPath);
    }

    private static void assertGeneratedFilesContain(Map<Path, List<String>> expectedSnippetsByPathsToFiles) {
        for (var expectedSnippetsByPathToFile : expectedSnippetsByPathsToFiles.entrySet()) {
            assertFileContains(expectedSnippetsByPathToFile.getKey(), expectedSnippetsByPathToFile.getValue().toArray(new String[0]));
        }
    }

    private static void assertGeneratedFilesNotContain(Map<Path, List<String>> unexpectedSnippetsByPathsToFiles) {
        for (var unexpectedSnippetsByPathToFile : unexpectedSnippetsByPathsToFiles.entrySet()) {
            assertFileNotContains(unexpectedSnippetsByPathToFile.getKey(), unexpectedSnippetsByPathToFile.getValue().toArray(new String[0]));
        }
    }

    private static void assertGeneratedFilesExist(List<Path> files) {
        for (Path file : files) {
            Assertions.assertThat(Files.exists(file))
                    .withFailMessage("Expected file %s to exist but was not found", file.toAbsolutePath())
                    .isTrue();
        }
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
}
