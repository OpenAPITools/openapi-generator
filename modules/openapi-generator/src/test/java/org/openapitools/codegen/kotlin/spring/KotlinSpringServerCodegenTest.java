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
import org.openapitools.codegen.languages.AbstractKotlinCodegen;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.DocumentationProvider;
import org.openapitools.codegen.languages.features.SwaggerUIFeatures;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.CodegenConstants.USE_ENUM_VALUE_INTERFACE;
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
    public void testOneOfInterfaceInheritedEnumDiscriminator() throws IOException {
        // Cross-generator check for the DefaultCodegen discriminator-type fix: the kotlin-spring
        // oneof_interface template emits the discriminator getter type too. Issue #22541: the
        // inline-enum discriminator is inherited from a base schema via allOf, so the sealed
        // interface must use the enum type rather than String.
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseOneOfInterfaces(true);
        codegen.setLegacyDiscriminatorBehavior(false);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, "false");

        generator.opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/oneOfDiscriminator.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(output + "/src/main/kotlin/org/openapitools/model/PetResponseEnumDisc.kt"),
                "val petType: PetType"
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
                "import kotlinx.coroutines.flow.Flow", "ResponseEntity<List<kotlin.String>>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2Api.kt"),
                "exchange");
        assertFileContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                "import kotlinx.coroutines.flow.Flow", "suspend fun", "ResponseEntity<List<kotlin.String>>");
        assertFileNotContains(Paths.get(output + "/src/main/kotlin/org/openapitools/api/TestV2ApiDelegate.kt"),
                "ApiUtil");

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

    @Test(description = "Spring Boot 4 should use Jackson 3 datetime property path")
    public void useSpringBoot4JacksonDateTimeProperty() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        // Spring Boot 4 uses Jackson 3, which moved WRITE_DATES_AS_TIMESTAMPS to
        // spring.jackson.datatype.datetime instead of spring.jackson.serialization
        Path applicationYaml = Paths.get(outputPath + "/src/main/resources/application.yaml");
        assertFileContains(applicationYaml, "datatype:");
        assertFileContains(applicationYaml, "datetime:");
        assertFileContains(applicationYaml, "WRITE_DATES_AS_TIMESTAMPS: false");
        assertFileNotContains(applicationYaml, "serialization:");
    }

    @Test(description = "Spring Boot 3 should use Jackson 2 serialization property path")
    public void useSpringBoot3JacksonSerializationProperty() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT3, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        // Spring Boot 3 uses Jackson 2, which has WRITE_DATES_AS_TIMESTAMPS under
        // spring.jackson.serialization
        Path applicationYaml = Paths.get(outputPath + "/src/main/resources/application.yaml");
        assertFileContains(applicationYaml, "serialization:");
        assertFileContains(applicationYaml, "WRITE_DATES_AS_TIMESTAMPS: false");
        assertFileNotContains(applicationYaml, "datatype:");
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
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(example = \"null\", description = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(description = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(example = \"null\", requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:Schema(requiredMode = Schema.RequiredMode.REQUIRED, description = \"\")"
        );
    }

    @Test(description = "use get Annotation use-site target on kotlin interface attributes (swagger1)")
    public void useTargetOnInterfaceAnnotationsWithSwagger1() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.SWAGGER1.toCliOptValue());
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());


        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue3596-use-correct-get-annotation-target.yaml"))
                        .config(codegen))
                .generate();

        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@ApiModelProperty(example = \"null\", value = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(example = \"null\", value = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(value = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@ApiModelProperty(example = \"null\", required = true, value = \"\")"
        );
        assertFileNotContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(example = \"null\", required = true, value = \"\")"
        );
        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Animal.kt"),
                "@get:ApiModelProperty(required = true, value = \"\")"
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
    public void generateSerializableModelImplementsOneOfInterfaces() throws Exception {
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
                ") : java.io.Serializable, UserOrPet, UserOrPetOrArrayString {",
                "private const val serialVersionUID: kotlin.Long = 1"
        );

        Path userPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/User.kt");
        assertFileContains(
                userPath,
                ") : java.io.Serializable, UserOrPet, UserOrPetOrArrayString {",
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
                "import reactor.core.publisher.Mono",
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
                "import reactor.core.publisher.Mono",
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

    /**
     * Regression test for https://github.com/OpenAPITools/openapi-generator/issues/17445.
     * OpenAPI 'default' responses must emit responseCode = "default" in @ApiResponse (swagger2),
     * not "0" (internal pre-processed value) or "200" (incorrect mapping from parent codegen).
     * Also verifies that useResponseEntity=false does not crash when the first response is 'default'.
     */
    @Test
    public void defaultResponseCodeRenderedAsDefault() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.REACTIVE, false,
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "swagger2",
                KotlinSpringServerCodegen.INTERFACE_ONLY, true,
                KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, false
        ), Map.of(
                CodegenConstants.MODELS, "false",
                CodegenConstants.MODEL_TESTS, "false",
                CodegenConstants.MODEL_DOCS, "false",
                CodegenConstants.APIS, "true",
                CodegenConstants.SUPPORTING_FILES, "false"
        ));
        Path userApi = root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt");
        // operations whose only OpenAPI response is 'default:' must use responseCode = "default"
        assertFileContains(userApi,
                "ApiResponse(responseCode = \"default\", description = \"successful operation\")"
        );
        // explicit HTTP 200 responses must still use the concrete status code
        assertFileContains(userApi,
                "ApiResponse(responseCode = \"200\", description = \"successful operation\", content"
        );
        // the raw internal representation ("0") must never appear in generated output
        assertFileNotContains(userApi,
                "responseCode = \"0\""
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
        return new Object[][]{
                {DocumentationProviderFeatures.DocumentationProvider.SPRINGDOC.name(),
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
        assertFileContains(petApi.toPath(), "fun listAllPets(@PageableDefault(page = 0, size = 20) @Parameter(hidden = true) pageable: Pageable)");
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

    @Test(description = "reactive spring-boot: array-of-string returns List<String> with suspend, not Flow<String> (issue #22662)")
    public void reactiveArrayOfStringReturnsListNotFlow() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, true);

        List<File> files = new DefaultGenerator()
                .opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/bugs/issue_7118.yaml"))
                        .config(codegen))
                .generate();

        Path apiPath = files.stream()
                .filter(f -> f.getName().equals("UsersApi.kt"))
                .findFirst()
                .orElseThrow()
                .toPath();

        assertFileContains(apiPath, "suspend fun", "List<kotlin.String>", "Set<kotlin.String>");
        // neither the list nor the uniqueItems (Set) operation must leak Flow<...> or a raw/nested container
        assertFileNotContains(apiPath, "Flow<kotlin.String>", "Flow<kotlin.collections.Set", "kotlin.collections.Set<");
    }

    @Test(description = "declarative http interface reactor: array-of-string returns Mono<List<String>>, not Flux<String> (issue #22662)")
    public void declarativeReactorArrayOfStringReturnsMono() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, false);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        List<File> files = new DefaultGenerator()
                .opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/bugs/issue_7118.yaml"))
                        .config(codegen))
                .generate();

        Path apiPath = files.stream()
                .filter(f -> f.getName().equals("UsersApi.kt"))
                .findFirst()
                .orElseThrow()
                .toPath();

        assertFileContains(apiPath, "Mono<List<kotlin.String>>", "Mono<Set<kotlin.String>>");
        assertFileNotContains(apiPath, "Flux<kotlin.String>", "import reactor.core.publisher.Flux",
                "kotlin.collections.Set<", "Mono<set<");
    }

    @Test(description = "declarative http interface reactor + ResponseEntity: array-of-string returns Mono<ResponseEntity<List<String>>> (issue #22662)")
    public void declarativeReactorArrayOfStringReturnsMonoResponseEntity() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.REACTIVE, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.DECLARATIVE_INTERFACE_REACTIVE_MODE, "reactor");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_RESPONSE_ENTITY, true);
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_FLOW_FOR_ARRAY_RETURN_TYPE, false);

        List<File> files = new DefaultGenerator()
                .opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/bugs/issue_7118.yaml"))
                        .config(codegen))
                .generate();

        Path apiPath = files.stream()
                .filter(f -> f.getName().equals("UsersApi.kt"))
                .findFirst()
                .orElseThrow()
                .toPath();

        assertFileContains(apiPath, "Mono<ResponseEntity<List<kotlin.String>>>", "Mono<ResponseEntity<Set<kotlin.String>>>");
        assertFileNotContains(apiPath, "Flux<kotlin.String>", "import reactor.core.publisher.Flux",
                "kotlin.collections.Set<", "Mono<ResponseEntity<set<");
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

    // ========== GENERATE PAGEABLE CONSTRAINT VALIDATION TESTS ==========

    @Test
    public void generatePageableConstraintValidationAddsSizeConstraint() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithSizeConstraint has maximum: 100 on size only
        int methodStart = content.indexOf("fun findPetsWithSizeConstraint(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithSizeConstraint method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidPageable(maxSize = 100)"),
                "@ValidPageable(maxSize = 100) should appear on the pageable parameter");
        Assert.assertFalse(paramBlock.contains("maxPage"),
                "maxPage should not appear when only size has a maximum constraint");

        assertFileContains(petApi.toPath(), "import org.openapitools.configuration.ValidPageable");
    }

    @Test
    public void generatePageableConstraintValidationAddsPageAndSizeConstraint() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithPageAndSizeConstraint has maximum: 999 on page and maximum: 50 on size
        int methodStart = content.indexOf("fun findPetsWithPageAndSizeConstraint(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithPageAndSizeConstraint method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidPageable(maxSize = 50, maxPage = 999)"),
                "@ValidPageable(maxSize = 50, maxPage = 999) should appear on the pageable parameter");
    }

    @Test
    public void generatePageableConstraintValidationGeneratesValidPageableFile() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File validPageableFile = files.get("ValidPageable.kt");
        Assert.assertNotNull(validPageableFile, "ValidPageable.kt should be generated when generatePageableConstraintValidation=true");
        assertFileContains(validPageableFile.toPath(), "annotation class ValidPageable");
        assertFileContains(validPageableFile.toPath(), "class PageableConstraintValidator");
        assertFileContains(validPageableFile.toPath(), "val maxSize: Int");
        assertFileContains(validPageableFile.toPath(), "val maxPage: Int");
        assertFileContains(validPageableFile.toPath(), "val minSize: Int");
        assertFileContains(validPageableFile.toPath(), "val minPage: Int");
        assertFileContains(validPageableFile.toPath(), "NO_LIMIT");
    }

    @Test
    public void generatePageableConstraintValidationDoesNotGenerateFileWhenDisabled() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        // NOT setting GENERATE_PAGEABLE_CONSTRAINT_VALIDATION (defaults to false)

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        Assert.assertNull(files.get("ValidPageable.kt"), "ValidPageable.kt should NOT be generated when generatePageableConstraintValidation=false");
        File petApi = files.get("PetApi.kt");
        assertFileNotContains(petApi.toPath(), "@ValidPageable");
    }

    @Test
    public void generatePageableConstraintValidationDoesNotGenerateFileWhenBeanValidationDisabled() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");
        additionalProperties.put(USE_BEANVALIDATION, "false");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        Assert.assertNull(files.get("ValidPageable.kt"), "ValidPageable.kt should NOT be generated when useBeanValidation=false");
        File petApi = files.get("PetApi.kt");
        assertFileNotContains(petApi.toPath(), "@ValidPageable");
    }

    // ========== AUTO X-SPRING-PAGINATED TESTS ==========

    @Test
    public void generatePageableConstraintValidationResolvesMaximumFromAllOfRef() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithSizeConstraintFromAllOfRef: maximum: 75 is on the referenced schema only
        int methodStart = content.indexOf("fun findPetsWithSizeConstraintFromAllOfRef(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithSizeConstraintFromAllOfRef method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidPageable(maxSize = 75)"),
                "@ValidPageable(maxSize = 75) should be resolved from allOf $ref schema");
    }

    @Test
    public void generatePageableConstraintValidationResolvesMinimumFromAllOfRef() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithMinSizeConstraintFromAllOfRef: minimum: 5 is on the referenced schema only
        int methodStart = content.indexOf("fun findPetsWithMinSizeConstraintFromAllOfRef(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithMinSizeConstraintFromAllOfRef method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidPageable(minSize = 5)"),
                "@ValidPageable(minSize = 5) should be resolved from allOf $ref schema");
    }

    // ========== AUTO X-SPRING-PAGINATED TESTS ==========

    @Test
    public void generateSortValidationAddsAnnotationForExplicitPaginated() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "@ValidSort(allowedValues = [\"id,asc\", \"id,desc\", \"name,asc\", \"name,desc\"])");
        assertFileContains(petApi.toPath(), "import org.openapitools.configuration.ValidSort");

        // @ValidSort must be a parameter annotation — appears in the 500-char window AFTER `fun findPetsWithSortEnum(`
        String content = Files.readString(petApi.toPath());
        int methodStart = content.indexOf("fun findPetsWithSortEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithSortEnum method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidSort(allowedValues = [\"id,asc\", \"id,desc\", \"name,asc\", \"name,desc\"])"),
                "@ValidSort should appear as a parameter annotation (inside the method signature, after `fun`)");
        Assert.assertTrue(paramBlock.contains("pageable: Pageable"),
                "findPetsWithSortEnum should have a pageable: Pageable parameter");

        // @ValidSort must NOT be a method-level annotation (not in the 500-char prefix before `fun`)
        String prefixBlock = content.substring(Math.max(0, methodStart - 500), methodStart);
        Assert.assertFalse(prefixBlock.contains("@ValidSort"),
                "@ValidSort should be a parameter annotation, not a method-level annotation");
    }

    @Test
    public void generateSortValidationAddsAnnotationForAutoDetectedPaginated() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");
        additionalProperties.put(AUTO_X_SPRING_PAGINATED, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "@ValidSort(allowedValues = [\"id,asc\", \"id,desc\"])");
    }

    @Test
    public void generateSortValidationHandlesRefSortEnum() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "@ValidSort(allowedValues = [\"id,asc\", \"id,desc\", \"createdAt,asc\", \"createdAt,desc\"])");
    }

    @Test
    public void generateSortValidationDoesNotAnnotateNonPaginatedOperation() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsNonPaginatedWithSortEnum has sort enum but NO pagination — must not get @ValidSort
        int methodStart = content.indexOf("fun findPetsNonPaginatedWithSortEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsNonPaginatedWithSortEnum method should exist");
        String methodBlock = content.substring(Math.max(0, methodStart - 500), methodStart);
        Assert.assertFalse(methodBlock.contains("@ValidSort"),
                "Non-paginated operation should not have @ValidSort even if sort param has enum values");
    }

    @Test
    public void generateSortValidationDoesNotAnnotateWhenSortHasNoEnum() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithoutSortEnum has pagination but sort has NO enum values
        int methodStart = content.indexOf("fun findPetsWithoutSortEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithoutSortEnum method should exist");
        String methodBlock = content.substring(Math.max(0, methodStart - 500), methodStart);
        Assert.assertFalse(methodBlock.contains("@ValidSort"),
                "Paginated operation with non-enum sort should not have @ValidSort");
    }

    @Test
    public void generateSortValidationGeneratesValidSortFile() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File validSortFile = files.get("ValidSort.kt");
        Assert.assertNotNull(validSortFile, "ValidSort.kt should be generated when generateSortValidation=true");
        assertFileContains(validSortFile.toPath(), "annotation class ValidSort");
        assertFileContains(validSortFile.toPath(), "class SortValidator");
        assertFileContains(validSortFile.toPath(), "val allowedValues: Array<String>");
        assertFileContains(validSortFile.toPath(), "DIRECTION_ASC_SUFFIX");
        assertFileContains(validSortFile.toPath(), "DIRECTION_DESC_SUFFIX");
    }

    @Test
    public void generateSortValidationDoesNotGenerateValidSortFileWhenDisabled() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        // NOT setting GENERATE_SORT_VALIDATION (defaults to false)

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        Assert.assertNull(files.get("ValidSort.kt"), "ValidSort.kt should NOT be generated when generateSortValidation=false");
        File petApi = files.get("PetApi.kt");
        assertFileNotContains(petApi.toPath(), "@ValidSort");
    }

    @Test
    public void generateSortValidationDoesNotGenerateValidSortFileWhenBeanValidationDisabled() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");
        additionalProperties.put(USE_BEANVALIDATION, "false");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        Assert.assertNull(files.get("ValidSort.kt"), "ValidSort.kt should NOT be generated when useBeanValidation=false");
        File petApi = files.get("PetApi.kt");
        assertFileNotContains(petApi.toPath(), "@ValidSort");
    }

    @Test
    public void generateSortValidationWorksForArraySortEnum() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithArraySortEnum: sort is type:array, items have inline enum → @ValidSort applied with Kotlin [] syntax
        int methodStart = content.indexOf("fun findPetsWithArraySortEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithArraySortEnum method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidSort(allowedValues = [\"id,asc\", \"id,desc\", \"name,asc\", \"name,desc\"])"),
                "@ValidSort with all four enum values should appear on the pageable parameter");
        Assert.assertTrue(paramBlock.contains("pageable: Pageable"),
                "findPetsWithArraySortEnum should have a pageable: Pageable parameter");
    }

    @Test
    public void generateSortValidationWorksForArraySortRefEnum() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithArraySortRefEnum: sort is type:array, items $ref to PetSort enum → @ValidSort with PetSort values
        int methodStart = content.indexOf("fun findPetsWithArraySortRefEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithArraySortRefEnum method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidSort(allowedValues = [\"id,asc\", \"id,desc\", \"createdAt,asc\", \"createdAt,desc\"])"),
                "@ValidSort with PetSort enum values should appear on the pageable parameter");
    }

    @Test
    public void generateSortValidationWorksForExternalParamRefArraySort() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithExternalParamRefArraySort: sort param $ref to external components file,
        // which defines type:array with items $ref to PetSortEnum in the same external file
        int methodStart = content.indexOf("fun findPetsWithExternalParamRefArraySort(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithExternalParamRefArraySort method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidSort(allowedValues = ["),
                "@ValidSort should appear when sort param is resolved from an external $ref parameter");
        Assert.assertTrue(paramBlock.contains("\"name,asc\"") || paramBlock.contains("\"id,asc\""),
                "@ValidSort should contain the enum values from the external PetSortEnum schema");
        Assert.assertTrue(paramBlock.contains("pageable: Pageable"),
                "findPetsWithExternalParamRefArraySort should have a pageable: Pageable parameter");
    }

    @Test
    public void generateSortValidationWorksForNonExplodedExternalParamRefArraySort() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");
        additionalProperties.put(GENERATE_SORT_VALIDATION, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsWithNonExplodedExternalParamRefArraySort: sort param $ref to external file,
        // explode: false — Spring parses ?sort=id,asc,name,desc as sequential token pairs.
        // @ValidSort validation works the same way since it operates on the deserialized Pageable.
        int methodStart = content.indexOf("fun findPetsWithNonExplodedExternalParamRefArraySort(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithNonExplodedExternalParamRefArraySort method should exist");
        String paramBlock = content.substring(methodStart, Math.min(content.length(), methodStart + 500));
        Assert.assertTrue(paramBlock.contains("@ValidSort(allowedValues = [\"name,asc\", \"name,desc\", \"id,asc\", \"id,desc\"])"),
                "@ValidSort with PetSortEnum values should appear even for non-exploded array sort param");
        Assert.assertTrue(paramBlock.contains("pageable: Pageable"),
                "findPetsWithNonExplodedExternalParamRefArraySort should have a pageable: Pageable parameter");
    }

    // ========== PAGEABLE DEFAULTS TESTS ==========

    @Test
    public void pageableDefaultsGeneratesSortDefaultsForSingleDescField() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(),
                "@SortDefault.SortDefaults(SortDefault(sort = [\"name\"], direction = Sort.Direction.DESC))");
        assertFileContains(petApi.toPath(), "import org.springframework.data.domain.Sort");
        assertFileContains(petApi.toPath(), "import org.springframework.data.web.SortDefault");
    }

    @Test
    public void pageableDefaultsGeneratesSortDefaultsForSingleAscField() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(),
                "@SortDefault.SortDefaults(SortDefault(sort = [\"id\"], direction = Sort.Direction.ASC))");
    }

    @Test
    public void pageableDefaultsGeneratesSortDefaultsForMixedDirections() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(),
                "@SortDefault.SortDefaults(SortDefault(sort = [\"name\"], direction = Sort.Direction.DESC), SortDefault(sort = [\"id\"], direction = Sort.Direction.ASC))");
    }

    @Test
    public void pageableDefaultsGeneratesPageableDefaultForPageAndSize() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        assertFileContains(petApi.toPath(), "@PageableDefault(page = 0, size = 25)");
        assertFileContains(petApi.toPath(), "import org.springframework.data.web.PageableDefault");
    }

    @Test
    public void pageableDefaultsGeneratesBothAnnotationsWhenAllDefaultsPresent() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        int methodStart = content.indexOf("fun findPetsWithAllDefaults(");
        Assert.assertTrue(methodStart >= 0, "findPetsWithAllDefaults method should exist");
        String methodBlock = content.substring(Math.max(0, methodStart - 500), methodStart + 500);

        Assert.assertTrue(methodBlock.contains("@PageableDefault(page = 0, size = 10)"),
                "findPetsWithAllDefaults should have @PageableDefault(page = 0, size = 10)");
        Assert.assertTrue(methodBlock.contains(
                        "@SortDefault.SortDefaults(SortDefault(sort = [\"name\"], direction = Sort.Direction.DESC), SortDefault(sort = [\"id\"], direction = Sort.Direction.ASC))"),
                "findPetsWithAllDefaults should have @SortDefault.SortDefaults with both fields");
    }

    @Test
    public void pageableDefaultsDoesNotAnnotateNonPageableOperation() throws Exception {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract("src/test/resources/3_0/spring/petstore-sort-validation.yaml", additionalProperties);

        File petApi = files.get("PetApi.kt");
        String content = Files.readString(petApi.toPath());

        // findPetsNonPaginatedWithSortEnum has no x-spring-paginated, so no pageable annotations
        int methodStart = content.indexOf("fun findPetsNonPaginatedWithSortEnum(");
        Assert.assertTrue(methodStart >= 0, "findPetsNonPaginatedWithSortEnum method should exist");
        String methodBlock = content.substring(Math.max(0, methodStart - 500), methodStart);
        Assert.assertFalse(methodBlock.contains("@SortDefault"),
                "Non-paginated operation should not have @SortDefault");
        Assert.assertFalse(methodBlock.contains("@PageableDefault"),
                "Non-paginated operation should not have @PageableDefault");
    }

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
    public void explicitXSpringPaginatedIgnoredForSpringCloud() throws Exception {
        // When x-spring-paginated: true is set explicitly in the spec but the library is spring-cloud,
        // the extension must be stripped so the template does not emit "pageable: Pageable".
        // Individual page/size/sort @RequestParam args from the spec should remain.
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(USE_TAGS, "true");
        additionalProperties.put(DOCUMENTATION_PROVIDER, "springdoc");
        additionalProperties.put(INTERFACE_ONLY, "true");
        additionalProperties.put(SKIP_DEFAULT_INTERFACE, "true");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-with-spring-pageable.yaml",
                additionalProperties,
                new HashMap<>(),
                configurator -> configurator.setLibrary("spring-cloud")
        );

        File petApi = files.get("PetApi.kt");
        Assert.assertNotNull(petApi, "PetApi.kt should be generated for spring-cloud library");

        // No Pageable type or its import must appear for spring-cloud
        assertFileNotContains(petApi.toPath(),
                "import org.springframework.data.domain.Pageable",
                "pageable: Pageable");

        // findPetsByStatus must exist without a Pageable parameter
        assertFileContains(petApi.toPath(), "fun findPetsByStatus(");

        // findPetsByTags must retain all individual query params defined alongside x-spring-paginated
        assertFileContains(petApi.toPath(), "@RequestParam(value = \"page\"");
        assertFileContains(petApi.toPath(), "@RequestParam(value = \"sort\"");
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

    @Test
    public void testDeprecatedAnnotationOnInterface() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/support-deprecated-api.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PingApi.kt"),
                "@Deprecated(message=\"Operation is deprecated\") @RequestMapping("
        );
    }

    @Test
    public void testDeprecatedAnnotationOnController() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/support-deprecated-api.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/PingApiController.kt"),
                "@Deprecated(message=\"Operation is deprecated\") @RequestMapping("
        );
    }

    @Test
    public void testCompanionObjectDefaultIsFalse() {
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(COMPANION_OBJECT), false);
    }

    @Test
    public void testCompanionObjectGeneratesCompanionInModel() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(COMPANION_OBJECT, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        assertFileContains(
                Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt"),
                "companion object { }"
        );
    }

    @Test
    public void shouldRefuseJackson3WithoutSpringBoot4() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "false");
        codegen.additionalProperties().put(AbstractKotlinCodegen.USE_JACKSON_3, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input);

        Assertions.assertThatExceptionOfType(IllegalArgumentException.class)
                .isThrownBy(generator::generate);
    }

    @Test
    public void shouldRefuseSpringBoot3AndSpringBoot4Together() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT3, "true");
        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input);

        Assertions.assertThatExceptionOfType(IllegalArgumentException.class)
                .isThrownBy(generator::generate);
    }

    @Test
    public void shouldAllowOpenApiNullableWithJackson3() throws IOException {
        // jackson-databind-nullable >= 0.2.10 supports both Jackson 2 and 3,
        // so openApiNullable + useJackson3 should no longer throw.
        Map<String, Object> props = new HashMap<>();
        props.put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        props.put(AbstractKotlinCodegen.USE_JACKSON_3, "true");
        props.put(CodegenConstants.OPENAPI_NULLABLE, "true");
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml", props);
        Assert.assertTrue(files.containsKey("TestModel.kt"), "TestModel.kt should be generated");
    }

    @Test
    public void shouldUseJakartaImportsWithSpringBoot4() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path modelPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt");
        assertFileContains(modelPath, "jakarta.validation");
        assertFileNotContains(modelPath, "javax.validation");
    }

    @Test
    public void shouldGenerateSpringBoot4PomWithJackson3Deps() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        codegen.additionalProperties().put(AbstractKotlinCodegen.USE_JACKSON_3, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path pomPath = Paths.get(outputPath + "/pom.xml");
        assertFileContains(pomPath, "spring-boot-starter-parent");
        assertFileContains(pomPath, "4.0.1");
        assertFileContains(pomPath, "tools.jackson.dataformat");
        assertFileContains(pomPath, "tools.jackson.module");
        assertFileNotContains(pomPath, "jackson-datatype-jsr310");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.dataformat");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.module");
    }

    @Test
    public void shouldGenerateJackson3BuildDepsWithVersions() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        codegen.additionalProperties().put(AbstractKotlinCodegen.USE_JACKSON_3, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        // Gradle build file must have Jackson 3 deps with explicit versions
        Path gradlePath = Paths.get(outputPath + "/build.gradle.kts");
        assertFileContains(gradlePath, "tools.jackson.dataformat:jackson-dataformat-yaml:");
        assertFileContains(gradlePath, "tools.jackson.module:jackson-module-kotlin:");
        // Should NOT include non-existent tools.jackson.core:jackson-annotations
        assertFileNotContains(gradlePath, "tools.jackson.core:jackson-annotations");

        // Annotations stay in com.fasterxml.jackson.annotation even with Jackson 3
        Path petModelPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/model/Pet.kt");
        assertFileContains(petModelPath, "com.fasterxml.jackson.annotation.JsonProperty");
    }

    @Test
    public void shouldDefaultToJackson3WhenSpringBoot4Enabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        // useJackson3 is NOT set — should default to true
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path pomPath = Paths.get(outputPath + "/pom.xml");
        assertFileContains(pomPath, "tools.jackson.dataformat");
        assertFileContains(pomPath, "tools.jackson.module");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.dataformat");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.module");
        assertFileNotContains(pomPath, "jackson-datatype-jsr310");
    }

    @Test
    public void shouldDeclareSpringdocVersionWhenSwaggerUIDisabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.SPRINGDOC.toCliOptValue());
        codegen.additionalProperties().put(SwaggerUIFeatures.USE_SWAGGER_UI, false);

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path pomPath = Paths.get(outputPath + "/pom.xml");
        String pomContent = new String(Files.readAllBytes(pomPath), StandardCharsets.UTF_8);
        String propertiesBlock = pomContent.substring(
                pomContent.indexOf("<properties>"),
                pomContent.indexOf("</properties>"));
        assertThat(propertiesBlock).contains("<springdoc-openapi.version>");
    }

    @Test
    public void shouldNotUseLegacyOAuth2WithSpringBoot4CloudLibrary() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
                "src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary("spring-cloud");

        codegen.additionalProperties().put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path clientConfigPath = Paths.get(outputPath + "/src/main/kotlin/org/openapitools/configuration/ClientConfiguration.kt");
        // Legacy OAuth2 classes must NOT be present
        assertFileNotContains(clientConfigPath, "DefaultOAuth2ClientContext");
        assertFileNotContains(clientConfigPath, "OAuth2FeignRequestInterceptor");
        assertFileNotContains(clientConfigPath, "ClientCredentialsResourceDetails");
        assertFileNotContains(clientConfigPath, "AuthorizationCodeResourceDetails");
        assertFileNotContains(clientConfigPath, "ImplicitResourceDetails");
        assertFileNotContains(clientConfigPath, "ResourceOwnerPasswordResourceDetails");

        // Modern OAuth2 client classes MUST be present
        assertFileContains(clientConfigPath, "OAuth2AuthorizedClientManager");
        assertFileContains(clientConfigPath, "AuthorizedClientServiceOAuth2AuthorizedClientManager");
        assertFileContains(clientConfigPath, "OAuth2AuthorizeRequest");
        assertFileContains(clientConfigPath, "OAuth2AuthorizedClientService");
        assertFileContains(clientConfigPath, "ClientRegistrationRepository");
        assertFileContains(clientConfigPath, "OAuth2RequestInterceptor");
    }

    @Test
    public void shouldDefaultToJackson3WhenSpringBoot4EnabledViaSetter() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        final KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.setOutputDir(output.getAbsolutePath());

        // Set via setter, NOT additionalProperties
        codegen.setUseSpringBoot4(true);
        codegen.additionalProperties().put(DOCUMENTATION_PROVIDER, DocumentationProvider.NONE.toCliOptValue());
        codegen.additionalProperties().put(ANNOTATION_LIBRARY, AnnotationLibrary.NONE.toCliOptValue());

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.opts(input).generate();

        Path pomPath = Paths.get(outputPath + "/pom.xml");
        assertFileContains(pomPath, "tools.jackson.dataformat");
        assertFileContains(pomPath, "tools.jackson.module");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.dataformat");
        assertFileNotContains(pomPath, "com.fasterxml.jackson.module");
        assertFileNotContains(pomPath, "jackson-datatype-jsr310");
    }

    @Test
    public void shouldAddParameterWithInHeaderWhenImplicitHeadersIsTrue() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_14418.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setLibrary(SPRING_BOOT);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(KotlinSpringServerCodegen.INTERFACE_ONLY, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(AbstractKotlinCodegen.IMPLICIT_HEADERS, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        File testApi = files.get("TestApi.kt");
        String content = Files.readString(testApi.toPath());
        String methodPattern = "fun test\\s*\\(.*?\\)";
        Pattern pattern = Pattern.compile(methodPattern);


        Matcher matcher = pattern.matcher(content);
        Assert.assertTrue(matcher.find(), "Method 'test' should be found in generated file");

        String methodSignature = matcher.group();
        Assert.assertFalse(methodSignature.contains("testHeader"),
                "Header param 'testHeader' should NOT be in method signature when implicitHeaders=true");

        Assert.assertTrue(content.contains("@Parameters"),
                "@Parameters annotation should be present");
        Assert.assertTrue(content.contains("testHeader"),
                "Header name 'testHeader' should appear in the annotation");
    }

    // -------------------------------------------------------------------------
    // substituteGenericPagedModel tests
    // -------------------------------------------------------------------------

    @Test
    public void substituteGenericPagedModel_isDisabledByDefault() throws IOException {
        // Without the option the paged schemas are generated as-is
        Map<String, Object> props = new HashMap<>();
        props.put(INTERFACE_ONLY, "true");
        props.put(SKIP_DEFAULT_INTERFACE, "true");
        props.put(USE_TAGS, "true");
        props.put(USE_SPRING_BOOT3, "true");
        // NOT setting SUBSTITUTE_GENERIC_PAGED_MODEL

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", props);

        assertThat(files).containsKey("UserPage.kt");
        assertThat(files).containsKey("PageMeta.kt");
    }


    @Test
    public void substituteGenericPagedModel_keepsPagedSchemas() throws IOException {
        // Paged schema classes must still be generated — springdoc @ApiResponse annotations reference them
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        assertThat(files).containsKey("UserPage.kt");
        assertThat(files).containsKey("OrderPage.kt");
        assertThat(files).containsKey("PetPageAllOf.kt");
    }

    @Test
    public void substituteGenericPagedModel_keepsPaginationMetadataSchema() throws IOException {
        // The shared pagination-metadata schema must also remain generated
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        assertThat(files).containsKey("PageMeta.kt");
    }

    @Test
    public void substituteGenericPagedModel_keepsNonPagedSchemas() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        assertThat(files).containsKey("User.kt");
        assertThat(files).containsKey("Pet.kt");
        assertThat(files).containsKey("UserList.kt");
        assertThat(files).containsKey("SearchResult.kt");
        assertThat(files).containsKey("PetSort.kt");
    }

    @Test
    public void substituteGenericPagedModel_replacesReturnTypeInOperation() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        // listUsers must return PagedModel<User>
        assertThat(content).contains("PagedModel<User>");
    }

    @Test
    public void substituteGenericPagedModel_replacesExternalRefPagedSchema() throws IOException {
        // OrderPage uses PageMetadata from an external file — must still be detected and return type replaced
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        File orderApi = files.get("OrderApi.kt");
        assertThat(orderApi).isNotNull();
        String content = Files.readString(orderApi.toPath());
        assertThat(content).contains("PagedModel<Order>");
    }

    @Test
    public void substituteGenericPagedModel_replacesAllOfPagedSchema() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        File petApi = files.get("PetApi.kt");
        assertThat(petApi).isNotNull();
        String content = Files.readString(petApi.toPath());
        assertThat(content).contains("PagedModel<Pet>");
    }

    @Test
    public void substituteGenericPagedModel_importsPagedModelAndItemTypeInApiFile() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        // The api file must import the generated PagedModel and the item type
        assertThat(content).contains("import org.openapitools.configuration.PagedModel");
        assertThat(content).contains("import org.openapitools.model.User");
    }

    @Test
    public void substituteGenericPagedModel_doesNotReplaceNonPagedReturnType() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        // listUsersSimple returns UserList — not a paged schema, must not be replaced
        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("UserList");
        assertThat(content).doesNotContain("PagedModel<UserList>");
    }

    /**
     * Common properties shared by all substituteGenericPagedModel tests for Kotlin Spring.
     */
    private Map<String, Object> commonKotlinPagedModelProps() {
        Map<String, Object> props = new HashMap<>();
        props.put(INTERFACE_ONLY, "true");
        props.put(SKIP_DEFAULT_INTERFACE, "true");
        props.put(USE_TAGS, "true");
        props.put(USE_SPRING_BOOT3, "true");
        props.put(SUBSTITUTE_GENERIC_PAGED_MODEL, "true");
        return props;
    }

    /**
     * Properties with annotations disabled — triggers model suppression.
     */
    private Map<String, Object> noAnnotationKotlinPagedModelProps() {
        Map<String, Object> props = commonKotlinPagedModelProps();
        props.put(DOCUMENTATION_PROVIDER, "none");
        props.put(ANNOTATION_LIBRARY, "none");
        return props;
    }

    @Test
    public void substituteGenericPagedModel_suppressesPagedSchemasWhenNoAnnotations() throws IOException {
        // With annotationLibrary=none, @ApiResponse is not generated → paged schemas not referenced
        // → they should be suppressed to avoid generating unused classes
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", noAnnotationKotlinPagedModelProps());

        assertThat(files).doesNotContainKey("UserPage.kt");
        assertThat(files).doesNotContainKey("OrderPage.kt");
        assertThat(files).doesNotContainKey("PetPageAllOf.kt");
    }

    @Test
    public void substituteGenericPagedModel_suppressesPageMetaWhenNoAnnotations() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", noAnnotationKotlinPagedModelProps());

        // PageMetadata is only referenced by OrderPage (which is suppressed) → suppressed
        assertThat(files).doesNotContainKey("PageMetadata.kt");
        // PageMeta is referenced by SearchResult (a non-paged schema) → must be kept
        assertThat(files).containsKey("PageMeta.kt");
    }

    @Test
    public void substituteGenericPagedModel_respectsSchemaMappingForItemType() throws IOException {
        // When the item schema (User) is mapped to an external FQN via schemaMappings,
        // the PagedModel type arg must use the mapped FQN, not the raw schema name.
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator.addSchemaMapping("User", "com.example.external.ExternalUser"));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        // Return type must use the schema-mapped FQN, not the raw schema name
        assertThat(content).contains("PagedModel<com.example.external.ExternalUser>");
        // toModelImport of a dotted name returns the FQN as-is → correct import
        assertThat(content).contains("import com.example.external.ExternalUser");
    }

    @Test
    public void substituteGenericPagedModel_generatesPagedModelSupportingFile() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", commonKotlinPagedModelProps());

        assertThat(files).containsKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_doesNotGeneratePagedModelFileWhenCustomMapping() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        assertThat(files).doesNotContainKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_respectsCustomImportMappingClassName() throws IOException {
        // When the user remaps "PagedModel" to a FQN with a different simple class name,
        // the generated code must use that simple name (not "PagedModel") as the type token
        // and emit the correct import for the custom FQN.
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("MyPagedModel<User>");
        assertThat(content).contains("import com.example.custom.MyPagedModel");
    }

    // substituteGenericPagedModel — spring-declarative-http-interface
    // -------------------------------------------------------------------------

    @Test
    public void substituteGenericPagedModel_springDeclarativeHttpInterface_replacesReturnTypeInOperation() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonDeclarativeHttpInterfacePagedModelProps(),
                new HashMap<>(),
                configurator -> configurator.setLibrary(SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("PagedModel<User>");
    }

    @Test
    public void substituteGenericPagedModel_springDeclarativeHttpInterface_generatesPagedModelSupportingFile() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonDeclarativeHttpInterfacePagedModelProps(),
                new HashMap<>(),
                configurator -> configurator.setLibrary(SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY));

        assertThat(files).containsKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_springDeclarativeHttpInterface_doesNotGeneratePagedModelFileWhenCustomMapping() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonDeclarativeHttpInterfacePagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .setLibrary(SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY)
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        assertThat(files).doesNotContainKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_springDeclarativeHttpInterface_respectsCustomImportMappingClassName() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                commonDeclarativeHttpInterfacePagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .setLibrary(SPRING_DECLARATIVE_HTTP_INTERFACE_LIBRARY)
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("MyPagedModel<User>");
        assertThat(content).contains("import com.example.custom.MyPagedModel");
    }

    /**
     * Common properties for substituteGenericPagedModel tests using spring-declarative-http-interface.
     */
    private Map<String, Object> commonDeclarativeHttpInterfacePagedModelProps() {
        Map<String, Object> props = new HashMap<>();
        props.put(USE_TAGS, "true");
        props.put(USE_SPRING_BOOT3, "true");
        props.put(SUBSTITUTE_GENERIC_PAGED_MODEL, "true");
        props.put(USE_RESPONSE_ENTITY, "false");
        return props;
    }

    // -------------------------------------------------------------------------
    // substituteGenericPagedModel — spring-cloud
    // -------------------------------------------------------------------------

    @Test
    public void substituteGenericPagedModel_springCloud_replacesReturnTypeInOperation() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                springCloudKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator.setLibrary(SPRING_CLOUD_LIBRARY));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("PagedModel<User>");
    }

    @Test
    public void substituteGenericPagedModel_springCloud_generatesPagedModelSupportingFile() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                springCloudKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator.setLibrary(SPRING_CLOUD_LIBRARY));

        assertThat(files).containsKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_springCloud_doesNotGeneratePagedModelFileWhenCustomMapping() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                springCloudKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .setLibrary(SPRING_CLOUD_LIBRARY)
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        assertThat(files).doesNotContainKey("PagedModel.kt");
    }

    @Test
    public void substituteGenericPagedModel_springCloud_respectsCustomImportMappingClassName() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml",
                springCloudKotlinPagedModelProps(),
                new HashMap<>(),
                configurator -> configurator
                        .setLibrary(SPRING_CLOUD_LIBRARY)
                        .addImportMapping("PagedModel", "com.example.custom.MyPagedModel"));

        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("MyPagedModel<User>");
        assertThat(content).contains("import com.example.custom.MyPagedModel");
    }

    /**
     * Common properties for substituteGenericPagedModel tests using spring-cloud.
     */
    private Map<String, Object> springCloudKotlinPagedModelProps() {
        Map<String, Object> props = new HashMap<>();
        props.put(USE_TAGS, "true");
        props.put(SUBSTITUTE_GENERIC_PAGED_MODEL, "true");
        return props;
    }

    // -------------------------------------------------------------------------
    // substituteGenericPagedModel — modelNameSuffix / modelNamePrefix
    // -------------------------------------------------------------------------

    @Test
    public void substituteGenericPagedModel_withModelNameSuffix_replacesReturnType() throws IOException {
        // When modelNameSuffix is set the returnBaseType includes the suffix,
        // so the registry lookup must also use the suffix-applied key.
        Map<String, Object> props = commonKotlinPagedModelProps();
        props.put("modelNameSuffix", "Dto");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", props);

        // listUsers returns UserPage → suffix applied → UserPageDto → replaced with PagedModel<UserDto>
        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("PagedModel<UserDto>");
    }

    @Test
    public void substituteGenericPagedModel_withModelNamePrefix_replacesReturnType() throws IOException {
        // When modelNamePrefix is set the returnBaseType includes the prefix,
        // so the registry lookup must also use the prefix-applied key.
        Map<String, Object> props = commonKotlinPagedModelProps();
        props.put("modelNamePrefix", "My");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", props);

        // listUsers returns UserPage → prefix applied → MyUserPage → replaced with PagedModel<MyUser>
        File userApi = files.get("UserApi.kt");
        assertThat(userApi).isNotNull();
        String content = Files.readString(userApi.toPath());
        assertThat(content).contains("PagedModel<MyUser>");
    }

    @Test
    public void substituteGenericPagedModel_withModelNameSuffix_suppressesPagedSchemasWhenNoAnnotations()
            throws IOException {
        // Verify schema suppression also works correctly under modelNameSuffix
        // (objs keys are suffix-applied, registry keys must match them).
        Map<String, Object> props = noAnnotationKotlinPagedModelProps();
        props.put("modelNameSuffix", "Dto");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/petstore-paged-model.yaml", props);

        assertThat(files).doesNotContainKey("UserPageDto.kt");
        assertThat(files).doesNotContainKey("OrderPageDto.kt");
        assertThat(files).doesNotContainKey("PetPageAllOfDto.kt");
    }


    @Test(description = "oneOf with discriminator generates thin sealed interface with Jackson annotations")
    public void testOneOfWithDiscriminatorGeneratesThinInterface() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(new OpenAPIParser().readLocation("src/test/resources/3_0/kotlin/polymorphism-oneof-discriminator.yaml", null, new ParseOptions()).getOpenAPI())
                        .config(new KotlinSpringServerCodegen() {{
                            setOutputDir(output.getAbsolutePath());
                        }}))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/model";

        // Animal should be a thin sealed interface with Jackson annotations and only the discriminator property
        assertFileContains(Paths.get(outputPath + "/Animal.kt"),
                "sealed interface Animal",
                "@JsonTypeInfo", "property = \"discriminator\"", "visible = true",
                "@JsonSubTypes",
                "JsonSubTypes.Type(value = Bird::class, name = \"BIRD\")",
                "JsonSubTypes.Type(value = Robobird::class, name = \"ROBOBIRD\")",
                "@JsonIgnoreProperties",
                "val discriminator: kotlin.String\n}"
        );
        // Should NOT contain subtype-specific properties (fat interface bug)
        assertFileNotContains(Paths.get(outputPath + "/Animal.kt"), "propertyA", "propertyB", "sameNameProperty");
    }

    @Test(description = "oneOf with discriminator generates subtypes that implement the sealed interface")
    public void testOneOfSubtypesImplementInterface() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(new OpenAPIParser().readLocation("src/test/resources/3_0/kotlin/polymorphism-oneof-discriminator.yaml", null, new ParseOptions()).getOpenAPI())
                        .config(new KotlinSpringServerCodegen() {{
                            setOutputDir(output.getAbsolutePath());
                        }}))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/model";

        // Bird and Robobird implement both oneOf hierarchies; discriminator props have default values
        assertFileContains(Paths.get(outputPath + "/Bird.kt"),
                "data class Bird",
                ") : Animal, AnotherAnimal {",
                "override val discriminator: kotlin.String = \"BIRD\"",
                "override val anotherDiscriminator: kotlin.String = \"ANOTHER_BIRD\""
        );
        // Subtypes must retain their own schema-specific properties
        assertFileContains(Paths.get(outputPath + "/Bird.kt"),
                "val propertyA: kotlin.String?",
                "val sameNameProperty: kotlin.Int?"
        );
        assertFileContains(Paths.get(outputPath + "/Robobird.kt"),
                "data class Robobird",
                ") : Animal, AnotherAnimal {",
                "override val discriminator: kotlin.String = \"ROBOBIRD\"",
                "override val anotherDiscriminator: kotlin.String = \"ANOTHER_ROBOBIRD\""
        );
        assertFileContains(Paths.get(outputPath + "/Robobird.kt"),
                "val propertyB: kotlin.String?",
                "val sameNameProperty: kotlin.String?"
        );
        // AnotherAnimal should also be a sealed interface with Jackson annotations
        assertFileContains(Paths.get(outputPath + "/AnotherAnimal.kt"),
                "sealed interface AnotherAnimal",
                "val anotherDiscriminator: kotlin.String\n}",
                "@JsonTypeInfo", "property = \"another_discriminator\"", "visible = true",
                "@JsonSubTypes",
                "JsonSubTypes.Type(value = Bird::class, name = \"ANOTHER_BIRD\")",
                "JsonSubTypes.Type(value = Robobird::class, name = \"ANOTHER_ROBOBIRD\")",
                "@JsonIgnoreProperties"
        );
        // Sealed interface must not contain subtype-specific properties or snake_case discriminator
        assertFileNotContains(Paths.get(outputPath + "/AnotherAnimal.kt"),
                "val another_discriminator",
                "propertyA", "propertyB", "sameNameProperty"
        );
    }

    @Test(description = "oneOf with discriminator using OpenAPI 3.1 spec generates sealed interface")
    public void testOneOf31SpecWithDiscriminator() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(new OpenAPIParser().readLocation("src/test/resources/3_1/polymorphism-and-discriminator.yaml", null, new ParseOptions()).getOpenAPI())
                        .config(new KotlinSpringServerCodegen() {{
                            setOutputDir(output.getAbsolutePath());
                        }}))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/model";

        assertFileContains(Paths.get(outputPath + "/Pet.kt"),
                "sealed interface Pet",
                "@JsonTypeInfo", "property = \"petType\"", "visible = true",
                "@JsonSubTypes",
                "JsonSubTypes.Type(value = Cat::class, name = \"cat\")",
                "JsonSubTypes.Type(value = Dog::class, name = \"dog\")",
                "@JsonIgnoreProperties",
                "val petType: kotlin.String\n"
        );
        assertFileNotContains(Paths.get(outputPath + "/Pet.kt"), "kotlin.Any", "huntingSkill", "packSize");
        // Discriminator is a constructor param with default value
        assertFileContains(Paths.get(outputPath + "/Cat.kt"),
                "data class Cat",
                ") : Pet {",
                "override val petType: kotlin.String = \"cat\""
        );
        assertFileNotContains(Paths.get(outputPath + "/Cat.kt"), "kotlin.Any");
        assertFileContains(Paths.get(outputPath + "/Dog.kt"),
                "data class Dog",
                ") : Pet {",
                "override val petType: kotlin.String = \"dog\""
        );
        assertFileNotContains(Paths.get(outputPath + "/Dog.kt"), "kotlin.Any");
    }

    @Test(description = "oneOf with $ref enum discriminator resolves property type correctly")
    public void testOneOfRefEnumDiscriminatorResolvesType() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(new OpenAPIParser().readLocation("src/test/resources/3_0/kotlin/polymorphism-oneof-enum-discriminator.yaml", null, new ParseOptions()).getOpenAPI())
                        .config(new KotlinSpringServerCodegen() {{
                            setOutputDir(output.getAbsolutePath());
                        }}))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/model";

        // Vehicle's discriminator should use the $ref enum type, not hardcoded kotlin.String
        assertFileContains(Paths.get(outputPath + "/Vehicle.kt"),
                "sealed interface Vehicle",
                "@JsonTypeInfo", "property = \"vehicleType\"", "visible = true",
                "@JsonSubTypes",
                "JsonSubTypes.Type(value = Car::class, name = \"CAR\")",
                "JsonSubTypes.Type(value = Truck::class, name = \"TRUCK\")",
                "@JsonIgnoreProperties",
                "val vehicleType: VehicleType\n}"
        );
        assertFileNotContains(Paths.get(outputPath + "/Vehicle.kt"), "numDoors", "payloadCapacity");
        // Children should implement Vehicle and have enum discriminator with default value
        assertFileContains(Paths.get(outputPath + "/Car.kt"),
                "data class Car",
                ") : Vehicle {",
                "override val vehicleType: VehicleType = VehicleType.CAR"
        );
        assertFileContains(Paths.get(outputPath + "/Truck.kt"),
                "data class Truck",
                ") : Vehicle {",
                "override val vehicleType: VehicleType = VehicleType.TRUCK"
        );
    }

    @Test(description = "oneOf without discriminator with useDeductionForOneOfInterfaces generates @JsonTypeInfo(DEDUCTION) annotation")
    public void testOneOfDeductionWithoutDiscriminatorGeneratesDeductionAnnotation() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(new OpenAPIParser().readLocation("src/test/resources/3_0/oneof_polymorphism_and_inheritance.yaml", null, new ParseOptions()).getOpenAPI())
                        .config(new KotlinSpringServerCodegen() {{
                            setOutputDir(output.getAbsolutePath());
                            additionalProperties().put(CodegenConstants.USE_DEDUCTION_FOR_ONE_OF_INTERFACES, "true");
                        }}))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/model";

        // Animal has oneOf [Dog, Cat] with NO discriminator → deduction should be applied
        assertFileContains(Paths.get(outputPath + "/Animal.kt"),
                "sealed interface Animal",
                "@JsonTypeInfo(use = JsonTypeInfo.Id.DEDUCTION)",
                "@JsonSubTypes(",
                "JsonSubTypes.Type(value = Dog::class)",
                "JsonSubTypes.Type(value = Cat::class)"
        );

        // Fruit has oneOf [Apple, Banana] WITH a discriminator → must NOT use deduction
        assertFileNotContains(Paths.get(outputPath + "/Fruit.kt"),
                "JsonTypeInfo.Id.DEDUCTION"
        );
        assertFileContains(Paths.get(outputPath + "/Fruit.kt"),
                "sealed interface Fruit",
                "@JsonTypeInfo(use = JsonTypeInfo.Id.NAME"
        );
    }

    @Test
    public void testSealedResponseInterfacesWithDeclarativeHttpInterface() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/kotlin/sealed-response-interfaces.yaml", null, new ParseOptions()).getOpenAPI();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "org.openapitools.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "org.openapitools.api");
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, "spring-declarative-http-interface");
        codegen.additionalProperties().put(USE_SEALED_RESPONSE_INTERFACES, "true");
        codegen.additionalProperties().put(USE_RESPONSE_ENTITY, "true");
        codegen.additionalProperties().put(REACTIVE, "false");
        codegen.additionalProperties().put(USE_FLOW_FOR_ARRAY_RETURN_TYPE, "false");

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        assertFileContains(Paths.get(outputPath + "/src/main/kotlin/org/openapitools/api/DefaultApi.kt"),
                "import org.openapitools.model.CreateUserResponse",
                "import org.openapitools.model.GetUserResponse",
                "fun createUser(",
                "): ResponseEntity<CreateUserResponse>",
                "fun getUser(",
                "): ResponseEntity<GetUserResponse>");
    }

    // useEnumValueInterface tests
    // -------------------------------------------------------------------------

    @Test
    public void useEnumValueInterface_isDisabledByDefault() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml", new HashMap<>());

        assertThat(files).doesNotContainKey("ValuedEnum.kt");
        assertFileNotContains(files.get("OrderStatus.kt").toPath(), ": ValuedEnum<");
    }

    @Test
    public void useEnumValueInterface_generatesInterface() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml",
                Map.of(USE_ENUM_VALUE_INTERFACE, "true"));

        assertThat(files).containsKey("ValuedEnum.kt");
        assertFileContains(files.get("ValuedEnum.kt").toPath(), "interface ValuedEnum<T>");
    }

    @Test
    public void useEnumValueInterface_topLevelEnumImplementsInterface() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml",
                Map.of(USE_ENUM_VALUE_INTERFACE, "true"));

        assertFileContains(files.get("OrderStatus.kt").toPath(),
                ": ValuedEnum<kotlin.String>",
                "override val value",
                "import org.openapitools.configuration.ValuedEnum");
    }

    @Test
    public void useEnumValueInterface_inlineEnumImplementsInterface() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml",
                Map.of(USE_ENUM_VALUE_INTERFACE, "true"));

        assertFileContains(files.get("Order.kt").toPath(),
                ": ValuedEnum<kotlin.String>",
                "override val value",
                "import org.openapitools.configuration.ValuedEnum");
    }

    @Test
    public void useEnumValueInterface_noFileGeneratedWithCustomImportMapping() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml",
                Map.of(USE_ENUM_VALUE_INTERFACE, "true"),
                new HashMap<>(),
                configurator -> configurator
                        .addImportMapping("ValuedEnum", "com.example.custom.ValuedEnum"));

        assertThat(files).doesNotContainKey("ValuedEnum.kt");
    }

    @Test
    public void useEnumValueInterface_customImportMappingUsedInGeneratedCode() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/enum-value-interface.yaml",
                Map.of(USE_ENUM_VALUE_INTERFACE, "true"),
                new HashMap<>(),
                configurator -> configurator
                        .addImportMapping("ValuedEnum", "com.example.custom.ValuedEnum"));

        assertFileContains(files.get("OrderStatus.kt").toPath(),
                ": ValuedEnum<kotlin.String>",
                "import com.example.custom.ValuedEnum");
    }

    // ========== REQUIRED + NULLABLE 4-STATE TESTS ==========

    /**
     * Scenario 1: required=true, nullable=false
     * Expected: non-nullable type, no default value, @JsonProperty(required=true).
     */
    @Test(description = "Scenario 1 – required+non-nullable: strict non-nullable property with no default")
    public void requiredNullable_scenario1_requiredNonNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                new HashMap<>());

        Path modelFile = files.get("TestModel.kt").toPath();
        // Must be non-nullable type (no '?'), must have @JsonProperty(required=true), must have no default
        assertFileContains(modelFile,
                "@get:JsonProperty(\"requiredNonNullable\", required = true) val requiredNonNullable: kotlin.String");
        // Must NOT have a nullable marker or default value
        assertFileNotContains(modelFile, "val requiredNonNullable: kotlin.String?");
        assertFileNotContains(modelFile, "val requiredNonNullable: kotlin.String = ");
    }

    /**
     * Scenario 2: required=true, nullable=true
     * Expected: nullable type, no default value, @JsonProperty(required=true).
     */
    @Test(description = "Scenario 2 – required+nullable: nullable type enforced by Jackson required=true")
    public void requiredNullable_scenario2_requiredNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                new HashMap<>());

        Path modelFile = files.get("TestModel.kt").toPath();
        // Must be nullable type with @JsonProperty(required=true), no default value
        assertFileContains(modelFile,
                "@get:JsonProperty(\"requiredNullable\", required = true) val requiredNullable: kotlin.String?");
        // Must NOT have a default value
        assertFileNotContains(modelFile, "val requiredNullable: kotlin.String? = ");
    }

    /**
     * Scenario 3: required=false, nullable=false, no default, openApiNullable=false (default).
     * Without openApiNullable, use lenient @JsonSetter(nulls = Nulls.SKIP) — silently ignores explicit null.
     * Always emits @JsonInclude(NON_NULL) so null fields are omitted from serialized output.
     */
    @Test(description = "Scenario 3 – optional+non-nullable, no openApiNullable: @JsonSetter(SKIP) + @JsonInclude(NON_NULL)")
    public void requiredNullable_scenario3_optionalNonNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true",
                        CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        String content = Files.readString(modelFile);
        int idx = content.indexOf("val optionalNonNullable:");
        Assert.assertTrue(idx >= 0, "optionalNonNullable property must exist");
        String context = content.substring(Math.max(0, idx - 200), idx);
        Assert.assertTrue(context.contains("@field:JsonInclude(JsonInclude.Include.NON_NULL)"),
                "optionalNonNullable must have @JsonInclude(NON_NULL) to omit null from serialized output");
        Assert.assertTrue(context.contains("@field:JsonSetter(nulls = Nulls.SKIP)"),
                "optionalNonNullable (no openApiNullable) should have @field:JsonSetter(nulls = Nulls.SKIP)");
        Assert.assertFalse(context.contains("@field:JsonSetter(nulls = Nulls.FAIL)"),
                "optionalNonNullable (no openApiNullable) must not have FAIL mode");
        // Must have JsonSetter, Nulls, and JsonInclude imports
        assertFileContains(modelFile,
                "import com.fasterxml.jackson.annotation.JsonInclude",
                "import com.fasterxml.jackson.annotation.JsonSetter",
                "import com.fasterxml.jackson.annotation.Nulls");
        // Must still be nullable type with null default
        assertFileContains(modelFile, "val optionalNonNullable: kotlin.String? = null");
    }

    /**
     * Scenario 3 with openApiNullable=true: required=false, nullable=false, no default.
     * Uses strict @JsonSetter(nulls = Nulls.FAIL) and always emits @JsonInclude(NON_NULL).
     */
    @Test(description = "Scenario 3 – optional+non-nullable with openApiNullable=true: @JsonSetter(FAIL) + @JsonInclude(NON_NULL)")
    public void requiredNullable_scenario3_optionalNonNullable_withOpenApiNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true",
                        CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true",
                        CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        String content = Files.readString(modelFile);
        int idx = content.indexOf("val optionalNonNullable:");
        Assert.assertTrue(idx >= 0, "optionalNonNullable property must exist");
        String context = content.substring(Math.max(0, idx - 200), idx);
        Assert.assertTrue(context.contains("@field:JsonInclude(JsonInclude.Include.NON_NULL)"),
                "optionalNonNullable must have @JsonInclude(NON_NULL) to omit null from serialized output");
        Assert.assertTrue(context.contains("@field:JsonSetter(nulls = Nulls.FAIL)"),
                "optionalNonNullable should have @field:JsonSetter(FAIL) when openApiNullable=true");
        // Must have all three imports
        assertFileContains(modelFile,
                "import com.fasterxml.jackson.annotation.JsonInclude",
                "import com.fasterxml.jackson.annotation.JsonSetter",
                "import com.fasterxml.jackson.annotation.Nulls");
        // Must be nullable type with null default
        assertFileContains(modelFile, "val optionalNonNullable: kotlin.String? = null");
    }

    /**
     * Scenario 3 with a defined default value: required=false, nullable=false, default="defaultValue", openApiNullable=false.
     * Uses SKIP mode and @JsonInclude(NON_NULL) — null fields are omitted, protecting the default.
     */
    @Test(description = "Scenario 3 – optional+non-nullable with default value: @JsonSetter(SKIP) + @JsonInclude(NON_NULL)")
    public void requiredNullable_scenario3_optionalNonNullable_withDefault() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true",
                        CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        String content = Files.readString(modelFile);
        int idx = content.indexOf("val optionalNonNullableWithDefault:");
        Assert.assertTrue(idx >= 0, "optionalNonNullableWithDefault property must exist");
        String context = content.substring(Math.max(0, idx - 300), idx);
        Assert.assertTrue(context.contains("@field:JsonInclude(JsonInclude.Include.NON_NULL)"),
                "optionalNonNullableWithDefault must have @JsonInclude(NON_NULL)");
        Assert.assertTrue(context.contains("@field:JsonSetter(nulls = Nulls.SKIP)"),
                "optionalNonNullableWithDefault should have @field:JsonSetter(nulls = Nulls.SKIP) when openApiNullable=false");
        Assert.assertFalse(context.contains("@field:JsonSetter(nulls = Nulls.FAIL)"),
                "optionalNonNullableWithDefault must not have FAIL mode when openApiNullable=false");
        assertFileContains(modelFile,
                "import com.fasterxml.jackson.annotation.JsonInclude",
                "import com.fasterxml.jackson.annotation.JsonSetter",
                "import com.fasterxml.jackson.annotation.Nulls");
    }

    /**
     * Scenario 4 (openApiNullable=false, default): required=false, nullable=true
     * Without openApiNullable, falls back to nullable type with null default (same as before this feature).
     */
    @Test(description = "Scenario 4 – optional+nullable without openApiNullable: nullable type with null default (legacy fallback)")
    public void requiredNullable_scenario4_optionalNullable_withoutOpenApiNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                new HashMap<>());

        Path modelFile = files.get("TestModel.kt").toPath();
        // Without openApiNullable, should still be Type? = null (legacy behavior preserved)
        assertFileContains(modelFile, "val optionalNullable: kotlin.String? = null");
        // Must NOT have JsonNullable wrapping
        assertFileNotContains(modelFile, "JsonNullable<kotlin.String>");
        // Must NOT have @field:JsonSetter on nullable optional (only non-nullable gets it)
        // (check that the line itself does not have JsonSetter)
        String content = Files.readString(modelFile);
        int idx = content.indexOf("val optionalNullable:");
        Assert.assertTrue(idx >= 0, "optionalNullable property must exist");
        String context = content.substring(Math.max(0, idx - 100), idx);
        Assert.assertFalse(context.contains("@field:JsonSetter"),
                "optionalNullable should not have @field:JsonSetter when nullable=true");
    }

    /**
     * Scenario 4 (openApiNullable=true): required=false, nullable=true
     * Expected: JsonNullable&lt;T&gt; wrapper with JsonNullable.undefined() default.
     */
    @Test(description = "Scenario 4 – optional+nullable with openApiNullable=true: JsonNullable 3-state wrapper")
    public void requiredNullable_scenario4_optionalNullable_withOpenApiNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        // Must use JsonNullable<T> wrapper
        assertFileContains(modelFile, "JsonNullable<kotlin.String>");
        // Must default to JsonNullable.undefined()
        assertFileContains(modelFile, "= JsonNullable.undefined()");
        // Must have JsonNullable import
        assertFileContains(modelFile, "import org.openapitools.jackson.nullable.JsonNullable");
        // Must NOT be a plain nullable type
        assertFileNotContains(modelFile, "val optionalNullable: kotlin.String? = null");
    }

    /**
     * Scenario 3 with Jackson 3 (Spring Boot 4) + openApiNullable=true: optional + non-nullable.
     *
     * @JsonSetter / Nulls imports should come from com.fasterxml.jackson.annotation
     * (Jackson 3.x intentionally kept jackson-annotations at 2.x, same package).
     */
    @Test(description = "Scenario 3 with Jackson 3 + openApiNullable: com.fasterxml.jackson.annotation.JsonSetter + Nulls imports")
    public void requiredNullable_scenario3_optionalNonNullable_withJackson3() throws IOException {
        Map<String, Object> props = new HashMap<>();
        props.put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        props.put(CodegenConstants.OPENAPI_NULLABLE, "true");
        props.put(CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml", props);

        Path modelFile = files.get("TestModel.kt").toPath();
        // Annotation must still be rendered
        assertFileContains(modelFile, "@field:JsonSetter(nulls = Nulls.FAIL)");
        // Imports must come from com.fasterxml.jackson.annotation (Jackson 3.x keeps annotations at 2.x)
        assertFileContains(modelFile,
                "import com.fasterxml.jackson.annotation.JsonSetter",
                "import com.fasterxml.jackson.annotation.Nulls");
        // Must be nullable type with null default
        assertFileContains(modelFile, "val optionalNonNullable: kotlin.String? = null");
    }

    /**
     * Issue #24401: optional+nullable ({@code JsonNullable<T>}) fields must no longer carry
     * {@code @field:JsonInclude(NON_ABSENT)} — the JsonNullable module already governs their inclusion.
     */
    @Test(description = "Issue #24401 – optional+nullable with openApiNullable=true: no @JsonInclude annotation")
    public void requiredNullable_scenario4_optionalNullable_hasNoJsonIncludeAnnotation() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        // NON_ABSENT must no longer be emitted anywhere
        assertFileNotContains(modelFile, "@field:JsonInclude(JsonInclude.Include.NON_ABSENT)");
        // optionalNullable must still be a JsonNullable<T> wrapper
        assertFileContains(modelFile, "JsonNullable<kotlin.String>");
    }

    /**
     * Issue #24401 (safe-but-noisy): with no flags set, kotlin-spring defaults to weak/7.23.0 behavior —
     * NO policy {@code @field:JsonInclude} or {@code @field:JsonSetter(nulls)} annotations are emitted.
     */
    @Test(description = "Issue #24401 – unset flags emit no policy annotations (kotlin-spring)")
    public void jsonInclude_unset_emitsNoPolicyAnnotations() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true"));

        Path modelFile = files.get("TestModel.kt").toPath();
        assertFileNotContains(modelFile, "@field:JsonInclude(");
        assertFileNotContains(modelFile, "@field:JsonSetter(");
    }

    /**
     * Issue #24401: default matrix for kotlin-spring. required fields -> ALWAYS,
     * optional non-nullable -> NON_NULL (default policy), optional nullable -> no annotation.
     */
    @Test(description = "Issue #24401 – default @JsonInclude matrix (kotlin-spring)")
    public void jsonInclude_kotlinMatrix_default() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true"));

        String content = Files.readString(files.get("TestModel.kt").toPath());
        Assert.assertTrue(jsonIncludeBlockFor(content, "requiredNonNullable").contains("@field:JsonInclude(JsonInclude.Include.ALWAYS)"),
                "required non-nullable must be ALWAYS");
        Assert.assertTrue(jsonIncludeBlockFor(content, "requiredNullable").contains("@field:JsonInclude(JsonInclude.Include.ALWAYS)"),
                "required nullable must be ALWAYS");
        Assert.assertTrue(jsonIncludeBlockFor(content, "optionalNonNullable").contains("@field:JsonInclude(JsonInclude.Include.NON_NULL)"),
                "optional non-nullable must be NON_NULL by default");
        Assert.assertFalse(jsonIncludeBlockFor(content, "optionalNullable").contains("@field:JsonInclude"),
                "optional nullable must have no @JsonInclude annotation");
    }

    /**
     * Issue #24401: {@code optionalNonNullPropertyJsonInclude=NONE} omits the annotation on optional
     * non-nullable properties while keeping the required-field protection.
     */
    @Test(description = "Issue #24401 – optionalNonNullPropertyJsonInclude=NONE (kotlin-spring)")
    public void jsonInclude_optionalNonNullPolicy_none() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true",
                        CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE, "NONE"));

        String content = Files.readString(files.get("TestModel.kt").toPath());
        Assert.assertFalse(jsonIncludeBlockFor(content, "optionalNonNullable").contains("@field:JsonInclude"),
                "optional non-nullable must have no annotation when policy is NONE");
        Assert.assertTrue(jsonIncludeBlockFor(content, "requiredNonNullable").contains("@field:JsonInclude(JsonInclude.Include.ALWAYS)"),
                "required-field protection must still be present");
    }

    /**
     * Issue #24401: {@code generateJsonIncludeAnnotations=false} removes ALL policy @JsonInclude
     * annotations, including required-field protection.
     */
    @Test(description = "Issue #24401 – generateJsonIncludeAnnotations=false removes all policy annotations (kotlin-spring)")
    public void jsonInclude_generateJsonIncludeAnnotations_false() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "false"));

        assertFileNotContains(files.get("TestModel.kt").toPath(), "@field:JsonInclude(");
    }

    /**
     * Issue #24401: a manual per-property {@code x-jackson-json-include-policy} vendor extension always
     * overrides the automatic behavior, even when {@code generateJsonIncludeAnnotations=false}.
     */
    @Test(description = "Issue #24401 – manual vendor-extension override wins (kotlin-spring)")
    public void jsonInclude_manualOverride_winsOverGenerateFlag() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_override.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "false"));

        String content = Files.readString(files.get("TestModel.kt").toPath());
        Assert.assertTrue(jsonIncludeBlockFor(content, "overridden").contains("@field:JsonInclude(JsonInclude.Include.NON_EMPTY)"),
                "manual override must be honored even with generateJsonIncludeAnnotations=false");
        Assert.assertFalse(jsonIncludeBlockFor(content, "plain").contains("@field:JsonInclude"),
                "non-overridden field must have no annotation when generateJsonIncludeAnnotations=false");
    }

    /**
     * Returns the source region isolating a single property's annotations, bounded by the previous
     * property's {@code @get:JsonProperty} marker, so @field:JsonInclude checks are property-specific.
     */
    private static String jsonIncludeBlockFor(String content, String propName) {
        int end = content.indexOf("@get:JsonProperty(\"" + propName + "\"");
        Assert.assertTrue(end >= 0, propName + " property must exist");
        int prev = content.lastIndexOf("@get:JsonProperty(", end - 1);
        return content.substring(Math.max(0, prev), end);
    }

    /**
     * Issue #24401: with one property per schema, each generated model must import exactly the Jackson
     * annotations its single property needs. openApiNullable=true so optional nullable uses JsonNullable.
     */
    @Test(description = "Issue #24401 – per-schema import isolation (kotlin-spring)")
    public void jsonInclude_perSchemaImports() throws IOException {
        final String jsonInclude = "import com.fasterxml.jackson.annotation.JsonInclude";
        final String jsonSetter = "import com.fasterxml.jackson.annotation.JsonSetter";
        final String nulls = "import com.fasterxml.jackson.annotation.Nulls";
        final String jsonNullable = "import org.openapitools.jackson.nullable.JsonNullable";

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_per_schema.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true",
                        CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true",
                        CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true"));

        // required non-nullable -> @field:JsonInclude(ALWAYS); no setter/nullable machinery
        Path requiredNonNullable = files.get("RequiredNonNullable.kt").toPath();
        assertFileContains(requiredNonNullable, jsonInclude);
        assertFileNotContains(requiredNonNullable, jsonSetter, nulls, jsonNullable);
        // required nullable -> @field:JsonInclude(ALWAYS)
        Path requiredNullable = files.get("RequiredNullable.kt").toPath();
        assertFileContains(requiredNullable, jsonInclude);
        assertFileNotContains(requiredNullable, jsonSetter, nulls, jsonNullable);
        // optional non-nullable -> @field:JsonInclude(NON_NULL) + @field:JsonSetter(Nulls.FAIL)
        Path optionalNonNullable = files.get("OptionalNonNullable.kt").toPath();
        assertFileContains(optionalNonNullable, jsonInclude, jsonSetter, nulls);
        assertFileNotContains(optionalNonNullable, jsonNullable);
        // optional nullable with openApiNullable=true -> JsonNullable<T>, NO @field:JsonInclude
        Path optionalNullable = files.get("OptionalNullable.kt").toPath();
        assertFileContains(optionalNullable, jsonNullable);
        assertFileNotContains(optionalNullable, jsonInclude, jsonSetter, nulls);
    }

    /**
     * Issue #24401: even with {@code generateJsonIncludeAnnotations=false}, a manual per-property vendor
     * extension must still emit its annotation AND the JsonInclude import must be present.
     */
    @Test(description = "Issue #24401 – manual override emits import even when annotations disabled (kotlin-spring)")
    public void jsonInclude_manualOverride_emitsImport_whenAnnotationsDisabled() throws IOException {
        final String jsonInclude = "import com.fasterxml.jackson.annotation.JsonInclude";

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_per_schema.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "false"));

        Path manualOverride = files.get("ManualOverride.kt").toPath();
        assertFileContains(manualOverride, jsonInclude);
        Assert.assertTrue(jsonIncludeBlockFor(Files.readString(manualOverride), "value")
                        .contains("@field:JsonInclude(JsonInclude.Include.NON_EMPTY)"),
                "manual override must be emitted even when generateJsonIncludeAnnotations=false");
        // A schema without the override must not import JsonInclude when annotations are disabled
        assertFileNotContains(files.get("OptionalNonNullable.kt").toPath(), jsonInclude);
    }

    /**
     * Issue #24401: a forced override on an optional+nullable ({@code JsonNullable<T>}) property must be
     * respected — the annotation is emitted and the JsonInclude import is added.
     */
    @Test(description = "Issue #24401 – forced override on JsonNullable emits annotation and import (kotlin-spring)")
    public void jsonInclude_forcedOverride_onJsonNullable_emitsAnnotationAndImport() throws IOException {
        final String jsonInclude = "import com.fasterxml.jackson.annotation.JsonInclude";
        final String jsonNullable = "import org.openapitools.jackson.nullable.JsonNullable";

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_per_schema.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true"));

        Path forced = files.get("ForcedOnJsonNullable.kt").toPath();
        assertFileContains(forced, jsonInclude, jsonNullable, "JsonNullable<kotlin.String>");
        Assert.assertTrue(jsonIncludeBlockFor(Files.readString(forced), "value")
                        .contains("@field:JsonInclude(JsonInclude.Include.NON_NULL)"),
                "forced override on JsonNullable field must be emitted");
    }

    /**
     * Issue #24401: a manual per-property override of {@code NONE} means "emit no annotation". Neither the
     * {@code @field:JsonInclude} annotation nor its import may be generated, otherwise the output fails to compile.
     */
    @Test(description = "Issue #24401 – manual NONE override emits no annotation or import (kotlin-spring)")
    public void jsonInclude_manualOverride_none_emitsNoAnnotationOrImport() throws IOException {
        final String jsonInclude = "import com.fasterxml.jackson.annotation.JsonInclude";

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_per_schema.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true"));

        Path manualNone = files.get("ManualNone.kt").toPath();
        assertFileNotContains(manualNone, jsonInclude);
        Assert.assertFalse(Files.readString(manualNone).contains("@field:JsonInclude"),
                "manual NONE override must emit no @field:JsonInclude annotation");
    }

    /**
     * Issue #24401: a whitespace-padded {@code NONE} override must be treated identically to a bare
     * {@code NONE} — the sentinel comparison must trim before checking, otherwise it falls through to
     * validation and generation fails for a value that should simply suppress the annotation.
     */
    @Test(description = "Issue #24401 – padded NONE override emits no annotation or import (kotlin-spring)")
    public void jsonInclude_manualOverride_paddedNone_emitsNoAnnotationOrImport() throws IOException {
        final String jsonInclude = "import com.fasterxml.jackson.annotation.JsonInclude";

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_per_schema.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true"));

        Path manualNonePadded = files.get("ManualNonePadded.kt").toPath();
        assertFileNotContains(manualNonePadded, jsonInclude);
        Assert.assertFalse(Files.readString(manualNonePadded).contains("@field:JsonInclude"),
                "padded NONE override must emit no @field:JsonInclude annotation");
    }

    /**
     * Issue #24401: an invalid manual per-property override must fail fast with an actionable error
     * during generation rather than emitting uncompilable Kotlin.
     */
    @Test(description = "Issue #24401 – invalid manual override fails fast (kotlin-spring)")
    public void jsonInclude_manualOverride_invalid_failsWithActionableError() {
        Throwable thrown = Assert.expectThrows(Throwable.class, () -> generateFromContract(
                "src/test/resources/3_0/spring/issue_24401_json_include_invalid_override.yaml",
                Map.of(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, "true")));

        java.io.StringWriter sw = new java.io.StringWriter();
        thrown.printStackTrace(new java.io.PrintWriter(sw));
        String trace = sw.toString();
        Assert.assertTrue(trace.contains("x-jackson-json-include-policy") && trace.contains("NOT_A_REAL_POLICY"),
                "expected an actionable error naming the invalid policy, but got: " + trace);
    }

    /**
     * Without openApiNullable the optional+nullable field is a plain {@code Type?} — no
     * {@code @field:JsonInclude(NON_ABSENT)} should be emitted because {@code JsonNullable} is
     * not used and the legacy nullable-type path doesn't need the annotation.
     */
    @Test(description = "Scenario 4 – optional+nullable without openApiNullable: no @JsonInclude(NON_ABSENT)")
    public void requiredNullable_scenario4_optionalNullable_noNonAbsentWithoutOpenApiNullable() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                new HashMap<>());

        Path modelFile = files.get("TestModel.kt").toPath();
        String content = Files.readString(modelFile);
        int idx = content.indexOf("val optionalNullable:");
        Assert.assertTrue(idx >= 0, "optionalNullable property must exist");
        String context = content.substring(Math.max(0, idx - 200), idx);
        Assert.assertFalse(context.contains("@field:JsonInclude(JsonInclude.Include.NON_ABSENT)"),
                "optionalNullable must NOT have NON_ABSENT when openApiNullable=false (no JsonNullable wrapping)");
    }

    /**
     * Scenario 4 with Jackson 3 (Spring Boot 4) + openApiNullable=true.
     * JsonNullable is in org.openapitools.jackson.nullable regardless of Jackson version
     * (jackson-databind-nullable >= 0.2.10 supports both).
     */
    @Test(description = "Scenario 4 with Jackson 3 + openApiNullable: JsonNullable works with tools.jackson")
    public void requiredNullable_scenario4_optionalNullable_withOpenApiNullable_jackson3() throws IOException {
        Map<String, Object> props = new HashMap<>();
        props.put(KotlinSpringServerCodegen.USE_SPRING_BOOT4, "true");
        props.put(CodegenConstants.OPENAPI_NULLABLE, "true");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml", props);

        Path modelFile = files.get("TestModel.kt").toPath();
        // Must use JsonNullable<T> wrapper
        assertFileContains(modelFile, "JsonNullable<kotlin.String>");
        // Must default to JsonNullable.undefined()
        assertFileContains(modelFile, "= JsonNullable.undefined()");
        // org.openapitools.jackson.nullable package is the same for Jackson 2 and 3
        assertFileContains(modelFile, "import org.openapitools.jackson.nullable.JsonNullable");
        // Must NOT be a plain nullable type
        assertFileNotContains(modelFile, "val optionalNullable: kotlin.String? = null");
    }

    /**
     * Scenario 4 with modelNameSuffix: the generated file is renamed (e.g. TestModelDto.kt)
     * but JsonNullable wrapping must still be applied to the optional+nullable property.
     */
    @Test(description = "Scenario 4 – openApiNullable=true + modelNameSuffix: JsonNullable present in renamed model file")
    public void requiredNullable_scenario4_withModelNameSuffix() throws IOException {
        Map<String, Object> props = new HashMap<>();
        props.put(CodegenConstants.OPENAPI_NULLABLE, "true");
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                props,
                new HashMap<>(),
                configurator -> configurator.setModelNameSuffix("Dto"));

        // File must be renamed
        Assert.assertNotNull(files.get("TestModelDto.kt"), "Expected TestModelDto.kt to be generated");
        Assert.assertNull(files.get("TestModel.kt"), "TestModel.kt must not exist when suffix=Dto");

        Path modelFile = files.get("TestModelDto.kt").toPath();
        assertFileContains(modelFile, "JsonNullable<kotlin.String>");
        assertFileContains(modelFile, "= JsonNullable.undefined()");
        assertFileContains(modelFile, "import org.openapitools.jackson.nullable.JsonNullable");
    }

    /**
     * Scenario 4 with modelNamePrefix: the generated file is renamed (e.g. ApiTestModel.kt)
     * but JsonNullable wrapping must still be applied.
     */
    @Test(description = "Scenario 4 – openApiNullable=true + modelNamePrefix: JsonNullable present in renamed model file")
    public void requiredNullable_scenario4_withModelNamePrefix() throws IOException {
        Map<String, Object> props = new HashMap<>();
        props.put(CodegenConstants.OPENAPI_NULLABLE, "true");
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-4-states.yaml",
                props,
                new HashMap<>(),
                configurator -> configurator.setModelNamePrefix("Api"));

        Assert.assertNotNull(files.get("ApiTestModel.kt"), "Expected ApiTestModel.kt to be generated");
        Assert.assertNull(files.get("TestModel.kt"), "TestModel.kt must not exist when prefix=Api");

        Path modelFile = files.get("ApiTestModel.kt").toPath();
        assertFileContains(modelFile, "JsonNullable<kotlin.String>");
        assertFileContains(modelFile, "= JsonNullable.undefined()");
        assertFileContains(modelFile, "import org.openapitools.jackson.nullable.JsonNullable");
    }

    /**
     * Scenario 4 with schemaMapping: when the type of an optional+nullable property is a $ref
     * that is schema-mapped to an external class, JsonNullable must wrap the mapped type.
     */
    @Test(description = "Scenario 4 – openApiNullable=true + schemaMapping: JsonNullable wraps the mapped external type")
    public void requiredNullable_scenario4_withSchemaMapping() throws IOException {
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/required-nullable-ref-type.yaml",
                Map.of(CodegenConstants.OPENAPI_NULLABLE, "true"),
                new HashMap<>(),
                configurator -> configurator.setSchemaMappings(
                        Map.of("RefType", "com.example.ExternalType")));

        Path modelFile = files.get("TestModel.kt").toPath();
        // The optional nullable ref property must be wrapped with the mapped external type
        assertFileContains(modelFile, "JsonNullable<com.example.ExternalType>");
        assertFileContains(modelFile, "= JsonNullable.undefined()");
        assertFileContains(modelFile, "import org.openapitools.jackson.nullable.JsonNullable");
    }

    @Test
    public void suspendFunctionsInterfaceOnly() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.SUSPEND_FUNCTIONS, true,
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
                                "suspend fun deletePet(",
                                "suspend fun getPetById("),
                        root.resolve("src/main/kotlin/org/openapitools/api/UserApi.kt"), List.of(
                                "suspend fun logoutUser()"),
                        root.resolve("src/main/kotlin/org/openapitools/api/StoreApi.kt"), List.of(
                                "suspend fun getInventory()")
                )
        );
    }

    @Test
    public void suspendFunctionsWithDelegatePattern() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.SUSPEND_FUNCTIONS, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
                KotlinSpringServerCodegen.DELEGATE_PATTERN, true,
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
                                "suspend fun deletePet(",
                                "suspend fun getPetById("),
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiDelegate.kt"), List.of(
                                "suspend fun deletePet(",
                                "suspend fun getPetById(")
                )
        );
    }

    @Test
    public void suspendFunctionsDefaultsToFalse() throws Exception {
        Path root = generateApiSources(Map.of(
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
                                "fun deletePet(",
                                "fun getPetById(")
                )
        );
        // Verify no suspend keyword appears
        Path petApiPath = root.resolve("src/main/kotlin/org/openapitools/api/PetApi.kt");
        String content = new String(Files.readAllBytes(petApiPath), java.nio.charset.StandardCharsets.UTF_8);
        Assert.assertFalse(content.contains("suspend fun"),
                "suspend should not be present when suspendFunctions is not enabled");
    }

    @Test
    public void suspendFunctionsWithServiceInterface() throws Exception {
        Path root = generateApiSources(Map.of(
                KotlinSpringServerCodegen.SUSPEND_FUNCTIONS, true,
                KotlinSpringServerCodegen.SERVICE_INTERFACE, true,
                KotlinSpringServerCodegen.DOCUMENTATION_PROVIDER, "none",
                KotlinSpringServerCodegen.ANNOTATION_LIBRARY, "none",
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
                        root.resolve("src/main/kotlin/org/openapitools/api/PetApiService.kt"), List.of(
                                "suspend fun deletePet(",
                                "suspend fun getPetById(")
                )
        );
    }


    @Test
    public void schemaMappingWithNullableAllOfRendersNullableKotlinProperty() throws IOException {
        // When a schema is substituted via schemaMapping and a property wraps it with
        // "nullable: true + allOf: [$ref]", the Kotlin Spring generator must render the
        // property as MappedType? (nullable with the FQN from the mapping).
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/schema-mapping-nullable-allof.yaml",
                new HashMap<>(),
                new HashMap<>(),
                configurator -> configurator.addSchemaMapping("ExternalModel", "com.example.ExternalModel"));

        File myObjectFile = files.get("MyObject.kt");
        assertThat(myObjectFile).isNotNull();
        String content = Files.readString(myObjectFile.toPath());
        assertThat(content).contains("com.example.ExternalModel?");
    }

    @Test(description = "nameMappings: @param:JsonProperty must use the original JSON field name for deserialization")
    public void paramJsonPropertyAnnotationWithNameMappings() throws IOException {
        // When a property is renamed via nameMappings, @param:JsonProperty must carry the
        // original JSON field name so Jackson can deserialize from the correct JSON key.
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/param-json-property.yaml",
                new HashMap<>(),
                new HashMap<>(),
                configurator -> configurator.addNameMapping("snake_case_value", "mappedValue")
        );

        File itemFile = files.get("Item.kt");
        assertThat(itemFile).isNotNull();
        assertFileContains(
                itemFile.toPath(),
                "@param:JsonProperty(\"snake_case_value\")\n    @get:JsonProperty(\"snake_case_value\", required = true) val mappedValue"
        );
    }

    @Test(description = "auto-renamed digit-starting property: @param:JsonProperty must use the original JSON field name")
    public void paramJsonPropertyAnnotationWithDigitStartingPropertyName() throws IOException {
        // When a property name starts with a digit, the Kotlin codegen wraps it in backticks
        // (e.g. "2nd_field" -> `2ndField`). @param:JsonProperty must still carry the original
        // JSON field name so that Jackson can deserialize it correctly.
        Map<String, File> files = generateFromContract(
                "src/test/resources/3_0/kotlin/param-json-property.yaml"
        );

        File itemFile = files.get("Item.kt");
        assertThat(itemFile).isNotNull();
        assertFileContains(
                itemFile.toPath(),
                "@param:JsonProperty(\"2nd_field\")\n    @get:JsonProperty(\"2nd_field\") val `2ndField`"
        );
    }


    /**
     * Regression test for https://github.com/OpenAPITools/openapi-generator/issues/24139
     * A property that $ref's an OAS 3.1 schema with type:[object,"null"] is nullable and must
     * NOT receive @field:JsonSetter(nulls = Nulls.FAIL).
     */
    @Test(description = "issue 24139: nullable $ref (type:[object,null]) must not get @JsonSetter(nulls = Nulls.FAIL)")
    public void testIssue24139NullableRefNoJsonSetterNullsFail() throws IOException {
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put("useBeanValidation", true);
        additionalProperties.put("openApiNullable", "true");
        additionalProperties.put(CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, "true");

        Map<String, File> files = generateFromContract(
                "src/test/resources/3_1/issue_24139.yaml",
                additionalProperties
        );

        File itemFile = files.get("Item.kt");
        assertThat(itemFile).isNotNull();

        // nestedNullable: $ref to NestedNullable (type:[object,"null"]) — nullable, no @JsonSetter(nulls = Nulls.FAIL)
        assertFileNotContains(itemFile.toPath(), "nestedNullable: NestedNullable");
        // The field must NOT have @JsonSetter(nulls = Nulls.FAIL) because the referenced schema is nullable
        String content = org.apache.commons.io.FileUtils.readFileToString(itemFile, StandardCharsets.UTF_8);
        // Extract the nestedNullable field block and verify annotation absence
        Assert.assertFalse(
                content.contains("@field:JsonSetter(nulls = Nulls.FAIL)\n    @param:JsonProperty(\"nestedNullable\")") ||
                content.contains("@field:JsonSetter(nulls = Nulls.FAIL)\n    @get:JsonProperty(\"nestedNullable\")"),
                "nestedNullable ($ref to nullable schema) must not have @JsonSetter(nulls = Nulls.FAIL)"
        );
    }
}
