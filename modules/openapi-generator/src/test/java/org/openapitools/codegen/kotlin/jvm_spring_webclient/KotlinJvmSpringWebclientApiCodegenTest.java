package org.openapitools.codegen.kotlin.jvm_spring_webclient;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.kotlin.assertions.KotlinFileAssert;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.kotlin.assertions.SecondaryConstructorAssert.assertThat;

public class KotlinJvmSpringWebclientApiCodegenTest {

    @Test
    public void testBaseUrlAndBasePathWithoutHostConstantsAreGenerated() throws IOException {
        final Map<String, File> files = generateFromOpenApiFile("src/test/resources/3_0/base-url.yaml");
        KotlinFileAssert.assertThat(files.get("ApiClient.kt"))
                .assertClass("ApiClient")
                .assertCompanion()
                .assertProperty("BASE_URL")
                .isConst()
                .hasInitializer("\"http://api.example.com/v1\"")
                .toCompanion()
                .assertProperty("BASE_PATH_WITHOUT_HOST")
                .isConst()
                .hasInitializer("\"/v1\"");
    }

    @Test
    public void testCreateUrlWithBasePathIsGenerated() throws IOException {
        final Map<String, File> files = generateFromOpenApiFile("src/test/resources/3_0/base-url.yaml");
        KotlinFileAssert.assertThat(files.get("ApiClient.kt"))
                .assertFunction("createUrlWithBasePath")
                .hasReturnType("String")
                .assertParameter("baseUrl")
                .hasType("String")
                .toFunction()
                .assertParameter("basePath")
                .hasType("String")
                .hasDefaultValue("ApiClient.BASE_PATH_WITHOUT_HOST");
    }

    @Test
    public void testConstructorHasBaseUrlAsDefaultParameter() throws IOException {
        final Map<String, File> files = generateFromOpenApiFile("src/test/resources/3_0/base-url.yaml");
        KotlinFileAssert.assertThat(files.get("HelloApi.kt"))
                .assertClass("HelloApi")
                .assertAnySecondaryConstructorSatisfies(sc ->
                    assertThat(sc)
                            .assertParameter("baseUrl")
                            .hasType("String")
                            .hasDefaultValue("ApiClient.BASE_URL")
                );
    }

    @Test
    public void testCreateWithBasePathIsGenerated() throws IOException {
        final Map<String, File> files = generateFromOpenApiFile("src/test/resources/3_0/base-url.yaml");
        KotlinFileAssert.assertThat(files.get("HelloApi.kt"))
                .assertClass("HelloApi")
                .assertCompanion()
                .assertFunction("createWithBasePath")
                .hasReturnType("HelloApi")
                .assertParameter("basePath")
                .hasType("String")
                .hasDefaultValue("ApiClient.BASE_PATH_WITHOUT_HOST");
    }

    private Map<String, File> generateFromOpenApiFile(final String openApiFileUrl) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setLibrary("jvm-spring-webclient");
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setSerializationLibrary("jackson");

        final OpenAPI openAPI = new OpenAPIParser().readLocation(openApiFileUrl, null, new ParseOptions()).getOpenAPI();

        final ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = createApiOnlyGenerator();
        return generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));
    }

    private DefaultGenerator createApiOnlyGenerator() {
        final DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        return generator;
    }
}
