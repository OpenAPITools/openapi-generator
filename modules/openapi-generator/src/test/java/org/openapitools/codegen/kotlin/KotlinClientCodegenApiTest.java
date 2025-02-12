package org.openapitools.codegen.kotlin;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.jetbrains.annotations.NotNull;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;

public class KotlinClientCodegenApiTest {

    @DataProvider(name = "clientLibraries")
    public Object[][] pathResponses() {
        return new Object[][]{
                {ClientLibrary.JVM_KTOR},
                {ClientLibrary.JVM_OKHTTP4},
                {ClientLibrary.JVM_SPRING_WEBCLIENT},
                {ClientLibrary.JVM_SPRING_RESTCLIENT},
                {ClientLibrary.JVM_RETROFIT2},
                {ClientLibrary.MULTIPLATFORM},
                {ClientLibrary.JVM_VOLLEY},
                {ClientLibrary.JVM_VERTX}
        };
    }

    @Test(dataProvider = "clientLibraries")
    void testPathVariableIsNotEscaped_19930(ClientLibrary library) throws IOException {

        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/issue19930-path-escaping.json");

        KotlinClientCodegen codegen = createCodegen(library);

        String outputPath = codegen.getOutputDir().replace('\\', '/');
        ClientOptInput input = createClientOptInput(openAPI, codegen);

        DefaultGenerator generator = new DefaultGenerator();

        enableOnlyApiGeneration(generator);

        generator.opts(input).generate();

        System.out.println(outputPath);

        assertFileContains(Paths.get(outputPath + "/src/" + library.getSourceRoot() + "/org/openapitools/client/apis/ArticleApi.kt"), "article('{Id}')");
    }

    @DataProvider(name = "useResponseAsReturnType")
    public static Object[][] useResponseAsReturnTypeTestData() {
        return new Object[][]{
                {null, "Response<Pet>", ": Response<Unit>"},
                {true, "Response<Pet>", ": Response<Unit>"},
                {false, "Pet", ""},
                {"false", "Pet", ""}};
    }

    @Test(dataProvider = "useResponseAsReturnType")
    public void testUseResponseAsReturnType(Object useResponseAsReturnType, String expectedResponse, String expectedUnitResponse) throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/petstore.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_RETROFIT2);
        codegen.additionalProperties().put(KotlinClientCodegen.USE_COROUTINES, "true");
        if (useResponseAsReturnType != null) {
            codegen.additionalProperties().put(KotlinClientCodegen.USE_RESPONSE_AS_RETURN_TYPE, useResponseAsReturnType);
        }

        ClientOptInput input = createClientOptInput(openAPI, codegen);

        DefaultGenerator generator = new DefaultGenerator();

        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(input).generate();
        File petApi = files.stream().filter(file -> file.getName().equals("PetApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(petApi.toPath()).stream().map(String::trim).collect(Collectors.toList());
        assertFileContainsLine(lines, "suspend fun addPet(@Body pet: Pet): " + expectedResponse);
        assertFileContainsLine(lines, "suspend fun deletePet(@Path(\"petId\") petId: kotlin.Long, @Header(\"api_key\") apiKey: kotlin.String? = null)" + expectedUnitResponse);
    }

    private static void assertFileContainsLine(List<String> lines, String line) {
        Assert.assertListContains(lines, s -> s.equals(line), line);
    }

    private static void enableOnlyApiGeneration(DefaultGenerator generator) {
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
    }

    @NotNull
    private static ClientOptInput createClientOptInput(OpenAPI openAPI, KotlinClientCodegen codegen) {
        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);
        return input;
    }

    private static OpenAPI readOpenAPI(String url) {
        return new OpenAPIParser()
                .readLocation(url, null, new ParseOptions()).getOpenAPI();
    }

    private KotlinClientCodegen createCodegen(ClientLibrary library) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setLibrary(library.getLibraryName());
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setSerializationLibrary(library.getSerializationLibrary());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");
        codegen.additionalProperties().put(KotlinClientCodegen.USE_SPRING_BOOT3, "true");
        codegen.additionalProperties().put(KotlinClientCodegen.DATE_LIBRARY, "kotlinx-datetime");
        return codegen;
    }
}
