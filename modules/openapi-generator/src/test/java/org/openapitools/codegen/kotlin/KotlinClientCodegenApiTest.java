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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

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

    @DataProvider(name = "librariesWithDateQueryHelper")
    public static Object[][] librariesWithDateQueryHelper() {
        return new Object[][]{
                {ClientLibrary.JVM_OKHTTP4},
                {ClientLibrary.JVM_SPRING_WEBCLIENT},
                {ClientLibrary.JVM_SPRING_RESTCLIENT},
                {ClientLibrary.JVM_VERTX}
        };
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

    @Test
    public void testRetrofit2DeleteWithBodyUsesHttpAnnotation_25454() throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/issue25454-delete-with-body.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_RETROFIT2);

        ClientOptInput input = createClientOptInput(openAPI, codegen);

        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(input).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(defaultApi.toPath()).stream().map(String::trim).collect(Collectors.toList());

        // Retrofit rejects @Body, @FormUrlEncoded and @Multipart on @DELETE, so these need @HTTP with hasBody
        assertFileContainsLine(lines, "@HTTP(method = \"DELETE\", path = \"items\", hasBody = true)");
        assertFileContainsLine(lines, "@HTTP(method = \"DELETE\", path = \"items/form\", hasBody = true)");
        // operations without a body, and methods that allow a body, keep the short annotation
        assertFileContainsLine(lines, "@DELETE(\"items/{id}\")");
        assertFileContainsLine(lines, "@POST(\"items\")");
    }

    @Test
    public void testOptionalParamsHaveDefaultNullJvmKtor() throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/petstore.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_KTOR);

        ClientOptInput input = createClientOptInput(openAPI, codegen);

        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(input).generate();
        File petApi = files.stream().filter(file -> file.getName().equals("PetApi.kt")).findAny().orElseThrow();

        assertFileContains(petApi.toPath(), "apiKey: kotlin.String? = null");
    }

    @Test
    public void testEnumDefaultForReferencedSchemaParameterJvmOkhttp4() throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/enum-default-query.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_OKHTTP4);
        codegen.additionalProperties().put("enumPropertyNaming", "UPPERCASE");

        ClientOptInput input = createClientOptInput(openAPI, codegen);

        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(input).generate();
        File statusApi = files.stream().filter(file -> file.getName().equals("StatusApi.kt")).findAny().orElseThrow();

        assertFileContains(statusApi.toPath(), "state: PetStatus? = PetStatus.AVAILABLE");
    }

    @DataProvider(name = "librariesWithPlainInlineEnumParams")
    public static Object[][] librariesWithPlainInlineEnumParams() {
        return new Object[][]{
                {ClientLibrary.JVM_KTOR},
                {ClientLibrary.JVM_VOLLEY}
        };
    }

    @Test(dataProvider = "librariesWithPlainInlineEnumParams")
    public void testInlineEnumArrayDefaultUsesItemValues_24851(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/issue24851-enum-array-default-query.yaml");

        KotlinClientCodegen codegen = createCodegen(library);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();

        assertFileContains(defaultApi.toPath(), "colors: kotlin.collections.Set<kotlin.String>? = setOf(\"red\",\"blue\")");
        assertFileContains(defaultApi.toPath(), "sizes: kotlin.collections.List<kotlin.Int>? = arrayListOf(2)");
        assertFileContains(defaultApi.toPath(), "refColors: kotlin.collections.List<Color>? = arrayListOf(Color.RED)");
    }

    @Test(dataProvider = "clientLibraries")
    void testEnumReservedDefaultNotHtmlEscaped(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/enum-default-query-reserved-word.json");
        KotlinClientCodegen codegen = createCodegen(library);
        ClientOptInput input = createClientOptInput(openAPI, codegen);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(input).generate();
        File documentApiFile = files.stream().filter(file -> file.getName().equals("DocumentApi.kt")).findAny().orElseThrow();

        String documentApiContents = Files.readString(documentApiFile.toPath());
        if (!documentApiContents.contains("enum class")) {
            return;
        }

        String expectedEnumName = "DispositionDocumentDownload";
        if (!documentApiContents.contains("enum class " + expectedEnumName)) {
            Assert.fail("Kotlin client library " + library.getLibraryName() + " generated enum class name for an operation parameter has changed. Please update the 'expectedEnumName' in this test to match the new name.");
        }

        assertFileContains(documentApiFile.toPath(), "disposition: " + expectedEnumName + "? = DispositionDocumentDownload.`inline`");
    }

    @Test
    public void testJvmOkHttp4ApiClientUsesExplicitDateTypeArgumentsForQuerySerialization() throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/petstore.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_OKHTTP4);
        String outputPath = codegen.getOutputDir().replace('\\', '/');

        DefaultGenerator generator = new DefaultGenerator();
        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        generator.opts(createClientOptInput(openAPI, codegen)).generate();

        String apiClientPath = outputPath + "/src/main/kotlin/org/openapitools/client/infrastructure/ApiClient.kt";
        assertFileContains(Paths.get(apiClientPath), "is OffsetDateTime -> parseDateToQueryString<OffsetDateTime>(value)");
        assertFileContains(Paths.get(apiClientPath), "is OffsetTime -> parseDateToQueryString<OffsetTime>(value)");
        assertFileContains(Paths.get(apiClientPath), "is LocalDateTime -> parseDateToQueryString<LocalDateTime>(value)");
        assertFileContains(Paths.get(apiClientPath), "is LocalDate -> parseDateToQueryString<LocalDate>(value)");
        assertFileContains(Paths.get(apiClientPath), "is LocalTime -> parseDateToQueryString<LocalTime>(value)");
        assertFileNotContains(Paths.get(apiClientPath), "is OffsetDateTime -> parseDateToQueryString(value)");
    }

    @Test(dataProvider = "librariesWithDateQueryHelper")
    public void testGeneratedApisUseExplicitDateTypeArgumentsForQuerySerialization(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/echo_api.yaml");

        KotlinClientCodegen codegen = createCodegen(library);
        DefaultGenerator generator = new DefaultGenerator();

        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File queryApi = files.stream().filter(file -> file.getName().equals("QueryApi.kt")).findAny().orElseThrow();

        assertFileContains(queryApi.toPath(), "parseDateToQueryString<kotlin.time.Instant>(");
        assertFileContains(queryApi.toPath(), "parseDateToQueryString<kotlinx.datetime.LocalDate>(");
        assertFileNotContains(queryApi.toPath(), "parseDateToQueryString(datetimeQuery)");
        assertFileNotContains(queryApi.toPath(), "parseDateToQueryString(dateQuery)");
        assertFileNotContains(queryApi.toPath(), "parseDateToQueryString(it)");
    }

    @Test
    public void testJvmKtorQueryParamWithTypeObject() throws IOException {
        OpenAPI openAPI = readOpenAPI("3_0/kotlin/jvm-ktor-type-object-query.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_KTOR);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();

        assertFileContains(defaultApi.toPath(), "mapFormExplode?.forEach { (key, value) -> localVariableQuery[key]");
        assertFileContains(defaultApi.toPath(), "mapFormNoexplode?.takeIf");
        assertFileContains(defaultApi.toPath(), "localVariableQuery[\"map_deep[$key]\"]");

        assertFileContains(defaultApi.toPath(), "modelFormExplode?.a?.let { localVariableQuery[\"a\"]");
        assertFileContains(defaultApi.toPath(), "modelFormNoexplode?.let { _model -> listOfNotNull(_model.a?.let { \"a,$it\" }, _model.b?.let { \"b,$it\" })");
        assertFileContains(defaultApi.toPath(), "localVariableQuery[\"model_deep[a]\"]");

        assertFileNotContains(defaultApi.toPath(), "mapDeep?.apply {");
    }

    @Test(description = "Verify a form style, exploded map query parameter goes on the wire one entry per parameter")
    public void testExplodedObjectQueryParameterJvmOkhttp() throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/exploded-object-query-param.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_OKHTTP4);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();

        String content = new String(Files.readAllBytes(defaultApi.toPath()), StandardCharsets.UTF_8);

        // form style with explode - the default - puts every entry on the wire under its own
        // property name. Serializing the whole map with toString() is what used to happen.
        assertFileContains(defaultApi.toPath(), "(filter as? kotlin.collections.Map<*, *>)?.forEach { (key, value) ->");
        assertFileNotContains(defaultApi.toPath(), "put(\"filter\", listOf(filter.toString()))");

        // a declared map behaves the same way
        assertFileContains(defaultApi.toPath(), "(typedFilter as? kotlin.collections.Map<*, *>)?.forEach { (key, value) ->");
        assertFileNotContains(defaultApi.toPath(), "put(\"typedFilter\"");

        // a null entry is left out rather than sent as "null", a collection repeats the key
        // once per element rather than going out as its toString(), and an entry is appended
        // rather than replacing a query parameter of the same name
        assertFileContains(defaultApi.toPath(),
                "is kotlin.collections.Iterable<*> -> value.toList()",
                "}.filterNotNull().map { parameterToString(it) }",
                "put(name, getOrElse(name) { emptyList() } + values)");

        // deepObject and form without explode both keep a single parameter
        assertFileContains(defaultApi.toPath(),
                "put(\"deepFilter\", listOf(deepFilter.toString()))",
                "put(\"flatFilter\", listOf(flatFilter.toString()))");

        // the exploded entries are added after every declared parameter, so a declared
        // parameter's put cannot overwrite an entry that happens to share its name
        Assert.assertTrue(content.indexOf("(filter as? kotlin.collections.Map") > content.indexOf("put(\"flatFilter\""),
                "exploded entries must be added after the declared query parameters");
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

    @DataProvider(name = "librariesWithDollarWireNames")
    public static Object[][] librariesWithDollarWireNames() {
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

    // Wire names containing `$` (OData-style `$filter`/`$top`) must be emitted
    // as escaped Kotlin string literals (`"\$filter"`); unescaped, Kotlin
    // string interpolation turns the wire key into the same-named parameter's
    // value — or fails compilation outright when no such variable exists
    // (`"$top"`). The same applies to `{{{path}}}` segments (`/odata/$count`),
    // path-param placeholders (`{$id}`), header names, deepObject keys,
    // multipart field names and apiKey names. Assertions stay within the
    // lines this change actually escapes: vertx keeps master's header and
    // form emission, and the retrofit2 exploded-property argument keeps its
    // raw baseName identifier (only the @Query wire string is escaped).
    @Test(dataProvider = "librariesWithDollarWireNames")
    void testDollarWireNamesAreEscaped(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-dollar-wire-name.yaml");

        KotlinClientCodegen codegen = createCodegen(library);
        String outputPath = codegen.getOutputDir().replace('\\', '/');
        ClientOptInput input = createClientOptInput(openAPI, codegen);
        DefaultGenerator generator = new DefaultGenerator();
        // ktor/multiplatform/retrofit2/okhttp4 put the apiKey name inside
        // ApiClient.kt, a supporting file; the other libraries don't emit it
        // at all (vertx wires the key in the api file itself)
        boolean apiKeyInApiClient = library == ClientLibrary.JVM_KTOR
                || library == ClientLibrary.MULTIPLATFORM
                || library == ClientLibrary.JVM_RETROFIT2
                || library == ClientLibrary.JVM_OKHTTP4;
        if (apiKeyInApiClient) {
            // keep supporting files for ApiClient.kt; disabling models
            // without enabling apis would switch api generation off too
            generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_TESTS, "false");
            generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        } else {
            enableOnlyApiGeneration(generator);
        }
        generator.opts(input).generate();

        Path apiFile = Paths.get(outputPath + "/src/" + library.getSourceRoot() + "/org/openapitools/client/apis/DefaultApi.kt");
        String api = new String(Files.readAllBytes(apiFile), StandardCharsets.UTF_8);
        // every library escapes the literal path and the {param} placeholder
        assertFileContains(apiFile, "odata/\\$count", "\\$id", "dollarId");
        // "$top" unescaped could only come from the wire key — no `top`
        // variable exists in scope, so it cannot be a legitimate interpolation
        // ("$filter" appears legitimately as the `filter` param's value-side
        // interpolation). The KDoc comment shows the path without a closing
        // quote (`GET /odata/$count`), so `odata/$count"` means an unescaped
        // string literal.
        assertFileNotContains(apiFile, "\"$top\"", "odata/$count\"");

        switch (library) {
            case JVM_KTOR:
                assertFileContains(apiFile,
                        "localVariableQuery[\"\\$filter\"]",
                        "localVariableQuery[\"\\$top\"]",
                        "localVariableQuery[\"\\$a\"]",
                        "localVariableQuery[\"\\$dobj[\\$f]\"]",
                        "localVariableHeaders[\"\\$x-head\"]",
                        "append(\"\\$fprop\"");
                break;
            case JVM_OKHTTP4:
                assertFileContains(apiFile,
                        "put(\"\\$filter\"",
                        "put(\"\\$top\"",
                        "put(\"\\$a\"",
                        "[\\$f]",
                        "localVariableHeaders[\"\\$x-head\"]");
                break;
            case JVM_RETROFIT2:
                assertFileContains(apiFile,
                        "@Query(\"\\$filter\")",
                        "@Query(\"\\$top\")",
                        "@Query(\"\\$a\")",
                        "@Header(\"\\$x-head\")",
                        "@Path(\"\\$id\")",
                        "@Part(\"\\$fprop\")");
                break;
            case MULTIPLATFORM:
                assertFileContains(apiFile,
                        "localVariableQuery[\"\\$filter\"]",
                        "localVariableQuery[\"\\$top\"]",
                        "localVariableQuery[\"\\$dobj\"]",
                        "localVariableHeaders[\"\\$x-head\"]",
                        "append(\"\\$fprop\"");
                break;
            case JVM_VERTX:
                assertFileContains(apiFile,
                        "add(\"\\$filter\"",
                        "add(\"\\$top\"",
                        "add(\"\\$dobj\"",
                        "apiKey[\"\\$api_key\"]",
                        "putHeader(\"\\$api_key\"");
                break;
            case JVM_VOLLEY:
                assertFileContains(apiFile,
                        "\"\\$filter\" to",
                        "\"\\$top\" to",
                        "\"\\$x-head\" to",
                        "\"\\$dobj\" to",
                        "\"\\$fprop\" to");
                break;
            case JVM_SPRING_WEBCLIENT:
            case JVM_SPRING_RESTCLIENT:
                assertFileContains(apiFile,
                        "put(\"\\$filter\"",
                        "put(\"\\$top\"",
                        "localVariableHeaders[\"\\$x-head\"]",
                        "\"\\$fprop\" to PartConfig");
                break;
            default:
                Assert.fail("unexpected library " + library);
        }

        // retrofit2 expands the exploded object's properties into @Query args,
        // so the `opts` parameter itself does not appear in its api file
        if (library != ClientLibrary.JVM_RETROFIT2) {
            Assert.assertTrue(api.contains("opts"),
                    library + ": object query param must still be emitted");
        }

        if (apiKeyInApiClient) {
            Path apiClient = Paths.get(outputPath + "/src/" + library.getSourceRoot()
                    + "/org/openapitools/client/infrastructure/ApiClient.kt");
            assertFileContains(apiClient, "\\$api_key");
        }
    }
}
