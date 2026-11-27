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

    @DataProvider(name = "librariesWithDeepObjectWireKeys")
    public static Object[][] librariesWithDeepObjectWireKeys() {
        return new Object[][]{
                {ClientLibrary.JVM_KTOR},
                {ClientLibrary.JVM_OKHTTP4}
        };
    }

    // Two deepObject query parameters share the property instances of the same
    // $ref'd schema (fromProperty cache). The per-parameter wire-key prefix must
    // stay parameter-local: filter-a[...] must not be overwritten by filter-b's
    // propagation. jvm-okhttp prefixes keys with the spec baseName (via
    // x-kotlin-param-base-name), not the camelized paramName.
    @Test(dataProvider = "librariesWithDeepObjectWireKeys")
    public void testDeepObjectParamsKeepOwnWireKeyPrefix(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-deepobject-shared-vars.yaml");

        KotlinClientCodegen codegen = createCodegen(library);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File searchApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();

        // the wire key must be paired with the value of the parameter it names:
        // filter-a[status] must read filterA's property, filter-b[status] filterB's
        if (library == ClientLibrary.JVM_KTOR) {
            assertFileContains(searchApi.toPath(),
                    "filterA?.status?.let { localVariableQuery[\"filter-a[status]\"] = listOf(\"$it\") }",
                    "filterB?.status?.let { localVariableQuery[\"filter-b[status]\"] = listOf(\"$it\") }");
        } else {
            assertFileContains(searchApi.toPath(),
                    "put(\"filter-a[status]\", listOf(filterA.status.toString()))",
                    "put(\"filter-b[status]\", listOf(filterB.status.toString()))");
        }
    }

    // clientLibraries plus jvm-microprofile, kept separate since the clientLibraries tests do not cover microprofile
    @DataProvider(name = "allClientLibraries")
    public static Object[][] allClientLibraries() {
        return new Object[][]{
                {ClientLibrary.JVM_KTOR},
                {ClientLibrary.JVM_OKHTTP4},
                {ClientLibrary.JVM_SPRING_WEBCLIENT},
                {ClientLibrary.JVM_SPRING_RESTCLIENT},
                {ClientLibrary.JVM_RETROFIT2},
                {ClientLibrary.MULTIPLATFORM},
                {ClientLibrary.JVM_VOLLEY},
                {ClientLibrary.JVM_VERTX},
                {ClientLibrary.JVM_MICROPROFILE}
        };
    }

    /**
     * Spec parameters whose names collide with a statement-level local
     * declared by the library's api template (a parameter reference in a
     * statement after the local's declaration would read the local, not the
     * parameter) are renamed with a {@code param_} prefix; their wire names
     * (baseName) are unchanged. Names referenced only inside the local's own
     * initializer (localVariableConfig, localVarResponse, localVarError) and
     * non-colliding names (localVariant, normalParam) are never renamed, and
     * no name is renamed in libraries whose templates declare none of these
     * localVariable* locals.
     */
    @Test(dataProvider = "allClientLibraries")
    public void testTemplateLocalNameCollisionParamsAreRenamed(ClientLibrary library) throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-template-local-collision.yaml");

        KotlinClientCodegen codegen = createCodegen(library);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(defaultApi.toPath()).stream().map(String::trim).collect(Collectors.toList());

        switch (library) {
            case JVM_OKHTTP4:
            case JVM_SPRING_WEBCLIENT:
            case JVM_SPRING_RESTCLIENT:
                // colliding params are renamed but still bound to their wire names
                assertFileContainsLine(lines, "val localVariableBody = paramLocalVariableBody");
                assertFileContainsLine(lines, "put(\"localVariableQuery\", listOf(paramLocalVariableQuery.toString()))");
                assertFileContainsLine(lines, "paramLocalVariableHeaders?.apply { localVariableHeaders[\"localVariableHeaders\"] = this.toString() }");
                // shadow-only and non-colliding names keep their names
                assertFileContainsLine(lines, "put(\"localVariableConfig\", listOf(localVariableConfig.toString()))");
                assertFileContainsLine(lines, "put(\"localVariableAuthNames\", listOf(localVariableAuthNames.toString()))");
                assertFileContainsLine(lines, "put(\"local-var-response\", listOf(localVarResponse.toString()))");
                assertFileContainsLine(lines, "put(\"localVarError\", listOf(localVarError.toString()))");
                assertFileContainsLine(lines, "put(\"localVariant\", listOf(localVariant.toString()))");
                assertFileContainsLine(lines, "put(\"normalParam\", listOf(normalParam.toString()))");
                break;
            case JVM_KTOR:
            case MULTIPLATFORM:
                assertFileContainsLine(lines, "val localVariableBody = paramLocalVariableBody");
                assertFileContainsLine(lines, "paramLocalVariableQuery?.apply { localVariableQuery[\"localVariableQuery\"] = listOf(\"$paramLocalVariableQuery\") }");
                assertFileContainsLine(lines, "paramLocalVariableAuthNames?.apply { localVariableQuery[\"localVariableAuthNames\"] = listOf(\"$paramLocalVariableAuthNames\") }");
                assertFileContainsLine(lines, "paramLocalVariableHeaders?.apply { localVariableHeaders[\"localVariableHeaders\"] = this.toString() }");
                assertFileContainsLine(lines, "localVariableConfig?.apply { localVariableQuery[\"localVariableConfig\"] = listOf(\"$localVariableConfig\") }");
                assertFileContainsLine(lines, "localVarResponse?.apply { localVariableQuery[\"local-var-response\"] = listOf(\"$localVarResponse\") }");
                assertFileContainsLine(lines, "localVarError?.apply { localVariableQuery[\"localVarError\"] = listOf(\"$localVarError\") }");
                assertFileContainsLine(lines, "localVariant?.apply { localVariableQuery[\"localVariant\"] = listOf(\"$localVariant\") }");
                assertFileContainsLine(lines, "normalParam?.apply { localVariableQuery[\"normalParam\"] = listOf(\"$normalParam\") }");
                break;
            case JVM_VERTX:
                // vert.x declares none of these localVariable* locals: nothing is renamed
                assertFileContainsLine(lines, "localVariableQuery?.let { request.queryParams().add(\"localVariableQuery\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVariableConfig?.let { request.queryParams().add(\"localVariableConfig\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVariableAuthNames?.let { request.queryParams().add(\"localVariableAuthNames\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVarResponse?.let { request.queryParams().add(\"local-var-response\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVarError?.let { request.queryParams().add(\"localVarError\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVariant?.let { request.queryParams().add(\"localVariant\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "normalParam?.let { request.queryParams().add(\"normalParam\", listOf(it.toString())) }");
                assertFileContainsLine(lines, "localVariableHeaders?.apply { request.putHeader(\"localVariableHeaders\", this.toString())}");
                assertFileContainsLine(lines, ".sendBuffer(responseBody(localVariableBody))");
                break;
            case JVM_RETROFIT2:
                assertFileContainsLine(lines, "fun collideBody(@Body localVariableBody: CollideBodyRequest, @Query(\"localVariableQuery\") localVariableQuery: kotlin.String? = null, @Query(\"localVariableConfig\") localVariableConfig: kotlin.String? = null, @Query(\"localVariableAuthNames\") localVariableAuthNames: kotlin.String? = null, @Query(\"local-var-response\") localVarResponse: kotlin.String? = null, @Query(\"localVarError\") localVarError: kotlin.String? = null, @Query(\"localVariant\") localVariant: kotlin.String? = null, @Query(\"normalParam\") normalParam: kotlin.String? = null, @Header(\"localVariableHeaders\") localVariableHeaders: kotlin.String? = null): Call<kotlin.String>");
                break;
            case JVM_VOLLEY:
                assertFileContainsLine(lines, "val body: Any? = localVariableBody");
                assertFileContainsLine(lines, "\"localVariableQuery\" to IRequestFactory.parameterToString(localVariableQuery),");
                assertFileContainsLine(lines, "\"localVariableConfig\" to IRequestFactory.parameterToString(localVariableConfig),");
                assertFileContainsLine(lines, "\"localVariableAuthNames\" to IRequestFactory.parameterToString(localVariableAuthNames),");
                assertFileContainsLine(lines, "\"local-var-response\" to IRequestFactory.parameterToString(localVarResponse),");
                assertFileContainsLine(lines, "\"localVarError\" to IRequestFactory.parameterToString(localVarError),");
                assertFileContainsLine(lines, "\"localVariant\" to IRequestFactory.parameterToString(localVariant),");
                assertFileContainsLine(lines, "\"normalParam\" to IRequestFactory.parameterToString(normalParam)");
                assertFileContainsLine(lines, "\"localVariableHeaders\" to IRequestFactory.parameterToString(localVariableHeaders)");
                break;
            case JVM_MICROPROFILE:
                assertFileContainsLine(lines, "localVariableBody: CollideBodyRequest,");
                assertFileContainsLine(lines, "@QueryParam(\"localVariableQuery\") localVariableQuery: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"localVariableConfig\") localVariableConfig: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"localVariableAuthNames\") localVariableAuthNames: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"local-var-response\") localVarResponse: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"localVarError\") localVarError: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"localVariant\") localVariant: kotlin.String? = null,");
                assertFileContainsLine(lines, "@QueryParam(\"normalParam\") normalParam: kotlin.String? = null,");
                assertFileContainsLine(lines, "@HeaderParam(\"localVariableHeaders\") localVariableHeaders: kotlin.String? = null");
                break;
            default:
                break;
        }
    }

    /**
     * An explicit parameterNameMapping entry stays authoritative: a mapped
     * spec parameter uses the mapped name even when its original name would
     * otherwise be renamed as a template-local collision.
     */
    @Test
    public void testTemplateLocalNameCollisionParameterNameMappingWins() throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-template-local-collision.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_OKHTTP4);
        codegen.parameterNameMapping().put("localVariableQuery", "explicitLocalVariableQuery");
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(defaultApi.toPath()).stream().map(String::trim).collect(Collectors.toList());

        assertFileContainsLine(lines, "put(\"localVariableQuery\", listOf(explicitLocalVariableQuery.toString()))");
        Assert.assertTrue(lines.stream().noneMatch(line -> line.contains("paramLocalVariableQuery")),
                "parameterNameMapping must win over the template-local rename");
    }

    /**
     * A parameterNameMapping entry that maps a colliding name to itself keeps
     * the parameter unrenamed: the mapping, not the collision check, decides
     * the final name. Unlike the explicitLocalVariableQuery case above (whose
     * mapped name would survive the rename check anyway), this fails if the
     * parameterNameMapping guard in toParamName is removed.
     */
    @Test
    public void testTemplateLocalNameCollisionParameterNameMappingCanPinName() throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-template-local-collision.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.JVM_OKHTTP4);
        codegen.parameterNameMapping().put("localVariableQuery", "localVariableQuery");
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(defaultApi.toPath()).stream().map(String::trim).collect(Collectors.toList());

        assertFileContainsLine(lines, "put(\"localVariableQuery\", listOf(localVariableQuery.toString()))");
        Assert.assertTrue(lines.stream().noneMatch(line -> line.contains("paramLocalVariableQuery")),
                "parameterNameMapping must pin the mapped name against the template-local rename");
    }

    /**
     * Multiplatform renders a single (non-array) file form field as
     * {@code param?.apply { append(param) }}: the appended value is the
     * parameter, so it must be referenced by paramName. Referencing the wire
     * name (baseName) produced an unresolved reference (upload_file) or read
     * the template's local (localVariableQuery, renamed for a template-local
     * collision) instead of the parameter.
     */
    @Test
    public void testMultiplatformMultipartFileParamAppendsParamName() throws IOException {
        OpenAPI openAPI = readOpenAPI("src/test/resources/3_0/kotlin/kotlin-multiplatform-multipart-file-param-name.yaml");

        KotlinClientCodegen codegen = createCodegen(ClientLibrary.MULTIPLATFORM);
        DefaultGenerator generator = new DefaultGenerator();
        enableOnlyApiGeneration(generator);

        List<File> files = generator.opts(createClientOptInput(openAPI, codegen)).generate();
        File defaultApi = files.stream().filter(file -> file.getName().equals("DefaultApi.kt")).findAny().orElseThrow();
        List<String> lines = Files.readAllLines(defaultApi.toPath()).stream().map(String::trim).collect(Collectors.toList());

        assertFileContainsLine(lines, "paramLocalVariableQuery?.apply { append(paramLocalVariableQuery) }");
        assertFileContainsLine(lines, "uploadFile?.apply { append(uploadFile) }");
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
