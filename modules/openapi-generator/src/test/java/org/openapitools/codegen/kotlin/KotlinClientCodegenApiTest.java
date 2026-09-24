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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.openapitools.codegen.config.CodegenConfigurator;

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

    @Test
    void testJvmOkhttp4OpenApi32OperationsAndQueryStringParam() throws IOException {
        Path target = Files.createTempDirectory("kotlin32");
        try {
            generate("jvm-okhttp4", "src/test/resources/3_2/query-operation.yaml", target);

            String api = new String(Files.readAllBytes(
                    target.resolve("src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt")), StandardCharsets.UTF_8);
            // non-standard methods are emitted verbatim via customMethod; there is no RequestMethod.QUERY
            for (String method : new String[]{"QUERY", "PURGE", "customMethod", "CHECK&FETCH", "X#Y", "A|B", "REPORT", "PROPPATCH"}) {
                Assert.assertTrue(api.contains("customMethod = \"" + method + "\""),
                        "expected verbatim customMethod literal for " + method);
            }
            // '$' must be escaped so the Kotlin string literal keeps it verbatim
            Assert.assertTrue(api.contains("customMethod = \"A\\$B\""),
                    "expected $-escaped customMethod literal for A$B");
            Assert.assertTrue(api.contains("method = RequestMethod.GET"),
                    "standard method kept on the RequestMethod enum");
            // `in: querystring` is wired verbatim, excluded from the name=value query map
            Assert.assertTrue(api.contains("encodedQueryString = listOfNotNull(qs).joinToString"),
                    "querystring param should be passed verbatim");
            Assert.assertFalse(api.contains("put(\"qs\""),
                    "querystring param must not be serialized as a name=value pair");
            // params named like template-internal locals are renamed, wire names stay
            Assert.assertTrue(api.contains("fun collidePetsRequestConfig(paramLocalVariableQuery"),
                    "colliding param names must be renamed");
            Assert.assertTrue(api.contains("put(\"localVariableQuery\", listOf(paramLocalVariableQuery.toString()))"),
                    "renamed param must keep its wire name");
            Assert.assertTrue(api.contains("encodedQueryString = listOfNotNull(paramLocalVariableQuery).joinToString"),
                    "renamed querystring param must still be wired verbatim");

            String requestConfig = new String(Files.readAllBytes(
                    target.resolve("src/main/kotlin/org/openapitools/client/infrastructure/RequestConfig.kt")), StandardCharsets.UTF_8);
            Assert.assertTrue(requestConfig.contains("val customMethod: String?"),
                    "RequestConfig should carry the verbatim method field");
            Assert.assertTrue(requestConfig.contains("val encodedQueryString: String?"),
                    "RequestConfig should carry the querystring field");

            String apiClient = new String(Files.readAllBytes(
                    target.resolve("src/main/kotlin/org/openapitools/client/infrastructure/ApiClient.kt")), StandardCharsets.UTF_8);
            Assert.assertTrue(apiClient.contains("builder.method(requestConfig.customMethod, customBody)"),
                    "ApiClient should dispatch verbatim methods");
            Assert.assertTrue(apiClient.contains("encodedQuery("),
                    "ApiClient should append the querystring verbatim");
            // OkHttp 5 rejects a null body for QUERY/REPORT/PROPPATCH, not just QUERY
            Assert.assertTrue(apiClient.contains("requestConfig.customMethod in REQUIRES_REQUEST_BODY_METHODS"),
                    "body-required methods must be handled as a set, not just QUERY");

            String docs = new String(Files.readAllBytes(target.resolve("docs/DefaultApi.md")), StandardCharsets.UTF_8);
            Assert.assertTrue(docs.contains("**A\\|B**"), "doc table should escape |");
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    void testJvmOkhttp4DeepObjectCollisionKeepsSpecWireName() throws IOException {
        Path target = Files.createTempDirectory("kotlin32-deepobj");
        try {
            generate("jvm-okhttp4", "src/test/resources/3_2/kotlin-deep-object-collision.yaml", target);
            String api = new String(Files.readAllBytes(
                    target.resolve("src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt")), StandardCharsets.UTF_8);
            // the kotlin parameter is renamed to avoid the localVariableBody local,
            // but the `name[prop]` wire prefix must stay the spec baseName
            Assert.assertTrue(api.contains("paramLocalVariableBody:"),
                    "colliding param name must be renamed");
            Assert.assertTrue(api.contains("put(\"localVariableBody[foo]\""),
                    "deepObject wire prefix must use the spec baseName, not the renamed param");
            Assert.assertFalse(api.contains("put(\"paramLocalVariableBody[foo]\""),
                    "deepObject wire prefix must not leak the renamed param");
            Assert.assertFalse(api.contains("put(\"foo[foo]\""),
                    "deepObject wire prefix must not collapse to the property name");
            // two deepObject params share the same cached property instance;
            // each must keep its own baseName on the wire (no cross-leak)
            Assert.assertTrue(api.contains("put(\"localVariableQuery[foo]\""),
                    "second deepObject param must keep its own baseName on the wire");
            Assert.assertTrue(api.contains("paramLocalVariableQuery:"),
                    "second colliding param name must be renamed");
            // spelling variants normalize to the same internal local names and
            // must hit the collision guard, keeping spec baseNames on the wire
            Assert.assertTrue(api.contains("put(\"local_variable_headers\", listOf(paramLocalVariableHeaders.toString()))"),
                    "snake_case variant must be renamed but keep spec wire name");
            Assert.assertTrue(api.contains("put(\"LocalVariableQuery\", listOf(paramLocalVariableQuery.toString()))"),
                    "PascalCase variant must be renamed but keep spec wire name");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Canary for issue #15: spec parameters named after template-internal locals
     * (the localVar prefix family) must be renamed while keeping their wire
     * names, and class members must be qualified with `this.` so same-named
     * parameters cannot shadow them. Covers every kotlin-client library.
     */
    @Test
    void testKotlinLibrariesAvoidTemplateLocalCollisions() throws IOException {
        String spec = "src/test/resources/3_0/kotlin/kotlin-member-collision.yaml";
        String[][] libraries = {
                // {library, api source path, additionalProperties}
                {"jvm-vertx", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "serializationLibrary=jackson"},
                {"jvm-volley", "src/main/java/org/openapitools/client/apis/DefaultApi.kt", "serializationLibrary=gson"},
                {"jvm-spring-restclient", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "useSpringBoot3=true", "serializationLibrary=jackson"},
                {"jvm-spring-webclient", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "useSpringBoot3=true", "serializationLibrary=jackson"},
                {"jvm-ktor", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "serializationLibrary=jackson"},
                {"jvm-retrofit2", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "serializationLibrary=jackson"},
                {"jvm-okhttp4", "src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt", "serializationLibrary=jackson"},
                {"multiplatform", "src/commonMain/kotlin/org/openapitools/client/apis/DefaultApi.kt", "dateLibrary=kotlinx-datetime"},
        };
        // every library: colliding params are renamed but keep their wire names
        String[][] renames = {
                {"localVariableAuthNames", "paramLocalVariableAuthNames"},
                {"local_variable_body", "paramLocalVariableBody"},
                {"local_variable_query", "paramLocalVariableQuery"},
                {"localVariableHeaders", "paramLocalVariableHeaders"},
                {"local_variable_response", "paramLocalVariableResponse"},
        };
        for (String[] lib : libraries) {
            Path target = Files.createTempDirectory("kotlin-collide-" + lib[0]);
            try {
                generate(lib[0], spec, target, Arrays.copyOfRange(lib, 2, lib.length));
                Path apiFile = target.resolve(lib[1]);
                Assert.assertTrue(Files.exists(apiFile), lib[0] + " must emit " + lib[1]);
                String api = new String(Files.readAllBytes(apiFile), StandardCharsets.UTF_8);
                for (String[] rename : renames) {
                    Assert.assertTrue(api.contains(rename[1] + ":"),
                            lib[0] + ": param " + rename[0] + " must be renamed to " + rename[1]);
                    Assert.assertTrue(api.contains("\"" + rename[0] + "\""),
                            lib[0] + ": wire name " + rename[0] + " must be preserved");
                }
                switch (lib[0]) {
                    case "jvm-vertx":
                        // member refs must be qualified so same-named params cannot shadow them
                        for (String member : new String[]{"this.vertx", "this.basePath", "this.apiKey",
                                "this.apiKeyPrefix", "this.username", "this.password", "this.accessToken",
                                "this.handleResponse(", "this.responseBody(", "this.encodeURIComponent(",
                                "this.parseDateToQueryString<"}) {
                            Assert.assertTrue(api.contains(member), "jvm-vertx must qualify " + member);
                        }
                        Assert.assertTrue(api.contains("fun basicAuthCollide(username: kotlin.String?"),
                                "jvm-vertx: spec param names must stay public");
                        Assert.assertTrue(api.contains("vertx?.let { localVariableRequest.queryParams().add(\"vertx\""),
                                "jvm-vertx: vertx param must be wired under its own name");
                        Assert.assertTrue(api.contains("localVariableForm.add(\"form\", form)"),
                                "jvm-vertx: form param must reach the form map");
                        break;
                    case "jvm-volley":
                        for (String member : new String[]{"this.requestFactory", "this.basePath",
                                "this.postProcessors", "this.requestQueue"}) {
                            Assert.assertTrue(api.contains(member), "jvm-volley must qualify " + member);
                        }
                        Assert.assertTrue(api.contains("\"form\" to IRequestFactory.parameterToString(form)"),
                                "jvm-volley: form param must reach the form map");
                        Assert.assertTrue(api.contains("\"request\" to IRequestFactory.parameterToString(request)"),
                                "jvm-volley: request param must reach the request");
                        break;
                    case "jvm-spring-restclient":
                        Assert.assertTrue(api.contains("this.request<"),
                                "jvm-spring-restclient: member request() must be qualified");
                        Assert.assertTrue(api.contains("val localVariableResult ="),
                                "jvm-spring-restclient: result local must be prefixed");
                        Assert.assertTrue(api.contains("val localVariableParams ="),
                                "jvm-spring-restclient: params local must be prefixed");
                        assertBareDateConversion(api, "jvm-spring-restclient");
                        break;
                    case "jvm-spring-webclient":
                        Assert.assertTrue(api.contains("this.request<"),
                                "jvm-spring-webclient: member request() must be qualified");
                        Assert.assertTrue(api.contains("val localVariableParams ="),
                                "jvm-spring-webclient: params local must be prefixed");
                        assertBareDateConversion(api, "jvm-spring-webclient");
                        break;
                    case "jvm-okhttp4":
                        // pre-existing locals keep their localVar* spelling
                        Assert.assertTrue(api.contains("localVarResponse") && api.contains("localVarError"),
                                "jvm-okhttp4: localVarResponse/localVarError must remain");
                        // member calls inside apply{} blocks need a labeled receiver
                        Assert.assertTrue(api.contains("this@DefaultApi.parseDateToQueryString<java.time.LocalDate>(dueDate)"),
                                "jvm-okhttp4: date conversion must reach the api class inside apply{}");
                        Assert.assertTrue(api.contains("this@DefaultApi.encodeURIComponent(path.toString())"),
                                "jvm-okhttp4: path encoding must reach the api class");
                        break;
                    case "jvm-ktor":
                    case "multiplatform":
                        // request()/jsonRequest()/urlEncodedFormRequest() are
                        // inherited ApiClient members; a spec `request` param
                        // must not shadow them
                        for (String member : new String[]{"this.request(", "this.jsonRequest(",
                                "this.urlEncodedFormRequest("}) {
                            Assert.assertTrue(api.contains(member), lib[0] + " must qualify " + member);
                        }
                        Assert.assertTrue(api.contains("request: kotlin.String?"),
                                lib[0] + ": spec param `request` must keep its name");
                        break;
                    default:
                        break;
                }
            } finally {
                deleteRecursively(target);
            }
        }
    }

    /**
     * Spring's parseDateToQueryString is a top-level function, and Kotlin
     * resolves a call site to the function even when a value parameter shares
     * its name — so no qualification is wanted at all. A package-qualified
     * call would actually break whenever a parameter is named `org` (the
     * first segment of the default package), which the fixture exercises.
     */
    private void assertBareDateConversion(String api, String lib) {
        Assert.assertTrue(api.contains("listOf(parseDateToQueryString<java.time.LocalDate>(dueDate))"),
                lib + ": date conversion must be an unqualified call");
        Assert.assertFalse(api.contains("infrastructure.parseDateToQueryString"),
                lib + ": date conversion must not be package-qualified");
        Assert.assertFalse(api.contains("this.parseDateToQueryString"),
                lib + ": date conversion must not be this-qualified");
    }

    /**
     * Lint guard for issue #15: every `val`/`var` declared at statement level in
     * an operation template must use the `localVar` prefix, so spec parameters
     * (which are renamed by the toParamName prefix rule) can never collide with
     * template-internal locals again.
     */
    @Test
    void testKotlinApiTemplatesUseLocalVariablePrefix() throws IOException {
        Path libs = Path.of("src/main/resources/kotlin-client/libraries");
        Pattern localDecl = Pattern.compile("^\\s*(?:val|var)\\s+([a-zA-Z_]\\w*)");
        List<String> violations = new ArrayList<>();
        try (var stream = Files.walk(libs)) {
            for (Path template : stream.filter(p -> p.getFileName().toString().equals("api.mustache")
                    || p.getFileName().toString().matches("(queryParams|queryParam|explodedQueryParam|pathParams|headerParams|bodyParams|formParams|paramJavadoc)\\.mustache")).toList()) {
                int lineNo = 0;
                for (String line : Files.readAllLines(template, StandardCharsets.UTF_8)) {
                    lineNo++;
                    Matcher m = localDecl.matcher(line.replaceAll("\\{\\{[^}]*\\}\\}", ""));
                    if (m.find() && !m.group(1).startsWith("localVar")) {
                        violations.add(template + ":" + lineNo + " declares `" + m.group(1) + "`");
                    }
                }
            }
        }
        Assert.assertTrue(violations.isEmpty(),
                "operation-scope locals must use the localVar prefix:\n" + String.join("\n", violations));
    }

    /**
     * Lint guard (issue #15, G1): every val/var member inherited from the
     * library's ApiClient must be referenced as `this.`/`this@` inside the
     * operation body, otherwise a spec parameter with the same name would
     * silently shadow the member. Member names are extracted mechanically
     * from the ApiClient constructor (volley: the api class's own header),
     * and the same set is checked against api.mustache's {{#operation}}
     * block plus the partial templates spliced into it. Comments and
     * mustache tags are stripped first; template lambda bindings use the
     * localVariable prefix so a bare member name is always a violation.
     */
    @Test
    void testKotlinApiTemplatesQualifyInheritedMembers() throws IOException {
        Path libs = Path.of("src/main/resources/kotlin-client/libraries");
        String[][] libraries = {
                {"jvm-ktor", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-okhttp", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-retrofit2", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-spring-restclient", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-spring-webclient", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-vertx", "infrastructure/ApiClient.kt.mustache"},
                {"jvm-volley", "api.mustache"},
                {"multiplatform", "infrastructure/ApiClient.kt.mustache"},
        };
        Pattern operationBlock = Pattern.compile("\\{\\{#operation\\}\\}(.*)\\{\\{/operation\\}\\}", Pattern.DOTALL);
        Pattern mustacheTag = Pattern.compile("\\{\\{\\{[^}]*\\}\\}\\}|\\{\\{[^}]*\\}\\}");
        List<String> violations = new ArrayList<>();
        for (String[] library : libraries) {
            Path libDir = libs.resolve(library[0]);
            Set<String> members = constructorMemberNames(libDir.resolve(library[1]));
            Assert.assertFalse(members.isEmpty(), library[0] + ": no ApiClient constructor members extracted");
            List<Path> targets = new ArrayList<>();
            try (var stream = Files.list(libDir)) {
                for (Path p : stream.filter(p -> p.getFileName().toString().equals("api.mustache")
                        || p.getFileName().toString().matches("(queryParams|queryParam|explodedQueryParam|pathParams|headerParams|bodyParams|formParams|paramJavadoc)\\.mustache")).toList()) {
                    targets.add(p);
                }
            }
            for (Path target : targets) {
                String source = Files.readString(target, StandardCharsets.UTF_8);
                // restrict api.mustache to the operation block; partials are
                // operation-scope by construction
                if (target.getFileName().toString().equals("api.mustache")) {
                    Matcher op = operationBlock.matcher(source);
                    if (!op.find()) {
                        violations.add(target + ": no {{#operation}} block found");
                        continue;
                    }
                    source = op.group(1);
                }
                int lineNo = 0;
                for (String line : source.split("\n", -1)) {
                    lineNo++;
                    String code = mustacheTag.matcher(line).replaceAll("");
                    String trimmed = code.strip();
                    if (trimmed.startsWith("*") || trimmed.startsWith("//")) {
                        continue;
                    }
                    for (String member : members) {
                        if (Pattern.compile("(?<![\\w.@$\"])" + member + "\\b").matcher(code).find()) {
                            violations.add(target + ":" + lineNo + " bare member `" + member + "`: " + trimmed);
                        }
                    }
                }
            }
        }
        Assert.assertTrue(violations.isEmpty(),
                "inherited members must be qualified with this./this@ inside operation bodies:\n"
                        + String.join("\n", violations));
    }

    /**
     * Extracts `val`/`var` member names from the first `class X(...)`
     * constructor in the given template (mustache tags stripped).
     */
    private static Set<String> constructorMemberNames(Path template) throws IOException {
        String source = Files.readString(template, StandardCharsets.UTF_8);
        Matcher cls = Pattern.compile("class\\s+[^\\s(]+\\s*\\(").matcher(source);
        if (!cls.find()) {
            return Collections.emptySet();
        }
        int depth = 1;
        int end = cls.end();
        while (end < source.length() && depth > 0) {
            char c = source.charAt(end);
            if (c == '(') {
                depth++;
            } else if (c == ')') {
                depth--;
            }
            end++;
        }
        String ctor = source.substring(cls.end(), end - 1)
                .replaceAll("\\{\\{[^}]*\\}\\}", "");
        Set<String> names = new TreeSet<>();
        Matcher m = Pattern.compile("\\b(?:val|var)\\s+([a-zA-Z_]\\w*)").matcher(ctor);
        while (m.find()) {
            names.add(m.group(1));
        }
        return names;
    }

    @Test
    void testNonOkhttpLibrariesSkipOpenApi32Operations() throws IOException {
        Path target = Files.createTempDirectory("kotlin32-skip");
        try {
            generate("jvm-ktor", "src/test/resources/3_2/query-operation.yaml", target);
            String api = new String(Files.readAllBytes(
                    target.resolve("src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt")), StandardCharsets.UTF_8);
            Assert.assertTrue(api.contains("listPets"), "GET operation should be kept");
            for (String op : new String[]{"queryPets", "purgePets", "customPets", "checkFetchPets", "hashPets", "pipePets", "dollarPets", "reportItems", "propPatch", "searchItems"}) {
                Assert.assertFalse(api.contains(op + "RequestConfig"),
                        "jvm-ktor must skip unsupported 3.2 operation " + op);
            }
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    void testJvmOkhttp4SkipsInvalidMethodToken() throws IOException {
        Path target = Files.createTempDirectory("kotlin32-invalid");
        try {
            // "MY METHOD" is not a valid RFC 9110 token; okhttp does not validate
            // tokens itself, so the generator must reject it
            String spec = "openapi: 3.2.0\n"
                    + "info: {title: t, version: '1'}\n"
                    + "paths:\n"
                    + "  /pets:\n"
                    + "    get:\n"
                    + "      operationId: listPets\n"
                    + "      responses: {'200': {description: ok}}\n"
                    + "    additionalOperations:\n"
                    + "      \"MY METHOD\":\n"
                    + "        operationId: badMethod\n"
                    + "        responses: {'204': {description: done}}\n";
            Path specFile = target.resolve("spec.yaml");
            Files.writeString(specFile, spec);
            generate("jvm-okhttp4", specFile.toString(), target.resolve("out"));
            String api = new String(Files.readAllBytes(
                    target.resolve("out/src/main/kotlin/org/openapitools/client/apis/DefaultApi.kt")), StandardCharsets.UTF_8);
            Assert.assertTrue(api.contains("listPets"), "GET operation should be kept");
            Assert.assertFalse(api.contains("badMethod"),
                    "invalid RFC 9110 method token must be skipped");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * End-to-end check: compiles the generated jvm-okhttp4 client with kotlinc and
     * runs a raw ServerSocket capture, verifying query/additionalOperations methods
     * and `in: querystring` reach the wire verbatim. Skipped when kotlinc is not on
     * PATH or the dependency jars cannot be located.
     */
    @Test
    void testJvmOkhttp4GeneratedClientSendsVerbatimMethods() throws IOException, InterruptedException {
        Path kotlinc = requireKotlinc();
        List<String> jars = captureDepJars(kotlinc);

        Path target = Files.createTempDirectory("kotlin32-verify");
        try {
            generate("jvm-okhttp4", "src/test/resources/3_2/query-operation.yaml", target);
            Path srcDir = target.resolve("src/main/kotlin");
            Path capture = target.resolve("Capture.kt");
            Files.copy(Path.of("src/test/resources/3_2/kotlin-okhttp-capture/Capture.kt"), capture);

            List<String> sources = Files.walk(srcDir)
                    .filter(p -> p.toString().endsWith(".kt"))
                    .map(Path::toString)
                    .collect(Collectors.toList());
            sources.add(capture.toString());

            String classPath = String.join(File.pathSeparator, jars);
            Path classesDir = target.resolve("classes");
            List<String> compile = new ArrayList<>(List.of(
                    kotlinc.toString(), "-cp", classPath, "-d", classesDir.toString(), "-jvm-target", "17"));
            compile.addAll(sources);
            runProcess(target, "kotlinc.log", 300, compile.toArray(new String[0]));

            String javaBin = Path.of(System.getProperty("java.home"), "bin", "java").toString();
            String output = runProcess(target, "run.log", 120,
                    javaBin, "-cp", classesDir + File.pathSeparator + classPath, "CaptureKt");
            Assert.assertTrue(output.contains("CAPTURE-PASS"),
                    "generated client did not send verbatim 3.2 methods/querystring:\n" + output);
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Issue #15 regression: a spec parameter named `org` breaks a
     * package-qualified parseDateToQueryString call (the qualifier resolves
     * against the parameter, not the package). The call is intentionally
     * unqualified — Kotlin prefers the function over a value parameter at a
     * call site — and both spring libraries must compile as-is with kotlinc,
     * alongside colliding parameters (`org`, `parseDateToQueryString`,
     * `request`, ...). Skipped when kotlinc or the copied deps are missing.
     */
    @Test
    void testJvmSpringGeneratedClientsCompileWithCollidingParams() throws IOException, InterruptedException {
        Path kotlinc = requireKotlinc();
        List<String> jars = captureDepJars(kotlinc);
        String classPath = String.join(File.pathSeparator, jars);

        String[][] libraries = {
                {"jvm-spring-restclient", "serializationLibrary=jackson", "useSpringBoot3=true"},
                {"jvm-spring-webclient", "serializationLibrary=jackson", "useSpringBoot3=true"},
        };
        for (String[] lib : libraries) {
            Path target = Files.createTempDirectory("kotlin-spring-compile-" + lib[0]);
            try {
                generate(lib[0], "src/test/resources/3_0/kotlin/kotlin-member-collision.yaml",
                        target, Arrays.copyOfRange(lib, 1, lib.length));
                List<String> sources = Files.walk(target.resolve("src/main/kotlin"))
                        .filter(p -> p.toString().endsWith(".kt"))
                        .map(Path::toString)
                        .collect(Collectors.toList());
                Assert.assertFalse(sources.isEmpty(), lib[0] + " produced no kotlin sources");
                List<String> compile = new ArrayList<>(List.of(
                        kotlinc.toString(), "-cp", classPath, "-d",
                        target.resolve("classes").toString(), "-jvm-target", "17"));
                compile.addAll(sources);
                runProcess(target, "kotlinc.log", 300, compile.toArray(new String[0]));
            } finally {
                deleteRecursively(target);
            }
        }
    }

    private static Path requireKotlinc() throws IOException {
        Path kotlinc = findOnPath("kotlinc");
        if (kotlinc == null) {
            throw new org.testng.SkipException("kotlinc is not on PATH; skipping generated-client verification");
        }
        // PATH may hold a symlink into the install; resolve it so lib/ is found correctly
        return kotlinc.toRealPath();
    }

    private static List<String> captureDepJars(Path kotlinc) {
        // okhttp5/moshi/spring jars are copied out-of-band by
        // maven-dependency-plugin (kotlin-capture-deps): they carry Kotlin
        // 1.8+/2.x metadata that the embedded 1.6 compiler in KotlinTestUtils
        // cannot read, so they must never sit on the shared test classpath
        Path depDir = Path.of("target/kotlin-capture-deps");
        List<String> jars;
        try (var stream = Files.list(depDir)) {
            jars = stream.filter(p -> p.toString().endsWith(".jar"))
                    .map(p -> p.toAbsolutePath().toString())
                    .collect(Collectors.toCollection(ArrayList::new));
        } catch (IOException e) {
            throw new org.testng.SkipException("kotlin-capture-deps missing (dependency:copy did not run): " + e);
        }
        if (jars.size() < 5) {
            throw new org.testng.SkipException("expected okhttp/okio/moshi/spring jars in " + depDir + ", found " + jars);
        }
        // use the stdlib/reflect bundled with the detected kotlinc so versions match
        // the compiler (the module's own test classpath pins an older kotlin.version)
        Path kotlincLib = kotlinc.getParent().getParent().resolve("lib");
        for (String name : new String[]{"kotlin-stdlib.jar", "kotlin-reflect.jar"}) {
            Path jar = kotlincLib.resolve(name);
            if (!Files.exists(jar)) {
                throw new org.testng.SkipException("kotlinc lib dir lacks " + name + ": " + kotlincLib);
            }
            jars.add(jar.toString());
        }
        return jars;
    }

    private static void generate(String library, String spec, Path outputDir, String... additionalProperties) {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary(library)
                .setInputSpec(spec)
                .setSkipOverwrite(false)
                .setOutputDir(outputDir.toAbsolutePath().toString().replace("\\", "/"));
        for (String kv : additionalProperties) {
            int eq = kv.indexOf('=');
            configurator.addAdditionalProperty(kv.substring(0, eq), kv.substring(eq + 1));
        }
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
    }

    private static Path findOnPath(String executable) {
        for (String dir : System.getenv("PATH").split(File.pathSeparator)) {
            for (String name : new String[]{executable, executable + ".bat", executable + ".exe"}) {
                Path candidate = Path.of(dir, name);
                if (Files.isExecutable(candidate)) {
                    return candidate;
                }
            }
        }
        return null;
    }

    private static String runProcess(Path workDir, String logName, long timeoutSeconds, String... command)
            throws IOException, InterruptedException {
        Path log = workDir.resolve(logName);
        Process p = new ProcessBuilder(command)
                .directory(workDir.toFile())
                .redirectErrorStream(true)
                .redirectOutput(log.toFile())
                .start();
        if (!p.waitFor(timeoutSeconds, java.util.concurrent.TimeUnit.SECONDS)) {
            p.destroyForcibly();
            Assert.fail("process timed out: " + String.join(" ", command)
                    + "\n" + new String(Files.readAllBytes(log), StandardCharsets.UTF_8));
        }
        String output = new String(Files.readAllBytes(log), StandardCharsets.UTF_8);
        Assert.assertEquals(p.exitValue(), 0, "process failed: " + String.join(" ", command) + "\n" + output);
        return output;
    }

    private static void deleteRecursively(Path dir) throws IOException {
        if (Files.exists(dir)) {
            try (var stream = Files.walk(dir)) {
                stream.sorted(java.util.Comparator.reverseOrder())
                        .forEach(p -> p.toFile().delete());
            }
        }
    }
}
