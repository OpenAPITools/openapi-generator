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
import java.util.List;
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
        } finally {
            deleteRecursively(target);
        }
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
        Path kotlinc = findOnPath("kotlinc");
        if (kotlinc == null) {
            throw new org.testng.SkipException("kotlinc is not on PATH; skipping generated-client verification");
        }
        // PATH may hold a symlink into the install; resolve it so lib/ is found correctly
        kotlinc = kotlinc.toRealPath();
        List<String> jars;
        try {
            jars = new ArrayList<>(Arrays.asList(
                    jarOf(okhttp3.OkHttpClient.class),
                    jarOf(okio.Buffer.class),
                    jarOf(com.squareup.moshi.Moshi.class),
                    jarOf(com.squareup.moshi.kotlin.reflect.KotlinJsonAdapterFactory.class),
                    jarOf(com.squareup.moshi.adapters.Rfc3339DateJsonAdapter.class)));
        } catch (Exception e) {
            throw new org.testng.SkipException("okhttp/moshi jars not resolvable from test classpath: " + e);
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

    private static void generate(String library, String spec, Path outputDir) {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("kotlin")
                .setLibrary(library)
                .setInputSpec(spec)
                .setSkipOverwrite(false)
                .setOutputDir(outputDir.toAbsolutePath().toString().replace("\\", "/"));
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

    private static String jarOf(Class<?> marker) throws Exception {
        return Path.of(marker.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
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
