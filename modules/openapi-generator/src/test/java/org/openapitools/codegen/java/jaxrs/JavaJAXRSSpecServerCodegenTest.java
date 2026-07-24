package org.openapitools.codegen.java.jaxrs;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.openapitools.codegen.TestUtils.*;
import static org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen.*;
import static org.openapitools.codegen.languages.features.GzipFeatures.USE_GZIP_FEATURE;
import static org.testng.Assert.assertTrue;

/**
 * Unit-Test for {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen}.
 *
 * @author attrobit
 */
public class JavaJAXRSSpecServerCodegenTest extends JavaJaxrsBaseTest {

    // Regex matching @jakarta.annotation.security.RolesAllowed({"**"}).
    // Built with char concatenation so the embedded double-quote is never inside a string literal,
    // which prevents formatters from stripping the escape and breaking compilation.
    private static final String ROLES_ALLOWED_WILDCARD_PATTERN =
            "@jakarta\\.annotation\\.security\\.RolesAllowed\\(\\{" + '"' + "\\*\\*" + '"' + "\\}\\)";

    // Regex matching @jakarta.annotation.security.RolesAllowed({"scope1","scope2",...}) -- one or more
    // named scopes. Excludes the {"**"} wildcard form via a negative lookahead on the first quoted value.
    private static final String ROLES_ALLOWED_SCOPED_PATTERN =
            "@jakarta\\.annotation\\.security\\.RolesAllowed\\(\\{"
                    + '"' + "(?!\\*\\*" + '"' + ")[^" + '"' + "]+" + '"'
                    + "(?:," + '"' + "[^" + '"' + "]+" + '"' + ")*"
                    + "\\}\\)";

    // Regex matching @jakarta.annotation.security.PermitAll. Negative lookahead guards against
    // a hypothetical @PermitAllSomething identifier in the generated source.
    private static final String PERMIT_ALL_PATTERN =
            "@jakarta\\.annotation\\.security\\.PermitAll(?![A-Za-z0-9_])";

    @BeforeMethod
    public void before() {
        codegen = new JavaJAXRSSpecServerCodegen();
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaJAXRSSpecServerCodegen codegen = new JavaJAXRSSpecServerCodegen();
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, false);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "org.openapitools.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "org.openapitools.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "org.openapitools.api");
        codegen.additionalProperties().put(JavaJAXRSSpecServerCodegen.SERVER_PORT, "8082");
        codegen.additionalProperties().put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "src/main/openapi/openapi.yaml");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavaJAXRSSpecServerCodegen codegen = new JavaJAXRSSpecServerCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.setOpenApiSpecFileLocation("src/main/resources/META-INF/openapi.yaml");
        codegen.processOpts();

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, true);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xx.yyyyyyyy.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xx.yyyyyyyy.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xx.yyyyyyyy.invoker");
        configAssert.assertValue(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "src/main/resources/META-INF/openapi.yaml");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaJAXRSSpecServerCodegen codegen = new JavaJAXRSSpecServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        codegen.additionalProperties().put("serverPort", "8088");
        codegen.additionalProperties().put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "openapi.yml");
        codegen.additionalProperties().put(SUPPORT_ASYNC, true);
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        ConfigAssert configAssert = new ConfigAssert(codegen.additionalProperties());
        configAssert.assertValue(CodegenConstants.HIDE_GENERATION_TIMESTAMP, codegen::isHideGenerationTimestamp, true);
        configAssert.assertValue(CodegenConstants.MODEL_PACKAGE, codegen::modelPackage, "xyz.yyyyy.mmmmm.model");
        configAssert.assertValue(CodegenConstants.API_PACKAGE, codegen::apiPackage, "xyz.yyyyy.aaaaa.api");
        configAssert.assertValue(CodegenConstants.INVOKER_PACKAGE, codegen::getInvokerPackage, "xyz.yyyyy.iiii.invoker");
        configAssert.assertValue(AbstractJavaJAXRSServerCodegen.SERVER_PORT, "8088");
        configAssert.assertValue(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, codegen::getOpenApiSpecFileLocation, "openapi.yml");
        configAssert.assertValue(SUPPORT_ASYNC, true);
    }

    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/" without "useTags"
     */
    @Test
    public void testAddOperationToGroupForRootResourceAndUseTagsFalse() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "findPrimaryresource";
        codegenOperation.path = "/";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();

        codegen.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);

        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey("default"));
        Assert.assertEquals(codegenOperation.baseName, "default");
    }

    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/" with "useTags"
     */
    @Test
    public void testAddOperationToGroupForRootResourceAndUseTagsTrue() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "findPrimaryresource";
        codegenOperation.path = "/";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        codegen.setUseTags(true);

        codegen.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);

        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey("Primaryresource"));
        Assert.assertEquals(codegenOperation.baseName, "Primaryresource");
    }

    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path param.
     */
    @Test
    public void testAddOperationToGroupForRootResourcePathParamAndUseTagsFalse() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "getPrimaryresource";
        codegenOperation.path = "/{uuid}";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();

        codegen.addOperationToGroup("Primaryresource", "/{uuid}", operation, codegenOperation, operationList);

        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey("default"));
    }

    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path param.
     */
    @Test
    public void testAddOperationToGroupForRootResourcePathParamAndUseTagsTrue() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "getPrimaryresource";
        codegenOperation.path = "/{uuid}";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        codegen.setUseTags(true);

        codegen.addOperationToGroup("Primaryresource", "/{uuid}", operation, codegenOperation, operationList);

        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey("Primaryresource"));
        Assert.assertEquals(codegenOperation.baseName, "Primaryresource");
    }

    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String,
     * Operation, CodegenOperation, Map)} for Resource with path "/subresource".
     */
    @Test
    public void testAddOperationToGroupForSubresource() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.path = "/subresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();

        codegen.addOperationToGroup("Default", "/subresource", operation, codegenOperation, operationList);

        Assert.assertEquals(codegenOperation.baseName, "subresource");
        Assert.assertEquals(operationList.size(), 1);
        assertTrue(operationList.containsKey("subresource"));
    }

    /**
     * Test {@link JavaJAXRSSpecServerCodegen#toApiName(String)} with subresource.
     */
    @Test
    public void testToApiNameForSubresource() {
        final String subresource = codegen.toApiName("subresource");
        Assert.assertEquals(subresource, "SubresourceApi");
    }

    @Test
    public void testGeneratePingDefaultLocation() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/main/openapi/openapi.yaml");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingDefaultArrayValue() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping-array-default.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/main/openapi/openapi.yaml");

        Path path = Paths.get(output.toPath() + "/src/gen/java/org/openapitools/model/AnArrayOfString.java");

        assertFileContains(path, "\nimport java.util.Arrays;\n");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingNoSpecFile() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);
        TestUtils.ensureDoesNotContainFile(files, output, "src/main/openapi/openapi.yaml");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingAlternativeLocation1() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "src/main/resources/META-INF/openapi.yaml");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/main/resources/META-INF/openapi.yaml");

        output.deleteOnExit();
    }

    @Test
    public void testGeneratePingAlternativeLocation2() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "openapi.yml");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);
        TestUtils.ensureContainsFile(files, output, "openapi.yml");

        output.deleteOnExit();
    }

    @Test
    public void testGenerateApiWithPrecedingPathParameter_issue1347() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "openapi.yml");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/issue_1347.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator(false);
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "openapi.yml");
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/DefaultApi.java");

        output.deleteOnExit();
    }

    @Test
    public void testGenerateApiWithCookieParameter_issue2908() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaJAXRSSpecServerCodegen.OPEN_API_SPEC_FILE_LOCATION, "openapi.yml");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/issue_2908.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator(false);
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "openapi.yml");
        files.stream().forEach(System.out::println);
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/SomethingApi.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/SomethingApi.java"), "@CookieParam");

        output.deleteOnExit();
    }

    @Test
    public void addsImportForSetArgument() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/setParameter.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator(false);
        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java");

        assertFileContains(path, "\nimport java.util.Set;\n");
    }

    @Test
    public void addsImportForSetResponse() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/setResponse.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        Path path = Paths.get(outputPath + "/src/gen/java/org/openapitools/api/ExamplesApi.java");

        assertFileContains(path, "\nimport java.util.Set;\n");
    }

    @Test
    public void generateApiWithAsyncSupport() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated class contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "\nimport java.util.concurrent.CompletableFuture;\n",
                "\npublic CompletionStage<Response> pingGet() {\n",
                "\nCompletableFuture.supplyAsync(() -> Response.ok().entity(\"magic!\").build())\n"
        );
    }

    @Test
    public void generateApiWithAsyncSupportAndInterfaceOnly() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interface contains CompletionStage<Void>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "\nCompletionStage<Void> pingGet();\n");
    }

    @Test
    public void generateApiWithAsyncSupportAndInterfaceOnlyAndResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(RETURN_RESPONSE, true); //And return type is Response

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interface contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "\nCompletionStage<Response> pingGet();\n");
    }

    @Test
    public void generateApiWithAsyncSupportAndInterfaceOnlyAndJBossResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(RETURN_JBOSS_RESPONSE, true); //And return type is RestResponse
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); //And return type is RestResponse

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interface contains CompletionStage<RestResponse<Pet>>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "\nimport org.jboss.resteasy.reactive.RestResponse;\n",
                "\nimport java.util.concurrent.CompletionStage;\n",
                "CompletionStage<RestResponse<Pet>> addPet", "CompletionStage<RestResponse<Void>> deletePet");
    }


    @Test
    public void generatePetstoreAPIWithAsyncSupport() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains CompletionStage
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "CompletionStage<Void> deletePet", //Support empty response
                "CompletionStage<List<Pet>> findPetsByStatus", //Support type of arrays response
                "CompletionStage<Pet> getPetById" //Support single type response
        );

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/StoreApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/StoreApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "CompletionStage<Map<String, Integer>>" //Support map response
        );

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/UserApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/UserApi.java"),
                "\nimport java.util.concurrent.CompletionStage;\n",
                "CompletionStage<String>" //Support simple types
        );
    }

    @Test
    public void generatePingWithAsyncSupportPrimitiveType() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4832.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in PingApi.java

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains CompletionStage with proper classes instead of primitive types
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "CompletionStage<Boolean> pingGetBoolean", //Support primitive types response
                "CompletionStage<Integer> pingGetInteger" //Support primitive types response
        );
    }

    @Test
    public void generateDeepObjectArrayWithPattern() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/deepobject-array-with-pattern.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(input).generate();

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/model/Options.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/Options.java"), "List< @Pattern(regexp=\"^[A-Z].*\")String> getA()");
    }

    @Test
    public void testHandleDefaultValue_issue8535() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_8535.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("TestHeadersApi.java"))
                .assertMethod("headersTest")
                .assertParameter("headerNumber").hasType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"11.2\""))
                .toParameter().toMethod()
                .assertParameter("headerString").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("headerStringWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("headerStringQuotes").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("headerStringQuotesWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("headerBoolean").hasType("Boolean")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"true\""));

        JavaFileAssert.assertThat(files.get("TestQueryParamsApi.java"))
                .assertMethod("queryParamsTest")
                .assertParameter("queryNumber").hasType("BigDecimal")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"11.2\""))
                .toParameter().toMethod()
                .assertParameter("queryString").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("queryStringWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\""))
                .toParameter().toMethod()
                .assertParameter("queryStringQuotes").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("queryStringQuotesWrapped").hasType("String")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"qwerty\\\"with quotes\\\" test\""))
                .toParameter().toMethod()
                .assertParameter("queryBoolean").hasType("Boolean")
                .assertParameterAnnotations()
                .containsWithNameAndAttributes("DefaultValue", ImmutableMap.of("value", "\"true\""));
    }

    @Test
    public void testValidAnnotation_issue14432() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_14432.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ComplexObject.java"))
                .fileContains("private @Valid List<LocalDate> dates")
                .fileDoesNotContain("private @Valid SymbolTypeEnum symbolType")
                .fileDoesNotContain("@Valid String")
                .fileDoesNotContain("@Valid Double");
    }

    @Test
    public void arrayNullableDefaultValueTests() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_13025.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SUPPORT_ASYNC, true); //Given support async is enabled
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated model contains correct default value for array properties (optional)
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/model/Body.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/Body.java"),
                "\nprivate @Valid List<String> arrayThatIsNull;\n");

        //And the generated model contains correct default value for array properties (required, nullable)
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/model/BodyWithRequiredNullable.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/BodyWithRequiredNullable.java"),
                "\nprivate @Valid List<String> arrayThatIsNull;\n");

        //And the generated model contains correct default value for array properties (required)
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/model/BodyWithRequired.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/BodyWithRequired.java"),
                "\nprivate @Valid List<String> arrayThatIsNotNull = new ArrayList<>();\n");

    }

    @Test
    public void generateApiForQuarkusWithGzipFeature() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_GZIP_FEATURE, true);

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated class contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "\nimport org.jboss.resteasy.annotations.GZIP\n",
                "@GZIP\n"
        );
    }

    @Test
    public void generateApiForQuarkusWithoutMutiny() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4832.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(SUPPORT_ASYNC, true);
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in PingApi.java

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated class contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "CompletionStage<Response> pingGetBoolean", //Support primitive types response
                "CompletionStage<Response> pingGetInteger" //Support primitive types response
        );
    }

    @Test
    public void generateApiForQuarkusWithMutinyApi() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4832.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in PingApi.java
        codegen.additionalProperties().put(SUPPORT_ASYNC, true);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_MUTINY, true);

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated class contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "Uni<Boolean> pingGetBoolean", //Support primitive types response
                "Uni<Integer> pingGetInteger" //Support primitive types response
        );
    }


    @Test
    public void generateApiForQuarkusWithMutinyImpl() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4832.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in PingApi.java
        codegen.additionalProperties().put(SUPPORT_ASYNC, true);
        codegen.additionalProperties().put(USE_MUTINY, true);

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //Using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated class contains CompletionStage<Response>
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        TestUtils.assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "Uni<Response> pingGetBoolean", //Support primitive types response
                "Uni<Response> pingGetInteger" //Support primitive types response
        );
    }

    @Test
    public void testHandleRequiredAndReadOnlyPropertiesCorrectly() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/required-and-readonly-property.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("ReadonlyAndRequiredProperties.java"))
                .assertProperty("requiredYesReadonlyYes")
                .toType()
                .assertMethod("getRequiredYesReadonlyYes")
                .assertMethodAnnotations()
                .hasSize(2)
                .containsWithNameAndAttributes("ApiModelProperty", ImmutableMap.of("required", "true"))
                // Mysteriously, but we need to surround the value with quotes if the Annotation only contains a single value
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"requiredYesReadonlyYes\""))
                .toMethod()
                .toFileAssert()
                .assertProperty("requiredYesReadonlyNo")
                .toType()
                .assertMethod("getRequiredYesReadonlyNo")
                .assertMethodAnnotations()
                .hasSize(3)
                .containsWithNameAndAttributes("ApiModelProperty", ImmutableMap.of("required", "true"))
                // Mysteriously, but we need to surround the value with quotes if the Annotation only contains a single value
                .containsWithNameAndAttributes("JsonProperty", ImmutableMap.of("value", "\"requiredYesReadonlyNo\""))
                .containsWithName("NotNull");
    }

    @Test
    public void generateSpecInterfaceWithMicroprofileOpenApiAnnotations() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in several API files
        codegen.additionalProperties().put(RETURN_RESPONSE, true); // Retrieve HTTP Response
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); // Use Jakarta
        codegen.setLibrary(QUARKUS_LIBRARY); // Set Quarkus

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains CompletionStage
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"pet\", version=\"1.0.0\", description=\"Everything about your Pets\",");

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/StoreApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/StoreApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"store\", version=\"1.0.0\", description=\"Access to Petstore orders\",");

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/UserApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/UserApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"user\", version=\"1.0.0\", description=\"Operations about user\",");
    }

    @Test
    public void generateSpecInterfaceWithJBossResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in several API files
        codegen.additionalProperties().put(RETURN_JBOSS_RESPONSE, true); // Use JBoss Response type
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); // Use Jakarta
        codegen.setLibrary(QUARKUS_LIBRARY); // Set Quarkus

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains RestResponse
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "\nimport org.jboss.resteasy.reactive.RestResponse;\n",
                "RestResponse<Pet> addPet", "RestResponse<Void> deletePet", "RestResponse<List<Pet>> findPetsByStatus",
                "RestResponse<Void> updatePetWithForm", "RestResponse<ModelApiResponse> uploadFile");
        assertFileContains(output.toPath().resolve("pom.xml"),
            "<groupId>io.quarkus.resteasy.reactive</groupId>",
            "<artifactId>resteasy-reactive</artifactId>");
    }

    @Test
    public void generateSpecInterfaceWithSwaggerV3Annotations() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, true); // only interfaces
        codegen.additionalProperties().put(USE_TAGS, true); // split by tags
        codegen.additionalProperties().put(USE_SWAGGER_V3_ANNOTATIONS, true); // enable Swagger v3 annotations
        // keep default library (spec), do NOT enable MicroProfile

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        // Then the java files are compilable
        validateJavaSourceFiles(files);

        // And the generated interfaces contain Swagger v3 annotations and imports
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "import io.swagger.v3.oas.annotations.*;",
                "import io.swagger.v3.oas.annotations.media.*;",
                "import io.swagger.v3.oas.annotations.responses.*;",
                "import io.swagger.v3.oas.annotations.tags.Tag;",
                "@Tag(name = \"Pet\")");
        // Ensure MicroProfile annotations are NOT present
        TestUtils.assertFileNotContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition");

        // And pom declares io.swagger.core.v3 swagger-annotations dependency and property
        assertFileContains(output.toPath().resolve("pom.xml"),
                "<groupId>io.swagger.core.v3</groupId>",
                "<artifactId>swagger-annotations</artifactId>",
                "<io.swagger.v3.annotations.version>");
    }

    @Test
    public void generateSpecInterfaceWithMutinyAndJBossResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in several API files
        codegen.additionalProperties().put(RETURN_JBOSS_RESPONSE, true); // Use JBoss Response type
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); // Use JBoss Response type
        codegen.additionalProperties().put(SUPPORT_ASYNC, true);
        codegen.additionalProperties().put(USE_MUTINY, true); // Use Mutiny
        codegen.setLibrary(QUARKUS_LIBRARY); // Set Quarkus

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains RestResponse
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "\nimport org.jboss.resteasy.reactive.RestResponse;\n", "Uni<RestResponse<Pet>> addPet",
            "Uni<RestResponse<Void>> deletePet", "Uni<RestResponse<List<Pet>>> findPetsByStatus",
            "Uni<RestResponse<ModelApiResponse>> uploadFile");
    }

    @Test
    public void generateSpecNonInterfaceWithMicroprofileOpenApiAnnotations() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, false); //And only interfaces are generated
        codegen.additionalProperties().put(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_TAGS, true); //And use tags to generate everything in several API files
        codegen.additionalProperties().put(RETURN_RESPONSE, true); // Retrieve HTTP Response
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); // Use Jakarta
        codegen.setLibrary(QUARKUS_LIBRARY); // Set Quarkus

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen); //using JavaJAXRSSpecServerCodegen

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate(); //When generating files

        //Then the java files are compilable
        validateJavaSourceFiles(files);

        //And the generated interfaces contains CompletionStage
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"pet\", version=\"1.0.0\", description=\"Everything about your Pets\",");

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/StoreApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/StoreApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"store\", version=\"1.0.0\", description=\"Access to Petstore orders\",");

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/UserApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/UserApi.java"),
                "@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(\n" +
                        "   info = @org.eclipse.microprofile.openapi.annotations.info.Info(\n" +
                        "        title = \"user\", version=\"1.0.0\", description=\"Operations about user\",");
    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationTrue_issue13444() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13444.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.ENUM_UNKNOWN_DEFAULT_CASE, "true");

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("return UNKNOWN_DEFAULT_OPEN_API");

    }

    @Test
    public void testEnumUnknownDefaultCaseDeserializationNotSet_issue13444() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_13444.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        JavaFileAssert.assertThat(files.get("Color.java"))
                .assertMethod("fromValue").bodyContainsLines("throw new IllegalArgumentException(\"Unexpected value '\" + value + \"'\");");

    }

    @Test
    public void disableGenerateJsonCreator() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/required-properties.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        ((JavaJAXRSSpecServerCodegen) codegen).setGenerateJsonCreator(false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        assertFileNotContains(files.get("RequiredProperties.java").toPath(), "@JsonCreator");
    }

    @Test
    public void testDiscriminatorMappingUsedInJsonTypeName() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/jaxrs/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        // Parent model uses its own name
        JavaFileAssert.assertThat(files.get("PetRequest.java"))
                .fileContains("@JsonTypeName(\"PetRequest\")");

        // Child models must use the discriminator mapping value (e.g. "CAT"), not the class name (e.g. "CatRequest")
        JavaFileAssert.assertThat(files.get("CatRequest.java"))
                .fileContains("@JsonTypeName(\"CAT\")")
                .fileDoesNotContain("@JsonTypeName(\"CatRequest\")");

        JavaFileAssert.assertThat(files.get("DogRequest.java"))
                .fileContains("@JsonTypeName(\"DOG\")")
                .fileDoesNotContain("@JsonTypeName(\"DogRequest\")");
    }

    /**
     * A model with a discriminator must emit {@code @JsonIgnoreProperties} on the discriminator
     * property so Jackson does not serialize that property twice (once for the manually declared
     * field and once for the {@code @JsonTypeInfo} type id), which produces a duplicate key in the
     * response body. {@code allowSetters = true} preserves the field during deserialization.
     */
    @Test
    public void testDiscriminatorEmitsJsonIgnorePropertiesToAvoidDuplicateKey() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/jaxrs/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        // The parent model (which carries the discriminator) must ignore the manually declared
        // discriminator property during serialization while still allowing it to be set during
        // deserialization. The discriminator property in this spec is "petType".
        JavaFileAssert.assertThat(files.get("PetRequest.java"))
                .hasImports("com.fasterxml.jackson.annotation.JsonIgnoreProperties")
                .fileContains(
                        "@JsonIgnoreProperties(",
                        "value = \"petType\"",
                        "allowSetters = true")
                // @JsonIgnoreProperties must precede @JsonTypeInfo on the class declaration.
                .fileContainsPattern("@JsonIgnoreProperties\\([\\s\\S]*?@JsonTypeInfo");

        // Child models do not carry the discriminator, so they must not receive the annotation.
        JavaFileAssert.assertThat(files.get("CatRequest.java"))
                .fileDoesNotContain("@JsonIgnoreProperties");
        JavaFileAssert.assertThat(files.get("DogRequest.java"))
                .fileDoesNotContain("@JsonIgnoreProperties");
    }

    /**
     * With {@code legacyDiscriminatorBehavior=false} the discriminator is propagated from the
     * parent onto every child reachable through the discriminator mapping, so each child also
     * emits {@code @JsonTypeInfo}. The {@code @JsonIgnoreProperties} fix must therefore apply to
     * the children too, otherwise they would serialize the discriminator property twice.
     */
    @Test
    public void testDiscriminatorJsonIgnorePropertiesPropagatesToChildren_whenLegacyBehaviorDisabled() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("legacyDiscriminatorBehavior", "false");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/discriminator-mapping-children.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(configurator.toClientOptInput()).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        // The parent and every mapped child must ignore the manually declared discriminator
        // property during serialization (it is written once by @JsonTypeInfo) while still
        // allowing it to be set during deserialization. The discriminator property is "petType".
        for (String modelFile : new String[]{"PetResponse.java", "CatResponse.java", "DogResponse.java"}) {
            JavaFileAssert.assertThat(files.get(modelFile))
                    .hasImports("com.fasterxml.jackson.annotation.JsonIgnoreProperties")
                    .fileContains(
                            "@JsonIgnoreProperties(",
                            "value = \"petType\"",
                            "allowSetters = true")
                    // @JsonIgnoreProperties must precede @JsonTypeInfo on the class declaration.
                    .fileContainsPattern("@JsonIgnoreProperties\\([\\s\\S]*?@JsonTypeInfo");
        }
    }

    /**
     * With {@code useOneOfInterfaces=true} a oneOf schema is generated as a Java interface, and the
     * concrete subtypes implement it. With {@code useSealed=true} the interface is {@code sealed} and
     * {@code permits} its subtypes, which become {@code final} and {@code implements} the interface.
     * The discriminator property is declared only on a shared non-discriminator base (acyclic pattern),
     * so the interface getter type must resolve to the enum model (PetType) from the mapped children.
     * Assertions use {@code assertFileContains} rather than {@code JavaFileAssert} because the latter
     * parses the source with a JavaParser language level that predates the sealed/permits keywords.
     */
    @Test
    public void testOneOfSealedInterfaceGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useOneOfInterfaces", "true");
        properties.put("useSealed", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/oneof_interface.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        String modelDir = output.getAbsolutePath().replace("\\", "/") + "/src/gen/java/org/openapitools/model/";

        assertFileContains(Paths.get(modelDir + "PetRequest.java"),
                "public sealed interface PetRequest",
                "permits CatRequest, DogRequest",
                "PetType getPetType();");
        assertFileNotContains(Paths.get(modelDir + "PetRequest.java"), "class PetRequest");

        assertFileContains(Paths.get(modelDir + "CatRequest.java"),
                "public final class CatRequest",
                "implements PetRequest",
                "public PetType getPetType()");
        assertFileNotContains(Paths.get(modelDir + "CatRequest.java"), "extends PetRequest");

        assertFileContains(Paths.get(modelDir + "DogRequest.java"),
                "public final class DogRequest",
                "implements PetRequest",
                "public PetType getPetType()");
        assertFileNotContains(Paths.get(modelDir + "DogRequest.java"), "extends PetRequest");
    }

    /**
     * With {@code useSealed=true} an allOf class hierarchy driven by a parent discriminator is
     * generated as a sealed parent class that permits its subclasses; the subclasses extend the
     * parent and become final. A middle tier with its own discriminator stays open downwards but
     * closed to outsiders: it is sealed over its own subclasses while extending its parent. A
     * standalone model with no subtypes becomes final, and the generated pom targets Java 17
     * (sealed types need JDK 17+).
     */
    @Test
    public void testSealedClassHierarchyGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useSealed", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/sealed_hierarchy.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        String outputDir = output.getAbsolutePath().replace("\\", "/");
        String modelDir = outputDir + "/src/gen/java/org/openapitools/model/";

        assertFileContains(Paths.get(modelDir + "Pet.java"),
                "public sealed class Pet",
                "permits Cat, Dog");
        assertFileContains(Paths.get(modelDir + "Cat.java"),
                "public sealed class Cat",
                "extends Pet",
                "permits PersianCat");
        assertFileNotContains(Paths.get(modelDir + "Cat.java"), "final class Cat");
        assertFileContains(Paths.get(modelDir + "PersianCat.java"),
                "public final class PersianCat",
                "extends Cat");
        assertFileContains(Paths.get(modelDir + "Dog.java"),
                "public final class Dog",
                "extends Pet");
        assertFileContains(Paths.get(modelDir + "Toy.java"), "public final class Toy");
        assertFileNotContains(Paths.get(modelDir + "Toy.java"), "sealed ", "permits ");

        assertFileContains(Paths.get(outputDir + "/pom.xml"), "<java.version>17</java.version>");
    }

    /**
     * {@code useSealed=true} without {@code useOneOfInterfaces}: the oneOf container is rendered as
     * a plain class whose oneOf members do not extend it, so it must not carry a sealed/permits
     * clause over them (the generated code would not compile). Like any other model without
     * subclasses it becomes final.
     */
    @Test
    public void testUseSealedWithoutOneOfInterfaces() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useSealed", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/oneof_interface.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        String modelDir = output.getAbsolutePath().replace("\\", "/") + "/src/gen/java/org/openapitools/model/";

        assertFileContains(Paths.get(modelDir + "PetRequest.java"), "public final class PetRequest");
        assertFileNotContains(Paths.get(modelDir + "PetRequest.java"), "sealed ", "permits ");
        assertFileContains(Paths.get(modelDir + "CatRequest.java"), "public final class CatRequest");
        assertFileContains(Paths.get(modelDir + "DogRequest.java"), "public final class DogRequest");
        assertFileContains(Paths.get(modelDir + "PetBase.java"), "public final class PetBase");
    }

    /**
     * Without {@code useSealed} the output is unchanged: no sealed/final/permits modifiers are
     * emitted (even though the permits list is populated on the models) and the generated pom
     * still targets Java 1.8.
     */
    @Test
    public void testWithoutUseSealedOutputIsUnchanged() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useOneOfInterfaces", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/oneof_interface.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        String outputDir = output.getAbsolutePath().replace("\\", "/");
        String modelDir = outputDir + "/src/gen/java/org/openapitools/model/";

        assertFileContains(Paths.get(modelDir + "PetRequest.java"), "public interface PetRequest");
        assertFileNotContains(Paths.get(modelDir + "PetRequest.java"), "sealed ", "permits ");
        assertFileContains(Paths.get(modelDir + "CatRequest.java"), "public class CatRequest");
        assertFileNotContains(Paths.get(modelDir + "CatRequest.java"), "final class");

        assertFileContains(Paths.get(outputDir + "/pom.xml"), "<java.version>1.8</java.version>");
    }

    /**
     * With {@code useRecords=true} the concrete subtypes of a generated oneOf interface render as Java
     * records rather than mutable classes. Because a record component {@code petType} exposes the
     * canonical accessor {@code petType()} (not {@code getPetType()}), the interface declares the
     * discriminator accessor in record style so the records satisfy it without bridge methods. The
     * record components carry the {@code @JsonProperty} annotations. Combined here with
     * {@code useSealed=true} so the interface is also sealed and permits the record subtypes.
     */
    @Test
    public void testOneOfRecordImplementationGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useOneOfInterfaces", "true");
        properties.put("useSealed", "true");
        properties.put("useRecords", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/oneof_interface.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        // assertFileContains is used rather than JavaFileAssert because the latter parses the source
        // with a JavaParser language level that predates sealed/record types.
        String modelDir = output.getAbsolutePath().replace("\\", "/") + "/src/gen/java/org/openapitools/model/";

        // The interface is sealed, permits the record subtypes, and declares the discriminator accessor
        // in record style (petType(), not getPetType()) so the records implement it via their canonical
        // accessors.
        assertFileContains(Paths.get(modelDir + "PetRequest.java"),
                "public sealed interface PetRequest",
                "permits CatRequest, DogRequest",
                "PetType petType();");
        assertFileNotContains(Paths.get(modelDir + "PetRequest.java"), "getPetType");

        // The concrete subtypes are records implementing (not extending) the interface, with @JsonProperty
        // on the components and no JavaBean getters or final-class declaration.
        assertFileContains(Paths.get(modelDir + "CatRequest.java"),
                "public record CatRequest(",
                "@JsonProperty(required = true, value = \"petType\")",
                "PetType petType",
                "implements PetRequest");
        assertFileNotContains(Paths.get(modelDir + "CatRequest.java"),
                "class CatRequest", "extends PetRequest", "public PetType getPetType()");

        assertFileContains(Paths.get(modelDir + "DogRequest.java"),
                "public record DogRequest(",
                "implements PetRequest");
        assertFileNotContains(Paths.get(modelDir + "DogRequest.java"),
                "class DogRequest", "extends PetRequest");
    }

    @Test
    public void testGenerateJsonNullableListFieldsHelperMethodReferences_issue23251() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(OPENAPI_NULLABLE, "true");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("jaxrs-spec")
            .setAdditionalProperties(properties)
            .setInputSpec("src/test/resources/bugs/issue_23251.yaml")
            .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/model/BugResponse.java");

        // Assert that the generated model contains JsonNullable fields
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/BugResponse.java"),
            "private JsonNullable<String> nullableField = JsonNullable.<String>undefined();",
            "private JsonNullable<List<String>> nullableList = JsonNullable.<List<String>>undefined();",
            "private JsonNullable<List<@Valid NestedResponse>> nullableObjectList = JsonNullable.<List<@Valid NestedResponse>>undefined();"
        );

        // Assert that the generated model contains correct add and remove helper methods reference for JsonNullable fields
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/model/BugResponse.java"),
            "this.nullableList.get().add(nullableListItem);",
                "this.nullableList.get().remove(nullableListItem);",
            "this.nullableObjectList.get().add(nullableObjectListItem);",
            "this.nullableObjectList.get().remove(nullableObjectListItem);");

        output.deleteOnExit();
    }

    /**
     * OpenAPI {@code deprecated} should surface as {@code @Deprecated} in jaxrs-spec output.
     */
    @Test
    public void generatesDeprecatedAnnotationsForModelsOperationsAndParameters_issue18941() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(INTERFACE_ONLY, true);

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        Path pet = output.toPath().resolve("src/gen/java/org/openapitools/model/Pet.java");
        JavaFileAssert.assertThat(pet)
                .assertProperty("status").assertPropertyAnnotations().containsWithName("Deprecated");
        JavaFileAssert.assertThat(pet).assertMethod("getStatus").hasAnnotation("Deprecated");
        JavaFileAssert.assertThat(pet).assertMethod("setStatus", "StatusEnum").hasAnnotation("Deprecated");

        Path petApi = output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java");
        JavaFileAssert.assertThat(petApi).assertMethod("findPetsByTags", "List<String>").hasAnnotation("Deprecated");
        JavaFileAssert.assertThat(petApi).fileContains("* @deprecated", "findPetsByTags");
        JavaFileAssert.assertThat(petApi).fileContains("findPetsByStatus", "@Deprecated", "@QueryParam(\"status\")");
    }

    /**
     * Verify that when using the quarkus library with interfaceOnly=true, the generated interface
     * method is always annotated with {@code @ResponseStatus(<code>)} for any 2xx or 3xx response,
     * including 200, for explicit documentation purposes.
     * ping.yaml has a 201 response.
     */
    @Test
    public void generateQuarkusInterfaceAddsResponseStatusAnnotationForSuccessCode() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY); //Given the quarkus library is used
        codegen.additionalProperties().put(INTERFACE_ONLY, true); //And only interfaces are generated
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); //Required: @ResponseStatus is only emitted for Jakarta EE (Quarkus 3+)
        // returnResponse and returnJBossResponse are both false (defaults)

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then the generated interface contains the ResponseStatus import and annotation with code 201
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "import org.jboss.resteasy.reactive.ResponseStatus;",
                "@ResponseStatus(201)");
    }

    /**
     * Verify that {@code @ResponseStatus(200)} IS emitted even for the default 200 status code,
     * for explicit documentation purposes.
     */
    @Test
    public void generateQuarkusInterfaceAddsResponseStatusAnnotationFor200Response() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); //Required: @ResponseStatus is only emitted for Jakarta EE (Quarkus 3+)

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then @ResponseStatus(200) IS present for explicit documentation
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PetApi.java");
        assertFileContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PetApi.java"),
                "import org.jboss.resteasy.reactive.ResponseStatus;",
                "@ResponseStatus(200)");
    }


    /**
     * Verify that the {@code @ResponseStatus} annotation is NOT emitted when returnResponse=true,
     * because the user controls the status code via the {@code Response} builder in that mode.
     */
    @Test
    public void generateQuarkusInterfaceDoesNotAddResponseStatusAnnotationWhenReturnResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); //Enabled so returnResponse is the only disabling factor
        codegen.additionalProperties().put(RETURN_RESPONSE, true); //Given returnResponse is true

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then the annotation must NOT appear
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileNotContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "@ResponseStatus",
                "import org.jboss.resteasy.reactive.ResponseStatus");
    }

    /**
     * Verify that the {@code @ResponseStatus} annotation is NOT emitted when returnJBossResponse=true,
     * because the caller controls the status code via the {@code RestResponse} wrapper in that mode.
     */
    @Test
    public void generateQuarkusInterfaceDoesNotAddResponseStatusAnnotationWhenReturnJBossResponse() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true); //Required by returnJBossResponse
        codegen.additionalProperties().put(RETURN_JBOSS_RESPONSE, true); //Given returnJBossResponse is true

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then the annotation must NOT appear
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileNotContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "@ResponseStatus",
                "import org.jboss.resteasy.reactive.ResponseStatus");
    }

    /**
     * Verify that {@code @ResponseStatus} is NOT emitted when using a non-Quarkus jaxrs-spec library,
     * since {@code org.jboss.resteasy.reactive.ResponseStatus} is a RESTEasy Reactive / Quarkus-specific annotation.
     */
    @Test
    public void generateNonQuarkusInterfaceDoesNotAddResponseStatusAnnotation() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        // No setLibrary call — uses the default jaxrs-spec library
        codegen.additionalProperties().put(INTERFACE_ONLY, true);

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then the annotation must NOT appear
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileNotContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "@ResponseStatus",
                "import org.jboss.resteasy.reactive.ResponseStatus");
    }

    /**
     * Verify that the concrete stub class does NOT contain {@code @ResponseStatus} because the
     * stub always returns {@code Response}, and RESTEasy Reactive ignores {@code @ResponseStatus}
     * when the method returns {@code Response}. The annotation lives on the interface instead.
     */
    @Test
    public void generateQuarkusConcreteClassDoesNotAddResponseStatusAnnotation() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/ping.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(INTERFACE_ONLY, false); //Given the concrete class is generated

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        validateJavaSourceFiles(files);

        //Then the concrete class must NOT contain @ResponseStatus (it returns Response, so it would be ignored)
        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/PingApi.java");
        assertFileNotContains(output.toPath().resolve("src/gen/java/org/openapitools/api/PingApi.java"),
                "@ResponseStatus",
                "import org.jboss.resteasy.reactive.ResponseStatus");
    }

    @Test
    public void useJakartaSecurityIsRegisteredWithDefaultFalse() {
        final JavaJAXRSSpecServerCodegen codegen = new JavaJAXRSSpecServerCodegen();
        Assert.assertTrue(
            codegen.cliOptions().stream()
                .anyMatch(opt -> USE_JAKARTA_SECURITY_ANNOTATIONS.equals(opt.getOpt()) && "false".equals(opt.getDefault())),
            "useJakartaSecurityAnnotations should be a registered CLI option defaulting to false"
        );
    }

    @Test
    public void useJakartaSecurityDefaultsToFalseForQuarkusLibrary() {
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.processOpts();

        Assert.assertFalse(
            (boolean) codegen.additionalProperties().getOrDefault(USE_JAKARTA_SECURITY_ANNOTATIONS, false)
        );
    }

    @Test
    public void useJakartaSecurityCanBeEnabledForQuarkusLibrary() {
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
        codegen.processOpts();

        new ConfigAssert(codegen.additionalProperties())
            .assertValue(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
    }

    @Test
    public void useJakartaSecurityNotProcessedForNonQuarkusLibrary() {
        // flag is only consumed when library=quarkus; for other libraries the block is skipped
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
        codegen.processOpts();

        // convertPropertyToBooleanAndWriteBack was never called, so the value was never
        // written back as a boolean — the key holds the raw Object we put in, not false
        Assert.assertNotEquals(false, codegen.additionalProperties().get(USE_JAKARTA_SECURITY_ANNOTATIONS));
    }

    /**
     * Regression for the latent bug where flags resolved before super.processOpts() were
     * silently dropped when library was supplied via additionalProperties (e.g. Gradle
     * configOptions) rather than via setLibrary(). Verifies the flag is honoured even
     * when the library field is not set on the codegen instance directly.
     */
    @Test
    public void useJakartaSecurityIsHonouredWhenLibrarySuppliedViaAdditionalProperties() {
        codegen.additionalProperties().put(CodegenConstants.LIBRARY, QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
        codegen.processOpts();

        new ConfigAssert(codegen.additionalProperties())
            .assertValue(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
    }

    @Test(expectedExceptions = IllegalArgumentException.class,
          expectedExceptionsMessageRegExp = ".*useJakartaSecurityAnnotations.*useJakartaEe.*")
    public void useJakartaSecurityRequiresUseJakartaEe() {
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(USE_JAKARTA_EE, false);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, true);
        codegen.processOpts();
    }

    /**
     * Single parameterised test exercising every spec fixture that drives @RolesAllowed({"**"}) emission.
     * Each row: (spec path, interfaceOnly, useJakartaSecurityAnnotations, expected wildcard occurrences, expected @PermitAll occurrences).
     *
     * The @PermitAll column proves mutual exclusion: wildcard and @PermitAll never appear on the same method,
     * and any operation that flips to "no security" must surface as @PermitAll rather than vanishing.
     *
     * Consolidates per-scheme test methods (HttpBasic, HttpBearer, ApiKey, OpenIdConnect, OAuth2, global, mixed-OR, AND-group)
     * since they only differ by spec path and expected count.
     */
    @DataProvider(name = "quarkusJakartaSecurityCases")
    public Object[][] quarkusJakartaSecurityCases() {
        return new Object[][] {
                // single OAuth2 flow, no scopes — GET → wildcard, POST has no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-no-scopes.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-no-scopes.yaml", true,  false, 0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-no-scopes.yaml", false, true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-no-scopes.yaml", false, false, 0, 0},
                // single OAuth2 flow, non-empty scopes → no @RolesAllowed({"**"}); both ops have security → no PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-with-scopes.yaml", true,  true,  0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-with-scopes.yaml", false, true,  0, 0},
                // multiple OAuth2 flows, all no scopes — GET → wildcard; POST has no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-multi-flow-no-scopes.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-multi-flow-no-scopes.yaml", false, true,  1, 1},
                // OR: one scheme no-scope + one scheme scoped — GET → wildcard, POST → scoped @RolesAllowed (no PermitAll)
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-empty-and-scoped.yaml", true,  true,  1, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-empty-and-scoped.yaml", false, true,  1, 0},
                // HTTP Basic — GET qualifies (wildcard); POST has no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-http-basic.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-http-basic.yaml", true,  false, 0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-http-basic.yaml", false, true,  1, 1},
                // HTTP Bearer — GET wildcard, POST no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-http-bearer.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-http-bearer.yaml", false, true,  1, 1},
                // API Key — GET wildcard, POST no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-api-key.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-api-key.yaml", false, true,  1, 1},
                // OpenID Connect — GET (oidc:[]) wildcard, POST no security (Rule D) → @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-openidconnect-no-scopes.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-openidconnect-no-scopes.yaml", false, true,  1, 1},
                // OpenID Connect — both ops scoped → no wildcard, no PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-openidconnect-with-scopes.yaml", true,  true,  0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-openidconnect-with-scopes.yaml", false, true,  0, 0},
                // global HTTP Basic + Bearer; GET inherits (→ @RolesAllowed({"**"})), POST has security:[] (→ @PermitAll)
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-security-one-op-disabled.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-security-one-op-disabled.yaml", false, true,  1, 1},
                // global OR: unscoped OAuth2 + scoped OAuth2; both ops inherit → both get @RolesAllowed({"**"})
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-oauth2-or-scoped-and-unscoped.yaml", true,  true,  2, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-oauth2-or-scoped-and-unscoped.yaml", false, true,  2, 0},
                // cross-type OR: scoped OAuth2 OR API Key — API Key qualifies even though OAuth2 alone would not
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-scoped-or-api-key.yaml", true,  true,  1, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-scoped-or-api-key.yaml", false, true,  1, 0},
                // AND group: oauth2 empty scopes AND openIdConnect admin:create → never emits (Rule H — no fall-through to @PermitAll)
                {"src/test/resources/3_0/jaxrs-spec/quarkus-and-group-mixed-scopes.yaml", true,  true,  0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-and-group-mixed-scopes.yaml", false, true,  0, 0},
                // OR list with anonymous alternative ({}) — least-restrictive wins, emits @PermitAll (Rule F)
                {"src/test/resources/3_0/jaxrs-spec/quarkus-or-with-anonymous.yaml", true,  true,  0, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-or-with-anonymous.yaml", false, true,  0, 1},
                // Rule A: op-level security:[] with no global -- emits @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-security.yaml", true,  true,  0, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-security.yaml", true,  false, 0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-security.yaml", false, true,  0, 1},
                // Rule B: op-level security:[] AND global security:[] -- emits @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-global-empty.yaml", true,  true,  0, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-global-empty.yaml", false, true,  0, 1},
                // Rule C: op-level security:[] overrides non-empty global -- GET inherits wildcard, POST gets @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-global-non-empty.yaml", true,  true,  1, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-op-empty-global-non-empty.yaml", false, true,  1, 1},
                // Rule D: spec has no security defined anywhere -- emits @PermitAll
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-no-security-defined.yaml", true,  true,  0, 1},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-no-security-defined.yaml", true,  false, 0, 0},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-no-security-defined.yaml", false, true,  0, 1},
                // Rule E: global security:[] inherited by op without local security -- emits @PermitAll (two ops)
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-empty-security.yaml", true,  true,  0, 2},
                {"src/test/resources/3_0/jaxrs-spec/quarkus-global-empty-security.yaml", false, true,  0, 2},
        };
    }

    @Test(dataProvider = "quarkusJakartaSecurityCases")
    public void quarkusEmitsExpectedRolesAllowedWildcardCount(String specPath, boolean interfaceOnly, boolean useFlag,
            int expectedWildcardCount, int expectedPermitAllCount) throws Exception {
        final String content = generateQuarkusItemsApi(specPath, interfaceOnly, useFlag, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), expectedWildcardCount,
                "wildcard @RolesAllowed count mismatch for " + specPath);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), expectedPermitAllCount,
                "@PermitAll count mismatch for " + specPath);
    }

    /**
     * Asserts @RolesAllowed coexists with MicroProfile @SecurityRequirement annotations on the same method.
     * The two come from independent template branches; this guards against template ordering or duplication regressions.
     */
    @Test
    public void quarkusJakartaSecurityCoexistsWithMicroProfileAnnotations() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-microprofile-coexist.yaml",
                true, true, true);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), 1,
                "Expected exactly one @RolesAllowed({\"**\"}) annotation");
        Assert.assertTrue(content.contains("SecurityRequirement"),
                "Expected MicroProfile @SecurityRequirement annotation alongside @RolesAllowed");
    }

    @DataProvider(name = "quarkusJakartaScopedRolesCases")
    public Object[][] quarkusJakartaScopedRolesCases() {
        return new Object[][] {
            // {specPath, interfaceOnly, expectedScopedAnnotationCount, expectedWildcardCount, expectedPermitAllCount}
            // OAuth2 scoped -- reuse PR-1 fixtures; must now emit scoped, not wildcard.
            // Both interfaceOnly modes exercised to guard against regressions in apiInterface.mustache vs apiMethod.mustache.
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-with-scopes.yaml", true, 2, 0, 0},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-with-scopes.yaml", false, 2, 0, 0},
            // Single scope, both interface and implementation modes
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-single-scope.yaml", true, 2, 0, 0},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-single-scope.yaml", false, 2, 0, 0},
            // Multiple scopes and OR-union
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-multiple-scopes.yaml", true, 1, 0, 0},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-multiple-scopes.yaml", false, 1, 0, 0},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-different-scopes.yaml", true, 1, 0, 0},
            // OpenID Connect scoped -- reuse PR-1 fixture
            {"src/test/resources/3_0/jaxrs-spec/quarkus-openidconnect-with-scopes.yaml", true, 2, 0, 0},
            // Global security inheritance and override paths
            {"src/test/resources/3_0/jaxrs-spec/quarkus-global-with-scopes.yaml", true, 2, 0, 0},
            // POST disables with security:[] -- now emits @PermitAll alongside GET's scoped @RolesAllowed
            {"src/test/resources/3_0/jaxrs-spec/quarkus-global-scopes-op-override.yaml", true, 1, 0, 1},
            // AND groups
            {"src/test/resources/3_0/jaxrs-spec/quarkus-and-with-apikey-and-scoped-oauth2.yaml", true, 1, 0, 0},
            // Rule H: mixed-scope AND group bails -- must NOT fall through to @PermitAll
            {"src/test/resources/3_0/jaxrs-spec/quarkus-and-multi-scoped-warns.yaml", true, 0, 0, 0},
            // GET: unscoped OR scoped -> wildcard wins. POST: scoped-only -> scoped emitted. One of each.
            // Exercised in both modes since this is the canonical "mutual exclusion at file level" fixture.
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-empty-and-scoped.yaml", true, 1, 1, 0},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-empty-and-scoped.yaml", false, 1, 1, 0},
            // Anonymous OR alternative alongside scoped -- emits @PermitAll (Rule F)
            {"src/test/resources/3_0/jaxrs-spec/quarkus-or-empty-anonymous-with-scopes.yaml", true, 0, 0, 1},
            {"src/test/resources/3_0/jaxrs-spec/quarkus-or-empty-anonymous-with-scopes.yaml", false, 0, 0, 1},
        };
    }

    @Test(dataProvider = "quarkusJakartaScopedRolesCases")
    public void quarkusEmitsExpectedScopedRolesAllowedCount(String specPath, boolean interfaceOnly,
            int expectedScopedCount, int expectedWildcardCount, int expectedPermitAllCount) throws Exception {
        final String content = generateQuarkusItemsApi(specPath, interfaceOnly, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN),
                expectedScopedCount,
                "scoped @RolesAllowed count mismatch for " + specPath);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN),
                expectedWildcardCount,
                "wildcard @RolesAllowed count mismatch for " + specPath);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN),
                expectedPermitAllCount,
                "@PermitAll count mismatch for " + specPath);
    }

    @Test
    public void quarkusEmitsAlphabeticallySortedDeduplicatedScopes() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-multiple-scopes.yaml",
                true, true, false);
        assertContainsRolesAllowed(content, "read:items", "write:items");
    }

    @Test
    public void quarkusOrAlternativesProduceUnionedScopes() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-or-different-scopes.yaml",
                true, true, false);
        assertContainsRolesAllowed(content, "admin", "user");
    }

    @Test
    public void quarkusGlobalScopesAreInheritedByOperationsWithoutOverride() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-global-with-scopes.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 2);
        assertContainsRolesAllowed(content, "admin");
    }

    @Test
    public void quarkusOperationOverrideShadowsGlobalScopedSecurity() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-global-scopes-op-override.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 1,
                "Expected exactly one scoped @RolesAllowed (GET with user scope)");
        assertContainsRolesAllowed(content, "user");
        Assert.assertFalse(
                content.contains("RolesAllowed({" + '"' + "admin" + '"'),
                "@RolesAllowed scope 'admin' must be shadowed by the per-operation override");
    }

    @Test
    public void quarkusGlobalEmptySecurityListEmitsPermitAll() throws Exception {
        // A top-level empty `security: []` block means "no security requirements at all" -- both
        // operations inherit the empty list and receive @PermitAll (Rule E).
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-global-empty-security.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), 0,
                "Expected no wildcard @RolesAllowed when global security is empty");
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 0,
                "Expected no scoped @RolesAllowed when global security is empty");
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), 2,
                "Expected @PermitAll on both inheriting operations when global security is empty");
    }

    @Test
    public void quarkusOrAnonymousAlternativeEmitsPermitAll() throws Exception {
        // Rule F: OR list contains `- {}` (anonymous) alongside an authenticated scheme.
        // Least-restrictive wins -- emit @PermitAll, never @RolesAllowed.
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-or-with-anonymous.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), 1,
                "Expected exactly one @PermitAll for OR with anonymous alternative");
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), 0,
                "Anonymous OR must not emit @RolesAllowed({\"**\"})");
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 0,
                "Anonymous OR must not emit scoped @RolesAllowed");
    }

    @Test
    public void quarkusMixedAndGroupDoesNotFallThroughToPermitAll() throws Exception {
        // Rule H: a mixed-scope AND group bails with a warning. The processor must NOT
        // silently emit @PermitAll as a fallback -- absence of authentication metadata is
        // semantically different from "unauthenticated allowed".
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-and-multi-scoped-warns.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), 0,
                "Mixed-scope AND group must not fall through to @PermitAll");
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), 0);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 0);
    }

    @Test
    public void quarkusNoSecurityDefinedAnywhereEmitsPermitAll() throws Exception {
        // Rule D: spec declares no security at all -- emit @PermitAll on every operation.
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-no-security-defined.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), 1,
                "Expected @PermitAll when no security is defined anywhere in the spec");
    }

    @Test
    public void quarkusPermitAllCoexistsWithMicroProfileAnnotations() throws Exception {
        // @PermitAll and MicroProfile OpenAPI annotations come from independent template branches;
        // they must both render on the same method without ordering or duplication regressions.
        // (security:[] means no @SecurityRequirement, so assert against @Operation which is always emitted.)
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-permit-all-microprofile-coexist.yaml",
                true, true, true);
        Assert.assertEquals(TestUtils.countOccurrences(content, PERMIT_ALL_PATTERN), 1,
                "Expected exactly one @PermitAll annotation");
        Assert.assertTrue(content.contains("org.eclipse.microprofile.openapi.annotations.Operation"),
                "Expected MicroProfile @Operation annotation alongside @PermitAll");
    }

    @Test
    public void quarkusScopedJakartaCoexistsWithMicroProfileAnnotations() throws Exception {
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-microprofile-coexist-scoped.yaml",
                true, true, true);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 1,
                "Expected exactly one scoped @RolesAllowed annotation");
        Assert.assertTrue(content.contains("SecurityRequirement"),
                "Expected MicroProfile @SecurityRequirement annotation alongside scoped @RolesAllowed");
    }

    @Test
    public void quarkusNeverEmitsBothWildcardAndScopedRolesAllowedOnSameOperation() throws Exception {
        // oauth2-scoped-or-api-key: GET has scoped OAuth2 OR unscoped API key -- wildcard wins, 0 scoped.
        // POST has scoped OAuth2 only -- scoped emitted, 0 wildcard. Verifies they never appear together.
        final String content = generateQuarkusItemsApi(
                "src/test/resources/3_0/jaxrs-spec/quarkus-oauth2-scoped-or-api-key.yaml",
                true, true, false);
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN), 1,
                "Expected wildcard @RolesAllowed on GET (API key in OR lifts it)");
        Assert.assertEquals(TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN), 1,
                "Expected scoped @RolesAllowed on POST (scoped OAuth2 only)");
        // The two annotations must be on different methods -- total 2 annotations, none doubled
        Assert.assertEquals(
                TestUtils.countOccurrences(content, ROLES_ALLOWED_WILDCARD_PATTERN)
                        + TestUtils.countOccurrences(content, ROLES_ALLOWED_SCOPED_PATTERN),
                2,
                "Each method must have at most one @RolesAllowed annotation");
    }

    @Test
    public void quarkusMixedSecuritySampleEmitsAllExpectedAnnotations() throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/jaxrs-spec/quarkus-mixed-security.yaml", null, new ParseOptions())
                .getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, true);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        validateJavaSourceFiles(files);

        // /public -- security: [] -- emits @PermitAll, no @RolesAllowed
        String publicApi = readGeneratedApi(output, "PublicApi.java");
        Assert.assertEquals(TestUtils.countOccurrences(publicApi, ROLES_ALLOWED_WILDCARD_PATTERN), 0,
                "PublicApi should not have @RolesAllowed -- security: [] disables it");
        Assert.assertEquals(TestUtils.countOccurrences(publicApi, ROLES_ALLOWED_SCOPED_PATTERN), 0,
                "PublicApi should not have scoped @RolesAllowed");
        Assert.assertEquals(TestUtils.countOccurrences(publicApi, PERMIT_ALL_PATTERN), 1,
                "PublicApi should have @PermitAll -- security: [] declares the operation unauthenticated");

        // /authenticated -- oauth2: [] -- wildcard @RolesAllowed({"**"})
        String authenticatedApi = readGeneratedApi(output, "AuthenticatedApi.java");
        Assert.assertEquals(TestUtils.countOccurrences(authenticatedApi, ROLES_ALLOWED_WILDCARD_PATTERN), 1,
                "AuthenticatedApi should have exactly one wildcard @RolesAllowed");
        Assert.assertEquals(TestUtils.countOccurrences(authenticatedApi, PERMIT_ALL_PATTERN), 0,
                "AuthenticatedApi must not have @PermitAll");

        // /admin -- oauth2: [admin] -- @RolesAllowed({"admin"})
        String adminApi = readGeneratedApi(output, "AdminApi.java");
        Assert.assertEquals(TestUtils.countOccurrences(adminApi, ROLES_ALLOWED_SCOPED_PATTERN), 1);
        Assert.assertEquals(TestUtils.countOccurrences(adminApi, PERMIT_ALL_PATTERN), 0,
                "AdminApi must not have @PermitAll");
        assertContainsRolesAllowed(adminApi, "admin");

        // /admin-or-user -- oauth2: [admin] OR oauth2: [user] -- @RolesAllowed({"admin","user"})
        String adminOrUserApi = readGeneratedApi(output, "AdminOrUserApi.java");
        Assert.assertEquals(TestUtils.countOccurrences(adminOrUserApi, ROLES_ALLOWED_SCOPED_PATTERN), 1);
        Assert.assertEquals(TestUtils.countOccurrences(adminOrUserApi, PERMIT_ALL_PATTERN), 0,
                "AdminOrUserApi must not have @PermitAll");
        assertContainsRolesAllowed(adminOrUserApi, "admin", "user");

        // /anonymous-or-authenticated -- oauth2: [] OR {} -- emits @PermitAll (Rule F)
        String anonymousOrAuthenticatedApi = readGeneratedApi(output, "AnonymousOrAuthenticatedApi.java");
        Assert.assertEquals(TestUtils.countOccurrences(anonymousOrAuthenticatedApi, PERMIT_ALL_PATTERN), 1,
                "AnonymousOrAuthenticatedApi should have @PermitAll -- anonymous OR alternative wins");
        Assert.assertEquals(TestUtils.countOccurrences(anonymousOrAuthenticatedApi, ROLES_ALLOWED_WILDCARD_PATTERN), 0,
                "AnonymousOrAuthenticatedApi must not have wildcard @RolesAllowed");
        Assert.assertEquals(TestUtils.countOccurrences(anonymousOrAuthenticatedApi, ROLES_ALLOWED_SCOPED_PATTERN), 0,
                "AnonymousOrAuthenticatedApi must not have scoped @RolesAllowed");
    }

    private static String readGeneratedApi(File outputDir, String apiFileName) throws Exception {
        return Files.readString(outputDir.toPath().resolve("src/gen/java/org/openapitools/api/" + apiFileName));
    }

    private static void assertContainsRolesAllowed(String content, String... expectedScopes) {
        StringBuilder expected = new StringBuilder("@jakarta.annotation.security.RolesAllowed({");
        for (int i = 0; i < expectedScopes.length; i++) {
            if (i > 0) expected.append(",");
            expected.append('"').append(expectedScopes[i]).append('"');
        }
        expected.append("})");
        Assert.assertTrue(
                content.contains(expected.toString()),
                "Expected to find: " + expected + "\nin generated content");
    }

    private String generateQuarkusItemsApi(String specPath, boolean interfaceOnly, boolean useJakartaSecurity, boolean useMicroProfile) throws Exception {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation(specPath, null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setLibrary(QUARKUS_LIBRARY);
        codegen.additionalProperties().put(INTERFACE_ONLY, interfaceOnly);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(USE_JAKARTA_SECURITY_ANNOTATIONS, useJakartaSecurity);
        if (useMicroProfile) {
            codegen.additionalProperties().put(USE_MICROPROFILE_OPENAPI_ANNOTATIONS, true);
        }

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        validateJavaSourceFiles(files);

        TestUtils.ensureContainsFile(files, output, "src/gen/java/org/openapitools/api/ItemsApi.java");
        return Files.readString(output.toPath().resolve("src/gen/java/org/openapitools/api/ItemsApi.java"));
    }

    /**
     * With {@code useOneOfInterfaces=true} a oneOf schema is generated as a Java interface, and the
     * concrete subtypes implement (not extend) it. The discriminator property is declared only on a
     * shared non-discriminator base (acyclic pattern), so the interface getter type resolves to the
     * enum model (PetType) from the mapped children rather than falling back to String.
     */
    @Test
    public void testOneOfInterfaceGeneration() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        Map<String, Object> properties = new HashMap<>();
        properties.put("useOneOfInterfaces", "true");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-spec")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/jaxrs-spec/oneof_interface.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(configurator.toClientOptInput()).generate();

        String modelDir = output.getAbsolutePath().replace("\\", "/") + "/src/gen/java/org/openapitools/model/";

        // The oneOf schema becomes an interface declaring the discriminator getter with the resolved
        // enum type rather than String.
        assertFileContains(Paths.get(modelDir + "PetRequest.java"),
                "public interface PetRequest",
                "PetType getPetType();");
        // an interface is not a class and must not extend a parent
        assertFileNotContains(Paths.get(modelDir + "PetRequest.java"), "class PetRequest");

        // The concrete subtypes implement (not extend) the interface, with a matching getter return type,
        // so there is no cyclical extends/implements and no return-type clash.
        assertFileContains(Paths.get(modelDir + "CatRequest.java"),
                "public class CatRequest",
                "implements PetRequest",
                "public PetType getPetType()");
        assertFileNotContains(Paths.get(modelDir + "CatRequest.java"), "extends PetRequest");

        assertFileContains(Paths.get(modelDir + "DogRequest.java"),
                "public class DogRequest",
                "implements PetRequest",
                "public PetType getPetType()");
        assertFileNotContains(Paths.get(modelDir + "DogRequest.java"), "extends PetRequest");

        // The @JsonTypeName of each subtype is the discriminator mapping value (CAT/DOG), resolved from
        // the oneOf interface it implements, not the class name - so polymorphic (de)serialization keys
        // off the discriminator value and round-trips with the @JsonSubTypes mapping on the interface.
        assertFileContains(Paths.get(modelDir + "CatRequest.java"), "@JsonTypeName(\"CAT\")");
        assertFileNotContains(Paths.get(modelDir + "CatRequest.java"), "@JsonTypeName(\"CatRequest\")");
        assertFileContains(Paths.get(modelDir + "DogRequest.java"), "@JsonTypeName(\"DOG\")");
        assertFileNotContains(Paths.get(modelDir + "DogRequest.java"), "@JsonTypeName(\"DogRequest\")");
    }

    @Test
    public void generatesEmailAnnotationWhenBeanValidationEnabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        Path person = Paths.get(outputPath + "/src/gen/java/org/openapitools/model/PersonWithEmail.java");
        // format: email string is validated with jakarta.validation.constraints.@Email,
        // covered by the wildcard "import {javaxPackage}.validation.constraints.*;"
        assertFileContains(person, "@Email");
        assertFileContains(person, "import javax.validation.constraints.*;");
    }

    @Test
    public void generatesJakartaEmailAnnotationWhenBeanValidationAndJakartaEnabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        Path person = Paths.get(outputPath + "/src/gen/java/org/openapitools/model/PersonWithEmail.java");
        assertFileContains(person, "@Email");
        assertFileContains(person, "import jakarta.validation.constraints.*;");
    }

    @Test
    public void doesNotGenerateEmailAnnotationWhenBeanValidationDisabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(false);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        Path person = Paths.get(outputPath + "/src/gen/java/org/openapitools/model/PersonWithEmail.java");
        Path api = Paths.get(outputPath + "/src/gen/java/org/openapitools/api/PersonApi.java");
        assertFileNotContains(person, "@Email");
        assertFileNotContains(api, "@Email");
    }

    @Test
    public void generatesEmailAnnotationOnQueryParameterWhenBeanValidationEnabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/issue_4876_format_email.yaml", null, new ParseOptions()).getOpenAPI();

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(true);

        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(input).generate();

        // a format: email query parameter is validated with @Email alongside @NotNull
        Path api = Paths.get(outputPath + "/src/gen/java/org/openapitools/api/PersonApi.java");
        assertFileContains(api, "import javax.validation.constraints.*;");
        assertFileContains(api, "@QueryParam(\"email\")", "@Email", "String email");
    }
}
