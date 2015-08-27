package io.swagger.codegen.cmd;

import config.Config;
import config.ConfigParser;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.SwaggerCodegen;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.models.auth.AuthorizationValue;
import io.swagger.parser.SwaggerParser;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Injectable;
import mockit.Mocked;
import mockit.StrictExpectations;
import org.apache.commons.lang3.ArrayUtils;
import org.testng.annotations.Test;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class GenerateTest {

    @Mocked
    SwaggerParser parser;

    @Injectable
    Swagger swagger;

    @Mocked
    DefaultGenerator defaultGenerator;

    @Mocked
    CodegenConfigLoader codegenConfigLoader;

    @Mocked
    ClientOptInput clientOptInput;

    @Injectable
    List<AuthorizationValue> authorizationValues;

    @Test
    public void testVerbose_ShortArg() throws Exception {
        doVerboseTest("-v");
    }

    @Test
    public void testVerbose_LongArg() throws Exception {
        doVerboseTest("--verbose");
    }

    @Test
    public void testRequiredArgs_ShortArgs() throws Exception {
        doRequiredArgsTest("-l", "-o", "-i");
    }

    @Test
    public void testRequiredArgs_LongArgs() throws Exception {
        doRequiredArgsTest("--lang", "--output", "--input-spec");
    }

    @Test
    public void testTemplateDir() throws Exception {
        final String absolutePath = new File("src").getAbsolutePath();

        doSingleAdditionalPropertyTest("--template-dir", CodegenConstants.TEMPLATE_DIR, "src", absolutePath);
        doSingleAdditionalPropertyTest("--template-dir", CodegenConstants.TEMPLATE_DIR, absolutePath, absolutePath);
        doSingleAdditionalPropertyTest("-t", CodegenConstants.TEMPLATE_DIR, "src", absolutePath);
        doSingleAdditionalPropertyTest("-t", CodegenConstants.TEMPLATE_DIR, absolutePath, absolutePath);
    }

    @Test
    public void testAuth() throws Exception {

        final String auth = "hello:world";

        new StrictExpectations() {{
            clientOptInput.setAuth(auth);
            times = 1;
        }};

        setupAndRunGenericTest("-a", auth);

        new StrictExpectations() {{
            clientOptInput.setAuth(auth);
            times = 1;
        }};

        setupAndRunGenericTest("--auth", auth);
    }

    @Test
    public void testSystemProperties() throws Exception {

        new StrictExpectations(System.class) {{
            System.setProperty("hello", "world");
            times = 1;
            System.setProperty("foo", "bar");
            times = 1;
        }};

        setupAndRunGenericTest("-D", "hello=world,foo=bar");
    }

    @Test
    public void testConfig(@Mocked final ConfigParser parser) throws Exception {

        final String configFilePath = "config.json";
        final String invokerPackage = "com.foo.bar.invoker";
        final String groupId = "com.foo.bar";
        Map<String, String> configMap = new HashMap<String, String>();
        configMap.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        configMap.put(CodegenConstants.GROUP_ID, groupId);
        final Config config = new Config(configMap);

        final String[] configArgs = {"-c", "--config"};

        for (String configArg : configArgs) {
            new StrictExpectations() {{
                parser.read(configFilePath);
                times = 1;
                result = config;

            }};

            final CodegenConfig codegenConfig = setupAndRunGenericTest(configArg, configFilePath);

            assertValueInMap(codegenConfig.additionalProperties(), CodegenConstants.INVOKER_PACKAGE, invokerPackage);
            assertValueInMap(codegenConfig.additionalProperties(), CodegenConstants.GROUP_ID, groupId);
        }
    }

    @Test
    public void testSkipOverwrite() throws Exception {

        CodegenConfig codegenConfig1 = setupAndRunGenericTest();
        assertFalse(codegenConfig1.isSkipOverwrite());

        CodegenConfig codegenConfig2 = setupAndRunGenericTest("-s");
        assertTrue(codegenConfig2.isSkipOverwrite());

        CodegenConfig codegenConfig3 = setupAndRunGenericTest("--skip-overwrite");
        assertTrue(codegenConfig3.isSkipOverwrite());
    }

    @Test
    public void testApiPackage() throws Exception {
        doSingleAdditionalPropertyTest("--api-package", CodegenConstants.API_PACKAGE, "io.foo.bar.api");
    }

    @Test
    public void testModelPackage() throws Exception {
        doSingleAdditionalPropertyTest("--model-package", CodegenConstants.MODEL_PACKAGE, "io.foo.bar.models");
    }

    @Test
    public void testInstantiationTypes() throws Exception {

        final CodegenConfig codegenConfig = setupAndRunGenericTest("--instantiation-types", "foo=bar,hello=world");

        assertValueInMap(codegenConfig.instantiationTypes(), "foo", "bar");
        assertValueInMap(codegenConfig.instantiationTypes(), "hello", "world");
    }

    @Test
    public void testTypeMappings() throws Exception {
        final CodegenConfig codegenConfig = setupAndRunGenericTest("--type-mappings", "foo=bar,hello=world");

        assertValueInMap(codegenConfig.typeMapping(), "foo", "bar");
        assertValueInMap(codegenConfig.typeMapping(), "hello", "world");
    }

    @Test
    public void testAdditionalProperties() throws Exception {
        final CodegenConfig codegenConfig = setupAndRunGenericTest("--additional-properties", "foo=bar,hello=world");

        assertValueInMap(codegenConfig.additionalProperties(), "foo", "bar");
        assertValueInMap(codegenConfig.additionalProperties(), "hello", "world");
    }

    @Test
    public void testLanguageSpecificPrimitives() throws Exception {
        final CodegenConfig codegenConfig = setupAndRunGenericTest("--language-specific-primitives", "foo,bar,hello,world");

        final Set<String> languageSpecificPrimitives = codegenConfig.languageSpecificPrimitives();

        assertTrue(languageSpecificPrimitives.contains("foo"));
        assertTrue(languageSpecificPrimitives.contains("bar"));
        assertTrue(languageSpecificPrimitives.contains("hello"));
        assertTrue(languageSpecificPrimitives.contains("world"));
    }

    @Test
    public void testImportMappings() throws Exception {
        final CodegenConfig codegenConfig = setupAndRunGenericTest("--import-mappings", "foo=bar,hello=world");

        assertValueInMap(codegenConfig.importMapping(), "foo", "bar");
        assertValueInMap(codegenConfig.importMapping(), "hello", "world");
    }

    @Test
    public void testInvokerPackage() throws Exception {
        doSingleAdditionalPropertyTest("--invoker-package", CodegenConstants.INVOKER_PACKAGE, "io.foo.bar.invoker");
    }

    @Test
    public void testGroupId() throws Exception {
        doSingleAdditionalPropertyTest("--group-id", CodegenConstants.GROUP_ID, "io.foo.bar");
    }

    @Test
    public void testArtifactId() throws Exception {
        doSingleAdditionalPropertyTest("--artifact-id", CodegenConstants.ARTIFACT_ID, "awesome-api");
    }

    @Test
    public void testArtifactVersion() throws Exception {
        doSingleAdditionalPropertyTest("--artifact-version", CodegenConstants.ARTIFACT_VERSION, "1.2.3");
    }

    private void doVerboseTest(String verboseFlag) {
        new StrictExpectations(System.class) {{
            System.setProperty("debugSwagger", "");
            times = 1;
            System.setProperty("debugModels", "");
            times = 1;
            System.setProperty("debugOperations", "");
            times = 1;
            System.setProperty("debugSupportingFiles", "");
            times = 1;
        }};

        setupAndRunGenericTest(verboseFlag);
    }

    private void doRequiredArgsTest(String langFlag, String outputDirFlag, String inputSpecFlag) {
        final String spec = "swagger.yaml";
        final String lang = "java";
        final String outputDir = "src/main/java";

        final String[] args = {"generate", langFlag, lang, outputDirFlag, outputDir, inputSpecFlag, spec};

        final CodegenConfig config = new JavaClientCodegen();

        setupStandardExpectations(spec, lang, config);

        SwaggerCodegen.main(args);

        new FullVerifications() {{
        }};

        assertEquals(config.getOutputDir(), new File(outputDir).getAbsolutePath());
    }

    private void doSingleAdditionalPropertyTest(String cliArg, String additionalPropertyKey, String expectedValue) {
        doSingleAdditionalPropertyTest(cliArg, additionalPropertyKey, expectedValue, expectedValue);
    }

    private void doSingleAdditionalPropertyTest(String cliArg, String additionalPropertyKey, String cliValue, String additionalPropertyValue) {

        final CodegenConfig config = setupAndRunGenericTest(cliArg, cliValue);

        assertValueInMap(config.additionalProperties(), additionalPropertyKey, additionalPropertyValue);
    }

    private CodegenConfig setupAndRunGenericTest(String... additionalParameters) {

        final String spec = "swagger.yaml";
        final String lang = "java";

        final String[] commonArgs = {"generate", "-l", lang, "-o", "path/to/some/directory", "-i", spec};

        String[] argsToUse = ArrayUtils.addAll(commonArgs, additionalParameters);

        final CodegenConfig config = new JavaClientCodegen();

        setupStandardExpectations(spec, lang, config);

        SwaggerCodegen.main(argsToUse);

        new FullVerifications() {{
        }};

        return config;
    }

    private void assertValueInMap(Map map, String propertyKey, String expectedPropertyValue) {
        assertTrue(map.containsKey(propertyKey));
        assertEquals(map.get(propertyKey), expectedPropertyValue);
    }

    private void setupStandardExpectations(final String spec, final String languageName, final CodegenConfig config) {

        new Expectations() {{
            CodegenConfigLoader.forName(languageName);
            times = 1;
            result = config;

            new ClientOptInput();
            times = 1;
            result = clientOptInput;

            clientOptInput.config(config);
            times = 1;
            result = clientOptInput;

            new SwaggerParser();
            times = 1;
            result = parser;

            clientOptInput.getAuthorizationValues();
            times = 1;
            result = authorizationValues;

            parser.read(spec, authorizationValues, true);
            times = 1;
            result = swagger;

            new DefaultGenerator();
            times = 1;
            result = defaultGenerator;

            clientOptInput.opts((ClientOpts) any);
            times = 1;
            result = clientOptInput;

            clientOptInput.swagger(swagger);
            times = 1;
            result = clientOptInput;

            defaultGenerator.opts(clientOptInput);
            times = 1;
            result = defaultGenerator;

            defaultGenerator.generate();
            times = 1;
        }};
    }

}
