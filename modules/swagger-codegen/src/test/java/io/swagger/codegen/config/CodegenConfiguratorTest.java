package io.swagger.codegen.config;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.auth.AuthParser;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.models.auth.AuthorizationValue;
import io.swagger.parser.SwaggerParser;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Injectable;
import mockit.Mocked;
import mockit.StrictExpectations;
import mockit.Tested;
import org.apache.commons.lang3.SerializationUtils;
import org.testng.annotations.Test;

import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

@SuppressWarnings("static-method")
public class CodegenConfiguratorTest {

    @Mocked
    SwaggerParser parser;

    @Mocked
    AuthParser authParser;

    @Injectable
    Swagger swagger;

    @Mocked
    CodegenConfigLoader codegenConfigLoader;

    @Injectable
    List<AuthorizationValue> authorizationValues;

    @Tested
    CodegenConfigurator configurator;

    @SuppressWarnings("unused")
    @Test
    public void testVerbose() throws Exception {

        configurator.setVerbose(true);

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

        setupAndRunGenericTest(configurator);
    }

    @Test
    public void testTemplateDir() throws Exception {

        final String templateDir = "src/test/resources";
        configurator.setTemplateDir(templateDir);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.TEMPLATE_DIR, toAbsolutePathDir(templateDir));
    }

    @SuppressWarnings("unused")
    @Test
    public void testSystemProperties() throws Exception {

        configurator.addSystemProperty("hello", "world")
                .addSystemProperty("foo", "bar");

        new Expectations(System.class) {{
            System.setProperty("hello", "world");
            times = 1;
            System.setProperty("foo", "bar");
            times = 1;
        }};

        setupAndRunGenericTest(configurator);
    }

    @Test
    public void testSkipOverwrite() throws Exception {
        CodegenConfigurator configurator1 = new CodegenConfigurator();
        configurator1.setSkipOverwrite(true);

        ClientOptInput clientOptInput = setupAndRunGenericTest(configurator1);
        assertTrue(clientOptInput.getConfig().isSkipOverwrite());

        CodegenConfigurator configurator2 = new CodegenConfigurator();
        configurator1.setSkipOverwrite(true);

        clientOptInput = setupAndRunGenericTest(configurator2);
        assertFalse(clientOptInput.getConfig().isSkipOverwrite());
    }

    @Test
    public void testApiPackage() throws Exception {
        final String apiPackage = "io.foo.bar.api";
        configurator.setApiPackage(apiPackage);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.API_PACKAGE, apiPackage);
    }


    @Test
    public void testModelPackage() throws Exception {
        final String modelPackage = "io.foo.bar.models";
        configurator.setModelPackage(modelPackage);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.MODEL_PACKAGE, modelPackage);
    }

    @Test
    public void testInstantiationTypes() throws Exception {

        configurator.addInstantiationType("foo", "bar")
                .addInstantiationType("hello", "world");

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().instantiationTypes(), "foo", "bar");
        assertValueInMap(clientOptInput.getConfig().instantiationTypes(), "hello", "world");
    }

    @Test
    public void testTypeMappings() throws Exception {

        configurator.addTypeMapping("foo", "bar")
                .addTypeMapping("hello", "world");

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().typeMapping(), "foo", "bar");
        assertValueInMap(clientOptInput.getConfig().typeMapping(), "hello", "world");
    }

    @Test
    public void testAdditionalProperties() throws Exception {

        configurator.addAdditionalProperty("foo", "bar")
                .addAdditionalProperty("hello", "world")
                .addAdditionalProperty("supportJava6", false)
                .addAdditionalProperty("useRxJava", true);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "foo", "bar");
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "hello", "world");
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "supportJava6", false);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "useRxJava", true);
    }

    @Test
    public void testLanguageSpecificPrimitives() throws Exception {

        configurator.addLanguageSpecificPrimitive("foo")
                .addLanguageSpecificPrimitive("bar")
                .addLanguageSpecificPrimitive("hello")
                .addLanguageSpecificPrimitive("world");

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertTrue(clientOptInput.getConfig().languageSpecificPrimitives().contains("foo"));
        assertTrue(clientOptInput.getConfig().languageSpecificPrimitives().contains("bar"));
        assertTrue(clientOptInput.getConfig().languageSpecificPrimitives().contains("hello"));
        assertTrue(clientOptInput.getConfig().languageSpecificPrimitives().contains("world"));
    }

    @Test
    public void testImportMappings() throws Exception {

        configurator.addImportMapping("foo", "bar")
                .addImportMapping("hello", "world");

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().importMapping(), "foo", "bar");
        assertValueInMap(clientOptInput.getConfig().importMapping(), "hello", "world");
    }

    @Test
    public void testInvokerPackage() throws Exception {
        final String invokerPackage = "io.foo.bar.models";
        configurator.setInvokerPackage(invokerPackage);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.INVOKER_PACKAGE, invokerPackage);
    }

    @Test
    public void testGroupId() throws Exception {
        final String expectedValue = "io.foo.bar.models";
        configurator.setGroupId(expectedValue);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.GROUP_ID, expectedValue);
    }

    @Test
    public void testArtifactId() throws Exception {
        final String expectedValue = "io.foo.bar.models";
        configurator.setArtifactId(expectedValue);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.ARTIFACT_ID, expectedValue);
    }

    @Test
    public void testArtifactVersion() throws Exception {
        final String expectedValue = "1.2.3";
        configurator.setArtifactVersion(expectedValue);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.ARTIFACT_VERSION, expectedValue);
    }

    @Test
    public void testLibrary() throws Exception {
        final String expectedValue = "jersey2";

        configurator.setLibrary(expectedValue);
        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertEquals(clientOptInput.getConfig().getLibrary(), expectedValue);
    }

    @Test
    public void testDynamicProperties() throws Exception {
        configurator.addDynamicProperty(CodegenConstants.LOCAL_VARIABLE_PREFIX, "_");
        configurator.addDynamicProperty("supportJava6", false);
        configurator.addDynamicProperty("useRxJava", true);

        final ClientOptInput clientOptInput = setupAndRunGenericTest(configurator);

        assertValueInMap(clientOptInput.getConfig().additionalProperties(), CodegenConstants.LOCAL_VARIABLE_PREFIX, "_");
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "supportJava6", false);
        assertValueInMap(clientOptInput.getConfig().additionalProperties(), "useRxJava", true);
    }

    @Test
    public void testFromFile() throws Exception {
        final CodegenConfigurator configurator = CodegenConfigurator.fromFile("src/test/resources/sampleConfig.json");

        assertEquals(configurator.getLang(), "java");
        assertEquals(configurator.getInputSpec(), "swagger.yaml");
        assertEquals(configurator.getOutputDir(), toAbsolutePathDir("src/gen/java"));
        assertEquals(configurator.isVerbose(), true);
        assertEquals(configurator.isSkipOverwrite(), true);
        assertEquals(configurator.getTemplateDir(), toAbsolutePathDir("src/main/resources"));
        assertEquals(configurator.getAuth(), "hello:world");
        assertEquals(configurator.getApiPackage(), "io.something.api");
        assertEquals(configurator.getModelPackage(), "io.something.models");
        assertEquals(configurator.getInvokerPackage(), "io.something.invoker");
        assertEquals(configurator.getGroupId(), "io.something");
        assertEquals(configurator.getArtifactId(), "awesome-api");
        assertEquals(configurator.getArtifactVersion(), "1.2.3");
        assertEquals(configurator.getLibrary(), "jersey2");

        assertEquals(configurator.getSystemProperties().size(), 1);
        assertValueInMap(configurator.getSystemProperties(), "systemProp1", "value1");

        assertEquals(configurator.getInstantiationTypes().size(), 1);
        assertValueInMap(configurator.getInstantiationTypes(), "hello", "world");

        assertEquals(configurator.getTypeMappings().size(), 1);
        assertValueInMap(configurator.getTypeMappings(), "foo", "bar");

        assertEquals(configurator.getAdditionalProperties().size(), 1);
        assertValueInMap(configurator.getAdditionalProperties(), "addtProp1", "value2");

        assertEquals(configurator.getImportMappings().size(), 1);
        assertValueInMap(configurator.getImportMappings(), "type1", "import1");


        assertEquals(configurator.getLanguageSpecificPrimitives().size(), 1);
        assertTrue(configurator.getLanguageSpecificPrimitives().contains("rolex"));

        assertEquals(configurator.getDynamicProperties().size(), 1);
        assertValueInMap(configurator.getDynamicProperties(), CodegenConstants.LOCAL_VARIABLE_PREFIX, "_");

        assertEquals(configurator.getIgnoreFileOverride(), "/path/to/override/.swagger-codegen-ignore");
    }

    @Test
    public void testCodegenConfiguratorIsSerializable() {
        final CodegenConfigurator configurator = CodegenConfigurator.fromFile("src/test/resources/sampleConfig.json");
        // Simply ensure that the object can be serialized
        SerializationUtils.serialize(configurator);
    }

    @SuppressWarnings("unused")
    private ClientOptInput setupAndRunGenericTest(CodegenConfigurator configurator) {

        final String spec = "swagger.yaml";
        final String lang = "java";
        final String outputDir = "src/test/java";
        final String expectedAuth = "hello:world";


        configurator.setLang(lang)
                .setOutputDir(outputDir)
                .setInputSpec(spec)
                .setAuth(expectedAuth);

        final CodegenConfig config = new JavaClientCodegen();

        setupStandardExpectations(spec, lang, configurator.getAuth(), config);

        ClientOptInput result = configurator.toClientOptInput();

        new FullVerifications() {{
        }};

        final String expectedOutputDir = toAbsolutePathDir(outputDir);

        assertEquals(result.getConfig().getOutputDir(), expectedOutputDir);

        return result;
    }

    private static String toAbsolutePathDir(String outputDir) {
        return Paths.get(outputDir).toAbsolutePath().toAbsolutePath().toString();
    }

    @SuppressWarnings("unused")
    private void setupStandardExpectations(final String spec, final String languageName, final String auth, final CodegenConfig config) {

        new StrictExpectations() {{
            CodegenConfigLoader.forName(languageName);
            times = 1;
            result = config;

            AuthParser.parse(auth); times=1; result = authorizationValues;

            new SwaggerParser();
            times = 1;
            result = parser;

            parser.read(spec, authorizationValues, true);
            times = 1;
            result = swagger;

        }};
    }

    private static void assertValueInMap(Map<?, ?> map, String propertyKey, Object expectedPropertyValue) {
        assertTrue(map.containsKey(propertyKey));
        assertEquals(map.get(propertyKey), expectedPropertyValue);
    }

}
