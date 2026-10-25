package org.openapitools.codegen.python;

import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;

public class PythonServerTestRunnerTest {

    @DataProvider
    public Object[][] serverGenerators() {
        return new Object[][]{
                {"python-flask"},
                {"python-aiohttp"},
                {"python-blueplanet"}
        };
    }

    @Test(dataProvider = "serverGenerators")
    public void testNoseIsNotAnAdvertisedOption(String generatorName) {
        assertFalse(CodegenConfigLoader.forName(generatorName).cliOptions().stream()
                .anyMatch(option -> "useNose".equals(option.getOpt())),
                generatorName + " must not advertise the removed test runner");
    }

    @DataProvider
    public Object[][] flaskConfigurations() {
        return new Object[][]{
                {false, null}, {false, false}, {false, "false"}, {false, true}, {false, "true"},
                {true, null}, {true, false}, {true, "false"}, {true, true}, {true, "true"}
        };
    }

    @Test(dataProvider = "flaskConfigurations")
    public void testFlaskAlwaysUsesPytest(boolean connexion3, Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-flask")
                .addAdditionalProperty("useConnexion3", connexion3), legacyOption);

        List<String> requirements = new ArrayList<>(Arrays.asList(
                "pytest~=7.1.0", "pytest-cov>=2.8.1", "pytest-randomly>=1.2.3"));
        if (!connexion3) {
            requirements.add("Flask-Testing==0.8.1");
        }
        assertEquals(Files.readAllLines(output.resolve("test-requirements.txt")), requirements);
        assertFileContains(output.resolve("tox.ini"), "pytest --cov=openapi_server");
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
    }

    @DataProvider
    public Object[][] legacyOptions() {
        return new Object[][]{{null}, {false}, {"false"}, {true}, {"true"}};
    }

    @Test(dataProvider = "legacyOptions")
    public void testAiohttpAlwaysUsesPytest(Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-aiohttp")
                .addAdditionalProperty("packageName", "petstore_api")
                .addAdditionalProperty("pythonSrcRoot", "src"), legacyOption);

        assertEquals(Files.readAllLines(output.resolve("test-requirements.txt")), Arrays.asList(
                "pytest~=7.1.0", "pytest-cov>=2.8.1", "pytest-randomly>=1.2.3",
                "pytest-aiohttp>=1.0.0", "pytest-asyncio>=0.17.2"));
        assertFileContains(output.resolve("tox.ini"), "pytest --cov=petstore_api", "asyncio_mode = auto");
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
        assertFileContains(output.resolve("tests/conftest.py"), "@pytest.fixture", "'petstore_api'");
    }

    @Test(dataProvider = "legacyOptions")
    public void testBluePlanetAlwaysUsesPytest(Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-blueplanet"), legacyOption);

        assertEquals(Files.readAllLines(output.resolve("app/test-requirements.txt")), Arrays.asList(
                "pytest~=4.6.7 # needed for python 2.7+3.4", "pytest-cov>=2.8.1",
                "pytest-randomly==1.2.3 # needed for python 2.7+3.4", "flask_testing==0.6.1"));
        assertFileContains(output.resolve("app/tox.ini"), "envlist = py35", "pytest --cov=openapi_server");
        assertFileNotContains(output.resolve("app/tox.ini"), "nosetests");
    }

    @Test(dataProvider = "flaskConfigurations")
    public void testFlaskCiInstallsAndRunsPytest(boolean connexion3, Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-flask")
                .addAdditionalProperty("useConnexion3", connexion3), legacyOption);

        assertFileContains(output.resolve(".travis.yml"),
                "pip install -r requirements.txt -r test-requirements.txt", "script: python -m pytest");
        assertFileNotContains(output.resolve(".travis.yml"), "nosetests");
    }

    @DataProvider
    public Object[][] sourceLayouts() {
        return new Object[][]{
                {"python-flask", false, "src/petstore_api"},
                {"python-flask", true, "src.petstore_api"},
                {"python-aiohttp", false, "petstore_api"},
                {"python-aiohttp", true, "src.petstore_api"}
        };
    }

    @Test(dataProvider = "sourceLayouts")
    public void testPytestPreservesSourceLayout(String generatorName, boolean sourceRootInImports,
                                               String coverageTarget) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .addAdditionalProperty("packageName", "petstore_api")
                .addAdditionalProperty("pythonSrcRoot", "src")
                .addAdditionalProperty("usePythonSrcRootInImports", sourceRootInImports)
                .addAdditionalProperty("testsUsePythonSrcRoot", true), "true");

        assertFileContains(output.resolve("tox.ini"), "pytest --cov=" + coverageTarget);
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
        assertFileExists(output.resolve("src/petstore_api/__main__.py"));
    }

    private Path generateSupportingFiles(CodegenConfigurator configurator, Object legacyOption) throws IOException {
        Path output = Files.createTempDirectory("python-server-test-runner-");
        configurator.setInputSpec("src/test/resources/2_0/petstore.yaml")
                .setOutputDir(output.toString())
                .addGlobalProperty("supportingFiles", "");
        if (legacyOption != null) {
            configurator.addAdditionalProperty("useNose", legacyOption);
        }
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        return output;
    }
}
