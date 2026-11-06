package org.openapitools.codegen.python;

import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
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
                {false, null}, {false, true}, {false, "true"},
                {true, null}, {true, true}, {true, "true"}
        };
    }

    @Test(dataProvider = "flaskConfigurations")
    public void testFlaskAlwaysUsesPytest(boolean connexion3, Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-flask")
                .addAdditionalProperty("useConnexion3", connexion3), legacyOption);

        Set<String> requirements = requirementNames(output.resolve("test-requirements.txt"));
        assertThat(requirements).contains("pytest", "pytest-cov", "pytest-randomly").doesNotContain("nose");
        assertThat(requirements.contains("flask-testing")).isEqualTo(!connexion3);
        assertFileContains(output.resolve("tox.ini"), "pytest --cov=openapi_server");
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
        assertFileContains(output.resolve(".travis.yml"),
                "pip install -r requirements.txt -r test-requirements.txt", "script: python -m pytest");
        assertFileNotContains(output.resolve(".travis.yml"), "nosetests");
    }

    @DataProvider
    public Object[][] legacyOptions() {
        return new Object[][]{{null}, {true}, {"true"}};
    }

    @Test(dataProvider = "legacyOptions")
    public void testAiohttpAlwaysUsesPytest(Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-aiohttp")
                .addAdditionalProperty("packageName", "petstore_api")
                .addAdditionalProperty("pythonSrcRoot", "src"), legacyOption);

        assertThat(requirementNames(output.resolve("test-requirements.txt")))
                .contains("pytest", "pytest-cov", "pytest-randomly", "pytest-aiohttp", "pytest-asyncio")
                .doesNotContain("nose");
        assertFileContains(output.resolve("tox.ini"), "pytest --cov=petstore_api", "asyncio_mode = auto");
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
    }

    @Test(dataProvider = "legacyOptions")
    public void testBluePlanetAlwaysUsesPytest(Object legacyOption) throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-blueplanet"), legacyOption);

        assertThat(requirementNames(output.resolve("app/test-requirements.txt")))
                .contains("pytest", "pytest-cov", "pytest-randomly", "flask-testing")
                .doesNotContain("nose");
        assertFileContains(output.resolve("app/tox.ini"), "pytest --cov=openapi_server");
        assertFileNotContains(output.resolve("app/tox.ini"), "nosetests");
    }

    @Test
    public void testPytestCoverageUsesSourceRoot() throws IOException {
        Path output = generateSupportingFiles(new CodegenConfigurator()
                .setGeneratorName("python-flask")
                .addAdditionalProperty("packageName", "petstore_api")
                .addAdditionalProperty("pythonSrcRoot", "src"), "true");

        assertFileContains(output.resolve("tox.ini"), "pytest --cov=src/petstore_api");
        assertFileNotContains(output.resolve("tox.ini"), "nosetests");
    }

    private Set<String> requirementNames(Path requirements) throws IOException {
        return Files.readAllLines(requirements).stream()
                .map(String::trim)
                .filter(line -> !line.isEmpty() && !line.startsWith("#"))
                .map(line -> line.split("[<>=!~;\\s]", 2)[0].replace('_', '-').toLowerCase(Locale.ROOT))
                .collect(Collectors.toSet());
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
