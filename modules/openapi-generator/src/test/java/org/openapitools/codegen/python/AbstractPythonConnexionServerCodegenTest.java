package org.openapitools.codegen.python;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.assertj.core.api.Assertions.assertThat;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen.*;

public class AbstractPythonConnexionServerCodegenTest {

    /**
     * Passing "description" as first param to succinctly identify the significance of test parameters.
     */
    @Test(dataProvider = "data")
    public void test(String description, Map<String, Object> additionalProperties, String modelName,
                     ExpectedValues expectedValues) {
        AbstractPythonConnexionServerCodegen codegen = new MockAbstractPythonConnexionServerCodegen("", false);
        codegen.additionalProperties().putAll(additionalProperties);
        codegen.processOpts();
        String pythonSrcRoot = Objects.toString(codegen.additionalProperties().get(PYTHON_SRC_ROOT), null);
        assertThat(pythonSrcRoot).isEqualTo(expectedValues.pythonSrcRoot);
        assertThat(codegen.apiPackage()).isEqualTo(expectedValues.expectedApiPackage);
        assertThat(codegen.modelFileFolder()).isEqualTo(expectedValues.expectedModelFileFolder);
        assertThat(codegen.apiFileFolder()).isEqualTo(expectedValues.expectedApiFileFolder);
        assertThat(codegen.apiTestFileFolder()).isEqualTo(expectedValues.expectedApiTestFileFolder);
        assertThat(codegen.toModelImport(modelName)).isEqualTo(expectedValues.expectedImport);
    }

    @DataProvider
    public Object[][] data() {
        return new Object[][]{
                new Object[]{
                        "Default setup",
                        Collections.emptyMap(),
                        "TestModel",
                        new ExpectedValues("from openapi_server.models.test_model import TestModel",
                                "openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Default setup with Python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root"),
                        "TestModel",
                        new ExpectedValues("from openapi_server.models.test_model import TestModel",
                                "openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                "test_root")
                },
                new Object[]{
                        "Python src in import",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root", USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.openapi_server.models.test_model import TestModel",
                                "test_root.openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import and tests under python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root", USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                MOVE_TESTS_UNDER_PYTHON_SRC_ROOT, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.openapi_server.models.test_model import TestModel",
                                "test_root.openapi_server.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "openapi_server", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import with specified package",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root",
                                USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                CodegenConstants.PACKAGE_NAME, "test_package"),
                        "TestModel",
                        new ExpectedValues("from test_root.test_package.models.test_model import TestModel",
                                "test_root.test_package.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test"),
                                null)
                },
                new Object[]{
                        "Python src in import with specified package and tests under python src root",
                        ImmutableMap.of(PYTHON_SRC_ROOT, "test_root",
                                USE_PYTHON_SRC_ROOT_IN_IMPORTS, "true",
                                CodegenConstants.PACKAGE_NAME, "test_package",
                                MOVE_TESTS_UNDER_PYTHON_SRC_ROOT, "true"),
                        "TestModel",
                        new ExpectedValues("from test_root.test_package.models.test_model import TestModel",
                                "test_root.test_package.controllers",
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "models"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test_package", "controllers"),
                                platformAgnosticPath("generated-code", "connexion", "test_root", "test"),
                                null)
                }
        };
    }

    private static class MockAbstractPythonConnexionServerCodegen extends AbstractPythonConnexionServerCodegen {
        public MockAbstractPythonConnexionServerCodegen(String templateDirectory, boolean fixBodyNameValue) {
            super(templateDirectory, fixBodyNameValue);
        }
    }

    private static class ExpectedValues {
        public final String expectedImport;
        public final String expectedApiPackage;
        public final String expectedModelFileFolder;
        public final String expectedApiFileFolder;
        public final String expectedApiTestFileFolder;
        public final String pythonSrcRoot;

        public ExpectedValues(String expectedImport, String expectedApiPackage, String expectedModelFileFolder,
                              String expectedApiFileFolder, String expectedApiTestFileFolder, String pythonSrcRoot) {
            this.expectedImport = expectedImport;
            this.expectedApiPackage = expectedApiPackage;
            this.expectedModelFileFolder = expectedModelFileFolder;
            this.expectedApiFileFolder = expectedApiFileFolder;
            this.expectedApiTestFileFolder = expectedApiTestFileFolder;
            this.pythonSrcRoot = pythonSrcRoot != null ? pythonSrcRoot + File.separatorChar : null;
        }
    }

    private static String platformAgnosticPath(String... nodes) {
        return StringUtils.join(nodes, File.separatorChar);
    }

    private static Path generateController(String generatorName, Map<String, Object> additionalProperties, String... controllerPathSegments) throws IOException {
        File output = Files.createTempDirectory("test-" + generatorName).toFile().getCanonicalFile();
        output.deleteOnExit();

        CodegenConfigurator config = new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setInputSpec("src/test/resources/3_0/python/connexion-param-sources.yaml")
                .setOutputDir(output.getAbsolutePath());
        for (Map.Entry<String, Object> entry : additionalProperties.entrySet()) {
            config.addAdditionalProperty(entry.getKey(), entry.getValue());
        }

        List<File> files = new DefaultGenerator().opts(config.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path controller = Paths.get(output.getAbsolutePath(), controllerPathSegments);
        assertFileExists(controller);
        return controller;
    }

    @Test(description = "header parameters are read from request headers rather than declared as Connexion handler arguments")
    public void testHeaderParamsReadFromRequestHeadersInConnexionControllers() throws IOException {
        Path aiohttpController = generateController(
                "python-aiohttp",
                Collections.emptyMap(),
                "openapi_server", "controllers", "default_controller.py");
        assertFileContains(
                aiohttpController,
                "async def create_item(request: web.Request, filter_param, body) -> web.Response:",
                "x_req_header = request.headers.get('X-Req-Header')");
        assertFileNotContains(aiohttpController, "async def create_item(request: web.Request, x_req_header");

        Path blueplanetController = generateController(
                "python-blueplanet",
                Collections.emptyMap(),
                "app", "openapi_server", "controllers", "default_controller.py");
        assertFileContains(
                blueplanetController,
                "def create_item(filter_param, body):  # noqa: E501",
                "x_req_header = connexion.request.headers.get('X-Req-Header')");
        assertFileNotContains(blueplanetController, "def create_item(x_req_header");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskController = generateController(
                    "python-flask",
                    props,
                    "openapi_server", "controllers", "default_controller.py");
            assertFileContains(
                    flaskController,
                    "def create_item(filter_param, body):  # noqa: E501",
                    "x_req_header = flask.request.headers.get('X-Req-Header')");
            assertFileNotContains(flaskController, "def create_item(x_req_header");
        }
    }

    @Test(description = "non-body model parameters are deserialized from their argument dict rather than overwritten from request.get_json()")
    public void testNonBodyModelParamsNotOverwrittenFromRequestBody() throws IOException {
        Path aiohttpController = generateController(
                "python-aiohttp",
                Collections.emptyMap(),
                "openapi_server", "controllers", "default_controller.py");
        assertFileContains(
                aiohttpController,
                "if isinstance(filter_param, dict):",
                "filter_param = QueryFilter.from_dict(filter_param)",
                "body = BodyPayload.from_dict(body)");
        assertFileNotContains(aiohttpController, "= .from_dict(");

        Path blueplanetController = generateController(
                "python-blueplanet",
                Collections.emptyMap(),
                "app", "openapi_server", "controllers", "default_controller.py");
        assertFileContains(
                blueplanetController,
                "if isinstance(filter_param, dict):",
                "filter_param = QueryFilter.from_dict(filter_param)  # noqa: E501",
                "body = BodyPayload.from_dict(connexion.request.get_json())  # noqa: E501");
        assertFileNotContains(blueplanetController, "QueryFilter.from_dict(connexion.request.get_json())");
        assertFileNotContains(blueplanetController, "= .from_dict(");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskController = generateController(
                    "python-flask",
                    props,
                    "openapi_server", "controllers", "default_controller.py");
            assertFileContains(
                    flaskController,
                    "if isinstance(filter_param, dict):",
                    "filter_param = QueryFilter.from_dict(filter_param)  # noqa: E501",
                    "body_payload = body",
                    "body_payload = BodyPayload.from_dict(flask.request.get_json())  # noqa: E501");
            assertFileNotContains(flaskController, "QueryFilter.from_dict(flask.request.get_json())");
        }
    }

}
