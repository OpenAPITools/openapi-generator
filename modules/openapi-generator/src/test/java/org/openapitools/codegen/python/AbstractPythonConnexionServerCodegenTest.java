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

    private static Path generateOutputDir(String generatorName, Map<String, Object> additionalProperties) throws IOException {
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

        return output.toPath();
    }

    @Test(description = "header and cookie parameters are read from request with type conversion and defaults rather than declared as Connexion handler arguments")
    public void testHeaderParamsReadFromRequestHeadersInConnexionControllers() throws IOException {
        Path aiohttpDir = generateOutputDir("python-aiohttp", Collections.emptyMap());
        Path aiohttpController = aiohttpDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path aiohttpTest = aiohttpDir.resolve(Paths.get("tests", "test_default_controller.py"));
        assertFileExists(aiohttpController);
        assertFileExists(aiohttpTest);
        assertFileContains(
                aiohttpController,
                "import json",
                "async def create_item(request: web.Request, filter_param, body, date_list=None, model_map=None) -> web.Response:",
                "x_req_header = request.headers.get('X-Req-Header')",
                "x_int_header = request.headers.get('X-Int-Header', 7)",
                "x_int_header = int(x_int_header)",
                "x_num_header = request.headers.get('X-Num-Header', 1.5)",
                "x_num_header = float(x_num_header)",
                "x_bool_header = request.headers.get('X-Bool-Header', True)",
                "x_bool_header = x_bool_header.lower() == 'true'",
                "x_json_header = request.headers.get('X-Json-Header')",
                "x_json_header = json.loads(x_json_header)",
                "x_json_header = HeaderArg.from_dict(x_json_header)",
                "x_tag_list = request.headers.get('X-Tag-List', ['foo', 'bar'])",
                "x_tag_list = x_tag_list.split(',')",
                "x_flag_list = request.headers.get('X-Flag-List', [True, False])",
                "x_flag_list = [x.lower() == 'true' if isinstance(x, str) else bool(x) for x in x_flag_list]",
                "x_date_list = request.headers.get('X-Date-List')",
                "x_date_list = x_date_list.split(',')",
                "if isinstance(x_date_list, list):",
                "x_date_list = [util.deserialize_date(s) for s in x_date_list]",
                "session_token = request.cookies.get('session_token', 'default-cookie')",
                "ids = request.cookies.get('ids', [1, 2])",
                "ids = ids.split(',')",
                "ids = [int(x) for x in ids]",
                "json_cookie = request.cookies.get('json_cookie')",
                "json_cookie = json.loads(json_cookie)",
                "json_cookie = HeaderArg.from_dict(json_cookie)");
        assertFileNotContains(aiohttpController, "async def create_item(request: web.Request, x_req_header");
        assertFileNotContains(aiohttpController, "session_token=None");
        assertFileNotContains(aiohttpController, "[foo, bar]");
        assertFileNotContains(aiohttpController, "[true, false]");
        assertFileContains(aiohttpTest, "'X-Req-Header':");
        assertFileNotContains(aiohttpTest, "'x_req_header':");

        Path blueplanetDir = generateOutputDir("python-blueplanet", Collections.emptyMap());
        Path blueplanetController = blueplanetDir.resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        Path blueplanetTest = blueplanetDir.resolve(Paths.get("app", "openapi_server", "test", "test_default_controller.py"));
        assertFileExists(blueplanetController);
        assertFileExists(blueplanetTest);
        assertFileContains(
                blueplanetController,
                "import json",
                "def create_item(filter_param, body, date_list=None, model_map=None):  # noqa: E501",
                "x_req_header = connexion.request.headers.get('X-Req-Header')",
                "x_int_header = connexion.request.headers.get('X-Int-Header', 7)",
                "x_int_header = int(x_int_header)",
                "x_num_header = connexion.request.headers.get('X-Num-Header', 1.5)",
                "x_num_header = float(x_num_header)",
                "x_bool_header = connexion.request.headers.get('X-Bool-Header', True)",
                "x_bool_header = x_bool_header.lower() == 'true'",
                "x_json_header = connexion.request.headers.get('X-Json-Header')",
                "x_json_header = json.loads(x_json_header)",
                "x_json_header = HeaderArg.from_dict(x_json_header)  # noqa: E501",
                "x_tag_list = connexion.request.headers.get('X-Tag-List', ['foo', 'bar'])",
                "x_tag_list = x_tag_list.split(',')",
                "x_flag_list = connexion.request.headers.get('X-Flag-List', [True, False])",
                "x_flag_list = [x.lower() == 'true' if isinstance(x, str) else bool(x) for x in x_flag_list]",
                "session_token = connexion.request.cookies.get('session_token', 'default-cookie')",
                "ids = connexion.request.cookies.get('ids', [1, 2])",
                "ids = ids.split(',')",
                "ids = [int(x) for x in ids]",
                "json_cookie = connexion.request.cookies.get('json_cookie')",
                "json_cookie = json.loads(json_cookie)",
                "json_cookie = HeaderArg.from_dict(json_cookie)  # noqa: E501");
        assertFileNotContains(blueplanetController, "def create_item(x_req_header");
        assertFileNotContains(blueplanetController, "session_token=None");
        assertFileNotContains(blueplanetController, "[foo, bar]");
        assertFileNotContains(blueplanetController, "[true, false]");
        assertFileContains(blueplanetTest, "('X-Req-Header',");
        assertFileNotContains(blueplanetTest, "('x_req_header',");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskDir = generateOutputDir("python-flask", props);
            Path flaskController = flaskDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
            Path flaskTest = flaskDir.resolve(Paths.get("openapi_server", "test", "test_default_controller.py"));
            assertFileExists(flaskController);
            assertFileExists(flaskTest);
            assertFileContains(
                    flaskController,
                    "import json",
                    "def create_item(filter_param, body, date_list=None, model_map=None):  # noqa: E501",
                    "x_req_header = flask.request.headers.get('X-Req-Header')",
                    "x_int_header = flask.request.headers.get('X-Int-Header', 7)",
                    "x_int_header = int(x_int_header)",
                    "x_num_header = flask.request.headers.get('X-Num-Header', 1.5)",
                    "x_num_header = float(x_num_header)",
                    "x_bool_header = flask.request.headers.get('X-Bool-Header', True)",
                    "x_bool_header = x_bool_header.lower() == 'true'",
                    "x_json_header = flask.request.headers.get('X-Json-Header')",
                    "x_json_header = json.loads(x_json_header)",
                    "x_json_header = HeaderArg.from_dict(x_json_header)  # noqa: E501",
                    "x_tag_list = flask.request.headers.get('X-Tag-List', ['foo', 'bar'])",
                    "x_tag_list = x_tag_list.split(',')",
                    "x_flag_list = flask.request.headers.get('X-Flag-List', [True, False])",
                    "x_flag_list = [x.lower() == 'true' if isinstance(x, str) else bool(x) for x in x_flag_list]",
                    "session_token = flask.request.cookies.get('session_token', 'default-cookie')",
                    "ids = flask.request.cookies.get('ids', [1, 2])",
                    "ids = ids.split(',')",
                    "ids = [int(x) for x in ids]",
                    "json_cookie = flask.request.cookies.get('json_cookie')",
                    "json_cookie = json.loads(json_cookie)",
                    "json_cookie = HeaderArg.from_dict(json_cookie)  # noqa: E501");
            assertFileNotContains(flaskController, "def create_item(x_req_header");
            assertFileNotContains(flaskController, "session_token=None");
            assertFileNotContains(flaskController, "[foo, bar]");
            assertFileNotContains(flaskController, "[true, false]");
            assertFileContains(flaskTest, "'X-Req-Header':");
            assertFileNotContains(flaskTest, "'x_req_header':");
        }
    }

    @Test(description = "non-body model, array, and map parameters are deserialized from their argument value rather than overwritten from request.get_json()")
    public void testNonBodyModelParamsNotOverwrittenFromRequestBody() throws IOException {
        Path aiohttpController = generateOutputDir("python-aiohttp", Collections.emptyMap())
                .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        assertFileContains(
                aiohttpController,
                "if isinstance(filter_param, dict):",
                "filter_param = QueryFilter.from_dict(filter_param)",
                "body = BodyPayload.from_dict(body)");
        assertFileNotContains(aiohttpController, "= .from_dict(");

        Path blueplanetController = generateOutputDir("python-blueplanet", Collections.emptyMap())
                .resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        assertFileContains(
                blueplanetController,
                "if isinstance(filter_param, dict):",
                "filter_param = QueryFilter.from_dict(filter_param)  # noqa: E501",
                "if isinstance(date_list, list):",
                "date_list = [util.deserialize_date(s) for s in date_list]  # noqa: E501",
                "if isinstance(model_map, dict):",
                "model_map = {k: QueryFilter.from_dict(v) for k, v in model_map.items()}  # noqa: E501",
                "body = BodyPayload.from_dict(connexion.request.get_json())  # noqa: E501");
        assertFileNotContains(blueplanetController, "QueryFilter.from_dict(connexion.request.get_json())");
        assertFileNotContains(blueplanetController, "deserialize_date(s) for s in connexion.request.get_json()");
        assertFileNotContains(blueplanetController, "connexion.request.get_json().items()");
        assertFileNotContains(blueplanetController, "= .from_dict(");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskController = generateOutputDir("python-flask", props)
                    .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
            assertFileContains(
                    flaskController,
                    "if isinstance(filter_param, dict):",
                    "filter_param = QueryFilter.from_dict(filter_param)  # noqa: E501",
                    "if isinstance(date_list, list):",
                    "date_list = [util.deserialize_date(s) for s in date_list]  # noqa: E501",
                    "if isinstance(model_map, dict):",
                    "model_map = {k: QueryFilter.from_dict(v) for k, v in model_map.items()}  # noqa: E501",
                    "body_payload = body",
                    "body_payload = BodyPayload.from_dict(flask.request.get_json())  # noqa: E501");
            assertFileNotContains(flaskController, "QueryFilter.from_dict(flask.request.get_json())");
            assertFileNotContains(flaskController, "deserialize_date(s) for s in flask.request.get_json()");
            assertFileNotContains(flaskController, "flask.request.get_json().items()");
        }
    }

}
