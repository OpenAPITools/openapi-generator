package org.openapitools.codegen.python;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractPythonConnexionServerCodegen;
import org.openapitools.codegen.languages.PythonAiohttpConnexionServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
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
        return generateOutputDir(generatorName, additionalProperties, "src/test/resources/3_0/python/connexion-param-sources.yaml");
    }

    private static Path generateOutputDir(String generatorName, Map<String, Object> additionalProperties,
                                          String specPath) throws IOException {
        File output = Files.createTempDirectory("test-" + generatorName).toFile().getCanonicalFile();
        output.deleteOnExit();

        CodegenConfigurator config = new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setInputSpec(specPath)
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
                "x_opt_date = request.headers.get('X-Opt-Date')",
                "if x_opt_date is not None:",
                "x_opt_date = util.deserialize_date(x_opt_date)",
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
                "x_status_list = request.headers.get('X-Status-List')",
                "x_status_list = x_status_list.split(',')",
                "x_status_list = [StatusEnum.from_dict(d) for d in x_status_list]",
                "x_obj_list = request.headers.get('X-Obj-List', [{'tag': 'alpha'}, {'tag': 'beta'}])",
                "x_obj_list = [QueryFilter.from_dict(d) for d in x_obj_list]",
                "x_nested_list = request.headers.get('X-Nested-List', [['a', 'b'], ['c']])",
                "session_token = request.cookies.get('session_token', 'default-cookie')",
                "opt_datetime = request.cookies.get('opt_datetime')",
                "if opt_datetime is not None:",
                "opt_datetime = util.deserialize_datetime(opt_datetime)",
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
        assertFileNotContains(aiohttpController, "['', '']");
        assertFileNotContains(aiohttpController, "x_obj_list.split(',')");
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
                "x_opt_date = connexion.request.headers.get('X-Opt-Date')",
                "if x_opt_date is not None:",
                "x_opt_date = util.deserialize_date(x_opt_date)",
                "x_json_header = connexion.request.headers.get('X-Json-Header')",
                "x_json_header = json.loads(x_json_header)",
                "x_json_header = HeaderArg.from_dict(x_json_header)  # noqa: E501",
                "x_tag_list = connexion.request.headers.get('X-Tag-List', ['foo', 'bar'])",
                "x_tag_list = x_tag_list.split(',')",
                "x_flag_list = connexion.request.headers.get('X-Flag-List', [True, False])",
                "x_flag_list = [x.lower() == 'true' if isinstance(x, str) else bool(x) for x in x_flag_list]",
                "x_status_list = connexion.request.headers.get('X-Status-List')",
                "x_status_list = x_status_list.split(',')",
                "x_status_list = [StatusEnum.from_dict(d) for d in x_status_list]  # noqa: E501",
                "x_obj_list = connexion.request.headers.get('X-Obj-List', [{'tag': 'alpha'}, {'tag': 'beta'}])",
                "x_obj_list = [QueryFilter.from_dict(d) for d in x_obj_list]  # noqa: E501",
                "x_nested_list = connexion.request.headers.get('X-Nested-List', [['a', 'b'], ['c']])",
                "session_token = connexion.request.cookies.get('session_token', 'default-cookie')",
                "opt_datetime = connexion.request.cookies.get('opt_datetime')",
                "if opt_datetime is not None:",
                "opt_datetime = util.deserialize_datetime(opt_datetime)",
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
        assertFileNotContains(blueplanetController, "['', '']");
        assertFileNotContains(blueplanetController, "x_obj_list.split(',')");
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
                    "x_opt_date = flask.request.headers.get('X-Opt-Date')",
                    "if x_opt_date is not None:",
                    "x_opt_date = util.deserialize_date(x_opt_date)",
                    "x_json_header = flask.request.headers.get('X-Json-Header')",
                    "x_json_header = json.loads(x_json_header)",
                    "x_json_header = HeaderArg.from_dict(x_json_header)  # noqa: E501",
                    "x_tag_list = flask.request.headers.get('X-Tag-List', ['foo', 'bar'])",
                    "x_tag_list = x_tag_list.split(',')",
                    "x_flag_list = flask.request.headers.get('X-Flag-List', [True, False])",
                    "x_flag_list = [x.lower() == 'true' if isinstance(x, str) else bool(x) for x in x_flag_list]",
                    "x_status_list = flask.request.headers.get('X-Status-List')",
                    "x_status_list = x_status_list.split(',')",
                    "x_status_list = [StatusEnum.from_dict(d) for d in x_status_list]  # noqa: E501",
                    "x_obj_list = flask.request.headers.get('X-Obj-List', [{'tag': 'alpha'}, {'tag': 'beta'}])",
                    "x_obj_list = [QueryFilter.from_dict(d) for d in x_obj_list]  # noqa: E501",
                    "x_nested_list = flask.request.headers.get('X-Nested-List', [['a', 'b'], ['c']])",
                    "session_token = flask.request.cookies.get('session_token', 'default-cookie')",
                    "opt_datetime = flask.request.cookies.get('opt_datetime')",
                    "if opt_datetime is not None:",
                    "opt_datetime = util.deserialize_datetime(opt_datetime)",
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
            assertFileNotContains(flaskController, "['', '']");
            assertFileNotContains(flaskController, "x_obj_list.split(',')");
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

    @Test(description = "python-aiohttp map parameters should iterate .items()")
    public void testAiohttpMapParametersIterateItems() throws IOException {
        String specPath = "src/test/resources/3_1/python/map-and-freeform-params.yaml";

        File aiohttpOutput = Files.createTempDirectory("test-aiohttp-map-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator aiohttpConfig = new CodegenConfigurator()
                    .setGeneratorName("python-aiohttp")
                    .setInputSpec(specPath)
                    .setOutputDir(aiohttpOutput.getAbsolutePath());
            new DefaultGenerator().opts(aiohttpConfig.toClientOptInput()).generate();

            Path aiohttpController = Paths.get(
                    aiohttpOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(aiohttpController);
            assertFileContains(
                    aiohttpController,
                    "    if date_map is not None:\n        date_map = {k: util.deserialize_date(v) for k, v in date_map.items()}",
                    "    if datetime_map is not None:\n        datetime_map = {k: util.deserialize_datetime(v) for k, v in datetime_map.items()}",
                    "    if model_map is not None:\n        model_map = {k: Tag.from_dict(v) for k, v in model_map.items()}");
            assertFileNotContains(
                    aiohttpController,
                    "for k, v in date_map}",
                    "for k, v in datetime_map}",
                    "for k, v in model_map}");
        } finally {
            FileUtils.deleteDirectory(aiohttpOutput);
        }
    }

    @Test(description = "Connexion servers should skip from_dict only on untyped/free-form object parameters and keep from_dict on composed/model parameters")
    public void testFreeFormAndAnyTypeParametersDoNotCallFromDict() throws IOException {
        String specPath = "src/test/resources/3_1/python/map-and-freeform-params.yaml";
        String composedSpecPath = "src/test/resources/3_1/python/composed-query-params.yaml";

        OpenAPI openAPI = new OpenAPIParser().readLocation(specPath, null, new ParseOptions()).getOpenAPI();
        PythonAiohttpConnexionServerCodegen codegen = new PythonAiohttpConnexionServerCodegen();
        codegen.setOpenAPI(openAPI);
        List<Parameter> testParameters = openAPI.getPaths().get("/test").getGet().getParameters();
        CodegenParameter freeFormParam = codegen.fromParameter(testParameters.get(3), new HashSet<>());
        CodegenParameter anyValParam = codegen.fromParameter(testParameters.get(4), new HashSet<>());

        assertThat(freeFormParam.paramName).isEqualTo("free_form");
        assertThat(freeFormParam.isFreeFormObject).isTrue();
        assertThat(freeFormParam.isAnyType).isFalse();
        assertThat(freeFormParam.isContainer).isFalse();
        assertThat(freeFormParam.isPrimitiveType).isFalse();
        assertThat(freeFormParam.dataType).isEqualTo("object");

        assertThat(anyValParam.paramName).isEqualTo("any_val");
        assertThat(anyValParam.isFreeFormObject).isFalse();
        assertThat(anyValParam.isAnyType).isTrue();
        assertThat(anyValParam.isContainer).isFalse();
        assertThat(anyValParam.isPrimitiveType).isFalse();
        assertThat(anyValParam.dataType).isEqualTo("object");

        OpenAPI composedOpenAPI = new OpenAPIParser().readLocation(composedSpecPath, null, new ParseOptions()).getOpenAPI();
        PythonAiohttpConnexionServerCodegen composedCodegen = new PythonAiohttpConnexionServerCodegen();
        composedCodegen.setOpenAPI(composedOpenAPI);
        List<Parameter> probeParameters = composedOpenAPI.getPaths().get("/t").getGet().getParameters();
        CodegenParameter composedParam = composedCodegen.fromParameter(probeParameters.get(0), new HashSet<>());
        CodegenParameter oneOfParam = composedCodegen.fromParameter(probeParameters.get(1), new HashSet<>());
        CodegenParameter addlParam = composedCodegen.fromParameter(probeParameters.get(2), new HashSet<>());
        assertThat(composedParam.isAnyType).isTrue();
        assertThat(composedParam.dataType).isEqualTo("Composed");
        assertThat(oneOfParam.isAnyType).isTrue();
        assertThat(oneOfParam.dataType).isEqualTo("OneOfM");
        assertThat(addlParam.paramName).isEqualTo("addl");
        assertThat(addlParam.isMap).isTrue();
        assertThat(addlParam.isContainer).isTrue();
        assertThat(addlParam.isFreeFormObject).isFalse();
        assertThat(addlParam.isAnyType).isFalse();
        assertThat(addlParam.dataType).isEqualTo("WithAddl");

        File aiohttpOutput = Files.createTempDirectory("test-aiohttp-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator aiohttpConfig = new CodegenConfigurator()
                    .setGeneratorName("python-aiohttp")
                    .setInputSpec(specPath)
                    .setOutputDir(aiohttpOutput.getAbsolutePath());
            new DefaultGenerator().opts(aiohttpConfig.toClientOptInput()).generate();

            Path aiohttpController = Paths.get(
                    aiohttpOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(aiohttpController);
            assertFileNotContains(
                    aiohttpController,
                    "free_form = ",
                    "any_val = ");
        } finally {
            FileUtils.deleteDirectory(aiohttpOutput);
        }

        File blueplanetOutput = Files.createTempDirectory("test-blueplanet-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator blueplanetConfig = new CodegenConfigurator()
                    .setGeneratorName("python-blueplanet")
                    .setInputSpec(specPath)
                    .setOutputDir(blueplanetOutput.getAbsolutePath());
            new DefaultGenerator().opts(blueplanetConfig.toClientOptInput()).generate();

            Path blueplanetController = Paths.get(
                    blueplanetOutput.getAbsolutePath(),
                    "app",
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(blueplanetController);
            assertFileNotContains(
                    blueplanetController,
                    "free_form = ",
                    "any_val = ");
        } finally {
            FileUtils.deleteDirectory(blueplanetOutput);
        }

        File flaskOutput = Files.createTempDirectory("test-flask-freeform-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator flaskConfig = new CodegenConfigurator()
                    .setGeneratorName("python-flask")
                    .setInputSpec(specPath)
                    .setOutputDir(flaskOutput.getAbsolutePath());
            new DefaultGenerator().opts(flaskConfig.toClientOptInput()).generate();

            Path flaskController = Paths.get(
                    flaskOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(flaskController);
            assertFileNotContains(
                    flaskController,
                    "free_form =",
                    "any_val =");
        } finally {
            FileUtils.deleteDirectory(flaskOutput);
        }

        File flaskComposedOutput = Files.createTempDirectory("test-flask-composed-params").toFile().getCanonicalFile();
        try {
            CodegenConfigurator flaskComposedConfig = new CodegenConfigurator()
                    .setGeneratorName("python-flask")
                    .setInputSpec(composedSpecPath)
                    .setOutputDir(flaskComposedOutput.getAbsolutePath());
            new DefaultGenerator().opts(flaskComposedConfig.toClientOptInput()).generate();

            Path flaskComposedController = Paths.get(
                    flaskComposedOutput.getAbsolutePath(),
                    "openapi_server",
                    "controllers",
                    "default_controller.py");
            assertFileExists(flaskComposedController);
            assertFileContains(
                    flaskComposedController,
                    ":type addl: Dict[str, str]",
                    // query parameters are converted from their own value, not overwritten from the request body
                    "composed = Composed.from_dict(composed)  # noqa: E501",
                    "oneof = OneOfM.from_dict(oneof)  # noqa: E501",
                    "plain = Pet.from_dict(plain)  # noqa: E501");
            assertFileNotContains(
                    flaskComposedController,
                    "addl =");
        } finally {
            FileUtils.deleteDirectory(flaskComposedOutput);
        }
    }

    @Test(description = "OpenAPI 3 form request bodies are received as a single dict argument "
            + "the way Connexion passes them (x-body-name or 'body'), with file fields staying "
            + "separate arguments")
    public void testFormBodyCollapsedIntoBodyArgument() throws IOException {
        final String specPath = "src/test/resources/3_0/python-flask/form-body.yaml";

        Path aiohttpController = generateOutputDir("python-aiohttp", Collections.emptyMap(), specPath)
                .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        assertFileExists(aiohttpController);
        assertFileContains(
                aiohttpController,
                "async def update_pet_with_form(request: web.Request, pet_id, body=None) -> web.Response:",
                "_form_body = body or {}",
                "name = _form_body.get('name')",
                "size = _form_body.get('size')",
                "tag = _form_body.get('tag')",
                "tag = util.deserialize_date(tag)",
                "async def upload_pet_image(request: web.Request, pet_id, file, body=None) -> web.Response:",
                "note = _form_body.get('note')",
                "async def create_thing(request: web.Request, verbose=None, form=None) -> web.Response:",
                "_form_body = form or {}",
                "payload = _form_body.get('payload')",
                "async def create_order(request: web.Request, body, coupon=None) -> web.Response:",
                "amount = _form_body.get('amount')",
                "async def replace_pet_form(request: web.Request, pet_id, body) -> web.Response:",
                "async def create_gadget(request: web.Request, body=None) -> web.Response:",
                "body = _form_body.get('body')",
                "async def upload_mixed_case(request: web.Request, body=None, file=None) -> web.Response:",
                "async def submit_pet_survey(request: web.Request, pet_id, body=None) -> web.Response:",
                "scores = _form_body.get('scores')",
                ":param body: Form fields: name (str): Updated name of the pet; "
                        + "size (int): Updated size of the pet; tag (str): Tag date",
                ":param body: Survey answers submitted by the owner. "
                        + "Form fields: scores (List[int]): Rating scores; "
                        + "labels (Dict[str, str]); comment (str); metadata (dict | bytes)",
                ":param form: Form fields: payload (str)",
                ":param file: file to upload",
                ":type body: dict",
                // a non-file argument colliding with the body name never reaches the
                // handler (Connexion overwrites it with the dict), so the signature and
                // docstring describe the dict that actually arrives
                "async def create_collision_thing(request: web.Request, body=None) -> web.Response:",
                "_form_body = body if isinstance(body, dict) else {}",
                // a same-named file field is passed after the dict and wins instead
                "async def upload_file_collision(request: web.Request, body=None) -> web.Response:",
                "note = _form_body.get('note')",
                // wire names are emitted as escaped Python string literals
                "async def create_quoted(request: web.Request, body=None) -> web.Response:",
                "its = _form_body.get('it\\'s')",
                "back_slash = _form_body.get('back\\\\slash')");
        assertFileNotContains(aiohttpController, "def update_pet_with_form(request: web.Request, pet_id, name");
        // the collapsed fields are no longer handler arguments, so they must not
        // keep their own :param entries; their names, types and descriptions move
        // into the dict argument's :param line
        assertFileNotContains(aiohttpController, ":param name:");
        assertFileNotContains(aiohttpController, ":param size:");
        assertFileNotContains(aiohttpController, ":param tag:");
        assertFileNotContains(aiohttpController, ":param scores:");
        assertFileNotContains(aiohttpController, ":param payload:");
        assertFileNotContains(aiohttpController, ":param amount:");
        assertFileNotContains(aiohttpController, ":type body: dict | bytes");
        assertFileNotContains(aiohttpController, "shadowed query parameter");
        assertFileNotContains(aiohttpController, "&#39;");
        // python-aiohttp always runs on Connexion 2.x, which prefers the requestBody-level
        // x-body-name its own codegen injects ('body'), not the schema-level one
        assertFileContains(
                aiohttpController,
                "async def create_widget(request: web.Request, body=None) -> web.Response:");
        assertFileNotContains(aiohttpController, "schbody");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskController = generateOutputDir("python-flask", props, specPath)
                    .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
            assertFileExists(flaskController);
            assertFileContains(
                    flaskController,
                    "def update_pet_with_form(pet_id, body=None):  # noqa: E501",
                    "_form_body = body or {}",
                    "name = _form_body.get('name')",
                    "size = _form_body.get('size')",
                    "tag = _form_body.get('tag')",
                    "tag = util.deserialize_date(tag)",
                    "def upload_pet_image(pet_id, file, body=None):  # noqa: E501",
                    "note = _form_body.get('note')",
                    "def create_thing(verbose=None, form=None):  # noqa: E501",
                    "_form_body = form or {}",
                    "payload = _form_body.get('payload')",
                    "def create_order(body, coupon=None):  # noqa: E501",
                    "amount = _form_body.get('amount')",
                    "def replace_pet_form(pet_id, body):  # noqa: E501",
                    "def create_gadget(body=None):  # noqa: E501",
                    "body = _form_body.get('body')",
                    "def upload_mixed_case(body=None, file=None):  # noqa: E501",
                    "def submit_pet_survey(pet_id, body=None):  # noqa: E501",
                    "scores = _form_body.get('scores')",
                    ":param body: Form fields: name (str): Updated name of the pet; "
                            + "size (int): Updated size of the pet; tag (str): Tag date",
                    ":param body: Survey answers submitted by the owner. "
                            + "Form fields: scores (List[int]): Rating scores; "
                            + "labels (Dict[str, str]); comment (str); metadata (dict | bytes)",
                    ":param form: Form fields: payload (str)",
                    ":param file: file to upload",
                    ":type body: dict",
                    "def create_collision_thing(body=None):  # noqa: E501",
                    "_form_body = body if isinstance(body, dict) else {}",
                    "def upload_file_collision(body=None):  # noqa: E501",
                    "note = _form_body.get('note')",
                    "def create_quoted(body=None):  # noqa: E501",
                    "its = _form_body.get('it\\'s')",
                    "back_slash = _form_body.get('back\\\\slash')");
            assertFileNotContains(flaskController, "def update_pet_with_form(pet_id, name");
            assertFileNotContains(flaskController, ":param name:");
            assertFileNotContains(flaskController, ":param size:");
            assertFileNotContains(flaskController, ":param tag:");
            assertFileNotContains(flaskController, ":param scores:");
            assertFileNotContains(flaskController, ":param payload:");
            assertFileNotContains(flaskController, ":param amount:");
            assertFileNotContains(flaskController, ":type body: dict | bytes");
            assertFileNotContains(flaskController, "shadowed query parameter");
            assertFileNotContains(flaskController, "&#39;");
        }

        Path flaskCx2Controller = generateOutputDir("python-flask", Collections.emptyMap(), specPath)
                .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        // Connexion 2.x still reads the deprecated schema-level x-body-name as a fallback
        assertFileContains(
                flaskCx2Controller,
                "def create_widget(schbody=None):  # noqa: E501",
                "_form_body = schbody or {}",
                ":param schbody:",
                ":type schbody: dict");
        Path flaskCx3Controller = generateOutputDir("python-flask", ImmutableMap.of("useConnexion3", "true"), specPath)
                .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        // Connexion 3.x only reads x-body-name on the requestBody, so the schema-level
        // extension is ignored and the argument falls back to 'body'
        assertFileContains(
                flaskCx3Controller,
                "def create_widget(body=None):  # noqa: E501",
                "note = _form_body.get('note')");
        assertFileNotContains(flaskCx3Controller, "schbody");
    }

    @Test(description = "webhook operations get their form body argument name and "
            + "required flag from the webhook PathItem's requestBody, the same way "
            + "path operations do (x-body-name or 'body')")
    public void testFormBodyCollapsedForWebhook() throws IOException {
        final String specPath = "src/test/resources/3_0/python-flask/form-webhook.yaml";

        Path aiohttpController = generateOutputDir("python-aiohttp", Collections.emptyMap(), specPath)
                .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        assertFileExists(aiohttpController);
        assertFileContains(
                aiohttpController,
                "async def on_new_thing(request: web.Request, payload) -> web.Response:",
                "_form_body = payload or {}",
                "name = _form_body.get('name')",
                ":param payload: Form fields: name (str)");
        assertFileNotContains(aiohttpController, ":param name:");

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            Path flaskController = generateOutputDir("python-flask", props, specPath)
                    .resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
            assertFileExists(flaskController);
            assertFileContains(
                    flaskController,
                    "def on_new_thing(payload):  # noqa: E501",
                    "_form_body = payload or {}",
                    "name = _form_body.get('name')",
                    ":param payload: Form fields: name (str)");
            assertFileNotContains(flaskController, ":param name:");
        }
    }

    @Test(description = "python-blueplanet targets Connexion 1.x, which passes form fields "
            + "individually, so its handlers keep one argument per form field")
    public void testBlueplanetKeepsIndividualFormFieldArguments() throws IOException {
        final String specPath = "src/test/resources/3_0/python-flask/form-body.yaml";

        Path blueplanetController = generateOutputDir("python-blueplanet", Collections.emptyMap(), specPath)
                .resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        assertFileExists(blueplanetController);
        assertFileContains(
                blueplanetController,
                "def update_pet_with_form(pet_id, name, size=None, tag=None):  # noqa: E501",
                "def upload_pet_image(pet_id, file, note=None):  # noqa: E501",
                "def create_thing(verbose=None, payload=None):  # noqa: E501",
                "def submit_pet_survey(pet_id, scores=None, labels=None, comment=None, metadata=None):  # noqa: E501",
                ":param name: Updated name of the pet",
                "def create_order(coupon=None, amount=None):  # noqa: E501",
                "def replace_pet_form(pet_id, amount=None):  # noqa: E501",
                "def create_widget(note=None):  # noqa: E501",
                "def create_gadget(body=None, name=None):  # noqa: E501",
                "def upload_mixed_case(name=None, file=None):  # noqa: E501");
        assertFileNotContains(blueplanetController, "_form_body");
        assertFileNotContains(blueplanetController, "def update_pet_with_form(pet_id, body");
    }

    @Test(description = "python-aiohttp generated requirements.txt and setup.py declare python_dateutil so util.deserialize_date/datetime actually parse dates")
    public void testAiohttpRequirementsIncludePythonDateutil() throws IOException {
        Path aiohttpDir = generateOutputDir("python-aiohttp", Collections.emptyMap());
        Path aiohttpRequirements = aiohttpDir.resolve("requirements.txt");
        Path aiohttpSetupPy = aiohttpDir.resolve("setup.py");
        assertFileExists(aiohttpRequirements);
        assertFileExists(aiohttpSetupPy);
        assertFileContains(aiohttpRequirements, "python_dateutil >= 2.6.0");
        assertFileContains(aiohttpSetupPy, "python_dateutil>=2.6.0");
    }
}
