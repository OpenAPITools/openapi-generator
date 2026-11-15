package org.openapitools.codegen.python;

import com.google.common.collect.ImmutableMap;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
        return generateOutputDir(generatorName, additionalProperties,
                "src/test/resources/3_0/python/connexion-param-sources.yaml");
    }

    private static Path generateOutputDir(String generatorName, String specPath,
                                          Map<String, Object> additionalProperties) throws IOException {
        return generateOutputDir(generatorName, additionalProperties, specPath);
    }

    private static Path generateOutputDir(String generatorName, Map<String, Object> additionalProperties,
                                          String inputSpec) throws IOException {
        File output = Files.createTempDirectory("test-" + generatorName).toFile().getCanonicalFile();
        output.deleteOnExit();

        CodegenConfigurator config = new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setInputSpec(inputSpec)
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
        assertFileNotContains(aiohttpController, "x_obj_list.split(',')");
        assertFileNotContains(aiohttpController, "['', '']");
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
        assertFileNotContains(blueplanetController, "x_obj_list.split(',')");
        assertFileNotContains(blueplanetController, "['', '']");
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
            assertFileNotContains(flaskController, "x_obj_list.split(',')");
            assertFileNotContains(flaskController, "['', '']");
            assertFileContains(flaskTest, "'X-Req-Header':");
            assertFileNotContains(flaskTest, "'x_req_header':");
        }
    }

    @Test(description = "property, parameter, and apiKey names must be emitted losslessly (not HTML-escaped) inside generated Python string literals")
    public void testEscapedNamesInAttributeMapAndApiKeyHeaders() throws IOException {
        String specPath = "src/test/resources/3_0/python-flask/escaped-names.yaml";

        Path flaskDir = generateOutputDir("python-flask", Collections.emptyMap(), specPath);
        Path flaskModel = flaskDir.resolve(Paths.get("openapi_server", "models", "escaped_names.py"));
        assertFileExists(flaskModel);
        assertFileContains(flaskModel,
                "'ab': 'a&b'",
                "'ok': 'o\\'k'",
                "'back_slash': 'back\\\\slash'",
                "'dqx': 'dq\"x'",
                "'unicode': 'unicodeé'");
        assertFileNotContains(flaskModel, "&amp;", "&#39;", "&quot;");
        Path flaskTest = flaskDir.resolve(Paths.get("openapi_server", "test", "test_default_controller.py"));
        assertFileExists(flaskTest);
        // an apiKey-in-header must send its keyParamName (X-A&B'Key), not the security
        // scheme name (apiKeyAuth); an apiKey-in-query must not be added to headers at
        // all; a query parameter must send its original name (q&we's), not the
        // sanitized Python parameter name (qwes)
        assertFileContains(flaskTest,
                "'X-A&B\\'Key': 'special-key'",
                "('q&we\\'s',",
                "('api_query_token', 'special-key')");
        assertFileNotContains(flaskTest,
                "'apiKeyAuth'", "'apiQueryAuth'", "('qwes',");
        // the 'X-A&amp;B&#39;Head' headerParam line emitted here stays HTML-escaped on
        // purpose: flask/aiohttp test and controller headerParam names are owned by the
        // object-params work (x-python-connexion-quoted-name), not by this change

        Path aiohttpDir = generateOutputDir("python-aiohttp", Collections.emptyMap(), specPath);
        Path aiohttpModel = aiohttpDir.resolve(Paths.get("openapi_server", "models", "escaped_names.py"));
        assertFileExists(aiohttpModel);
        assertFileContains(aiohttpModel,
                "'ab': 'a&b'",
                "'ok': 'o\\'k'",
                "'back_slash': 'back\\\\slash'",
                "'dqx': 'dq\"x'",
                "'unicode': 'unicodeé'");
        assertFileNotContains(aiohttpModel, "&amp;", "&#39;", "&quot;");
        Path aiohttpTest = aiohttpDir.resolve(Paths.get("tests", "test_default_controller.py"));
        assertFileExists(aiohttpTest);
        assertFileContains(aiohttpTest,
                "'X-A&B\\'Key': 'special-key'",
                "('q&we\\'s',",
                "('api_query_token', 'special-key')");
        assertFileNotContains(aiohttpTest,
                "'apiKeyAuth'", "'apiQueryAuth'", "('qwes',");

        Path blueplanetDir = generateOutputDir("python-blueplanet", Collections.emptyMap(), specPath);
        Path blueplanetModel = blueplanetDir.resolve(Paths.get("app", "openapi_server", "models", "escaped_names.py"));
        assertFileExists(blueplanetModel);
        assertFileContains(blueplanetModel,
                "'ab': 'a&b'",
                "'ok': 'o\\'k'",
                "'back_slash': 'back\\\\slash'",
                "'dqx': 'dq\"x'",
                "'unicode': 'unicodeé'");
        assertFileNotContains(blueplanetModel, "&amp;", "&#39;", "&quot;");
        Path blueplanetTest = blueplanetDir.resolve(Paths.get("app", "openapi_server", "test", "test_default_controller.py"));
        assertFileExists(blueplanetTest);
        // blueplanet's test header list must send the original header name, escaped for
        // a single-quoted Python literal
        assertFileContainsTrimmedLine(blueplanetTest, "headers = [('X-A&B\\'Head', 'x_ab_head_example')]");
        assertFileContainsTrimmedLine(blueplanetTest,
                "headers += [('Cookie', '; '.join(['X-C&oo\\'kie=' + str('x_cookie_example')]))]");
        assertFileNotContains(blueplanetTest, "('x_ab_head',", "&amp;", "&#39;");
        // the query parameter must be sent under its wire name too, not the sanitized
        // Python parameter name
        assertFileContainsTrimmedLine(blueplanetTest, "query_string = [('q&we\\'s', 'qwes_example')]");
        assertFileNotContains(blueplanetTest, "('qwes',");
        Path blueplanetController = blueplanetDir.resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        assertFileExists(blueplanetController);
        // the controller must read header and cookie parameters under their original
        // names, escaped for a single-quoted Python literal rather than HTML-escaped
        assertFileContainsTrimmedLine(blueplanetController,
                "x_ab_head = connexion.request.headers.get('X-A&B\\'Head')");
        assertFileContainsTrimmedLine(blueplanetController,
                "x_cookie = connexion.request.cookies.get('X-C&oo\\'kie')");
        assertFileNotContains(blueplanetController, "&amp;", "&#39;");
        // an unescaped quote would leave a broken literal like 'X-A&B'Head' in the
        // file; pin that no such unterminated form is emitted (replaces the former
        // `python3 -m py_compile` syntax check, which must not be required by the
        // unit tests — see #25556)
        assertFileNotContains(blueplanetTest, "'X-A&B'Head'", "'q&we's'", "'X-C&oo'kie'");
        assertFileNotContains(blueplanetController, "'X-A&B'Head'", "'X-C&oo'kie'");
    }

    @Test(description = "escapeForPythonSingleQuotedLiteral is lossless: it escapes backslash, single quote, and control characters so the literal evaluates back to the original name")
    public void testEscapeForPythonSingleQuotedLiteralIsLossless() {
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("a&b")).isEqualTo("a&b");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("o'k")).isEqualTo("o\\'k");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("back\\slash")).isEqualTo("back\\\\slash");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("dq\"x")).isEqualTo("dq\"x");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("unicode\u00e9")).isEqualTo("unicode\u00e9");
        // escapeTextInSingleQuotes folds \t, \n, \r to a space and rewrites "'''"; this conversion must not
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("a\tb\nc\rd")).isEqualTo("a\\tb\\nc\\rd");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("x'''y")).isEqualTo("x\\'\\'\\'y");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("a\u000bb")).isEqualTo("a\\x0bb");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral("a\u007fb")).isEqualTo("a\\x7fb");
        assertThat(AbstractPythonConnexionServerCodegen.escapeForPythonSingleQuotedLiteral(null)).isNull();
    }

    @Test(description = "pythonic_params generators get Connexion-style argument names (id_, filter_) while python-blueplanet keeps the parent naming")
    public void testPythonicParamsArgumentNamesPerGenerator() throws IOException {
        String specPath = "src/test/resources/3_0/python-flask/builtin-param-names.yaml";

        Path aiohttpDir = generateOutputDir("python-aiohttp", specPath, Collections.emptyMap());
        Path aiohttpController = aiohttpDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path aiohttpTest = aiohttpDir.resolve(Paths.get("tests", "test_default_controller.py"));
        assertFileExists(aiohttpController);
        assertFileContains(
                aiohttpController,
                "import json",
                "async def get_thing(request: web.Request, id_, filter_=None, type_=None, format_=None, from_=None, page_size=None) -> web.Response:",
                // id arrives via a $ref'd components/parameters entry; leading
                // underscores and 'callback' are passed through by Connexion;
                // 'date'/'self' are not builtins or keywords
                "async def get_probe(request: web.Request, id_, _from=None, callback=None, date=None, self=None) -> web.Response:",
                // a JSON header named 'json' must not shadow the json module
                // used by the template for JSON content headers
                "_json = request.headers.get('json')",
                "_json = json.loads(_json)",
                "async def post_thing_form(request: web.Request, filter_, type_=None) -> web.Response:");
        // generated tests send form data under the wire (original) names
        assertFileContains(
                aiohttpTest,
                "'filter': 'filter__example'",
                "data.add_field('filter', 'filter__example')");
        assertFileNotContains(aiohttpTest, "'filter_':");

        Path blueplanetDir = generateOutputDir("python-blueplanet", specPath, Collections.emptyMap());
        Path blueplanetController = blueplanetDir.resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        assertFileExists(blueplanetController);
        // blueplanet registers the API without pythonic_params (Connexion 1.x),
        // so handler argument names keep the parent rules
        assertFileContains(
                blueplanetController,
                "def get_thing(id, filter=None, type=None, format=None, _from=None, page_size=None):",
                "def get_probe(id, _from=None, param_callback=None, _date=None, _self=None):",
                "_json = connexion.request.headers.get('json')",
                "def post_thing_form(filter, type=None):");
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

    @Test(description = "a parameter named 'request' must not collide with the aiohttp request context passed via pass_context_arg_name")
    public void testAiohttpConnexionContextArgNameAvoidsParameterNamedRequest() throws IOException {
        // a query parameter named "request" renames the context argument to request_context
        Path aiohttpDir = generateOutputDir("python-aiohttp",
                "src/test/resources/3_0/python/request-param-name.yaml", Collections.emptyMap());
        Path aiohttpController = aiohttpDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path aiohttpInit = aiohttpDir.resolve(Paths.get("openapi_server", "__init__.py"));
        Path aiohttpConftest = aiohttpDir.resolve(Paths.get("tests", "conftest.py"));
        assertFileExists(aiohttpController);
        assertFileExists(aiohttpInit);
        assertFileExists(aiohttpConftest);
        assertFileContains(
                aiohttpController,
                "async def list_things(request_context: web.Request, request=None) -> web.Response:",
                "x_trace = request_context.headers.get('X-Trace')");
        assertFileNotContains(aiohttpController, "request: web.Request, request");
        assertFileContains(aiohttpInit, "pass_context_arg_name='request_context'");
        assertFileContains(aiohttpConftest, "pass_context_arg_name='request_context'");

        // the alternative name gets numbered when it collides with a parameter too
        Path collisionDir = generateOutputDir("python-aiohttp",
                "src/test/resources/3_0/python/request-context-param-name.yaml", Collections.emptyMap());
        Path collisionController = collisionDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path collisionInit = collisionDir.resolve(Paths.get("openapi_server", "__init__.py"));
        assertFileExists(collisionController);
        assertFileExists(collisionInit);
        assertFileContains(
                collisionController,
                "async def list_things(request_context2: web.Request, request=None, request_context=None) -> web.Response:",
                "x_trace = request_context2.headers.get('X-Trace')");
        assertFileNotContains(collisionController, "request_context: web.Request");
        assertFileContains(collisionInit, "pass_context_arg_name='request_context2'");

        // without a 'request' parameter the context argument keeps its default name
        Path defaultDir = generateOutputDir("python-aiohttp", Collections.emptyMap());
        Path defaultInit = defaultDir.resolve(Paths.get("openapi_server", "__init__.py"));
        Path defaultConftest = defaultDir.resolve(Paths.get("tests", "conftest.py"));
        Path defaultController = defaultDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        assertFileExists(defaultInit);
        assertFileExists(defaultConftest);
        assertFileExists(defaultController);
        assertFileContains(defaultInit, "pass_context_arg_name='request'");
        assertFileContains(defaultConftest, "pass_context_arg_name='request'");
        assertFileNotContains(defaultInit, "request_context");
        assertFileContains(
                defaultController,
                "async def create_item(request: web.Request, filter_param, body, date_list=None, model_map=None) -> web.Response:",
                "x_req_header = request.headers.get('X-Req-Header')");
    }

    @Test(description = "the aiohttp request context argument must not collide with names produced by duplicate-parameter renumbering or form fields inside composed schemas")
    public void testAiohttpConnexionContextArgNameAvoidsRenumberedAndComposedFormNames() throws IOException {
        // duplicate parameters are renumbered (request_context -> request_context2),
        // so the context argument is numbered past them
        Path duplicateDir = generateOutputDir("python-aiohttp",
                "src/test/resources/3_0/python/request-context-duplicate-param-name.yaml", Collections.emptyMap());
        Path duplicateController = duplicateDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path duplicateInit = duplicateDir.resolve(Paths.get("openapi_server", "__init__.py"));
        assertFileExists(duplicateController);
        assertFileExists(duplicateInit);
        assertFileContains(
                duplicateController,
                "async def list_things(request_context3: web.Request, request_context2, request=None, request_context=None) -> web.Response:");
        assertFileNotContains(duplicateController, "request_context2: web.Request");
        assertFileContains(duplicateInit, "pass_context_arg_name='request_context3'");

        // a form field declared inside an allOf schema becomes a handler argument too
        Path formDir = generateOutputDir("python-aiohttp",
                "src/test/resources/3_0/python/request-form-allof-param-name.yaml", Collections.emptyMap());
        Path formController = formDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
        Path formInit = formDir.resolve(Paths.get("openapi_server", "__init__.py"));
        assertFileExists(formController);
        assertFileExists(formInit);
        assertFileContains(
                formController,
                "async def create_thing(request_context: web.Request, request=None) -> web.Response:");
        assertFileNotContains(formController, "request: web.Request, request");
        assertFileContains(formInit, "pass_context_arg_name='request_context'");
    }

    @Test(description = "the aiohttp request context argument must not collide with a webhook parameter named 'request'")
    public void testAiohttpConnexionContextArgNameAvoidsWebhookParameterNamedRequest() throws IOException {
        // webhooks render through controller.mustache like regular operations, so a
        // webhook parameter named "request" renames the context argument too
        Path webhookDir = generateOutputDir("python-aiohttp",
                "src/test/resources/3_1/python/webhook-request-param-name.yaml", Collections.emptyMap());
        Path webhookController = webhookDir.resolve(Paths.get("openapi_server", "controllers", "hooks_controller.py"));
        Path webhookInit = webhookDir.resolve(Paths.get("openapi_server", "__init__.py"));
        assertFileExists(webhookController);
        assertFileExists(webhookInit);
        assertFileContains(
                webhookController,
                "async def on_new_thing(request_context: web.Request, request=None) -> web.Response:");
        assertFileNotContains(webhookController, "request: web.Request, request");
        assertFileContains(webhookInit, "pass_context_arg_name='request_context'");
    }

    @Test(description = "header parameters defined with content+$ref should use dataType when baseType is null in python-aiohttp")
    public void testJsonHeaderContentParameterUsesDataTypeInAiohttpController() throws IOException {
        File aiohttpOutput = Files.createTempDirectory("test-aiohttp").toFile().getCanonicalFile();
        aiohttpOutput.deleteOnExit();

        final CodegenConfigurator aiohttpConfig = new CodegenConfigurator()
                .setGeneratorName("python-aiohttp")
                .setInputSpec("src/test/resources/3_1/python/json-header-content.yaml")
                .setOutputDir(aiohttpOutput.getAbsolutePath());

        List<File> aiohttpFiles = new DefaultGenerator().opts(aiohttpConfig.toClientOptInput()).generate();
        aiohttpFiles.forEach(File::deleteOnExit);

        Path aiohttpController = Paths.get(
                aiohttpOutput.getAbsolutePath(),
                "openapi_server",
                "controllers",
                "default_controller.py");
        assertFileExists(aiohttpController);
        assertFileContains(
                aiohttpController,
                "from openapi_server.models.header_arg import HeaderArg",
                "x_json_arg = HeaderArg.from_dict(x_json_arg)");
        assertFileNotContains(aiohttpController, "= .from_dict(");
    }

    @Test(description = "header parameters defined with content+$ref should use dataType when baseType is null in python-blueplanet")
    public void testJsonHeaderContentParameterUsesDataTypeInBlueplanetController() throws IOException {
        File blueplanetOutput = Files.createTempDirectory("test-blueplanet").toFile().getCanonicalFile();
        blueplanetOutput.deleteOnExit();

        final CodegenConfigurator blueplanetConfig = new CodegenConfigurator()
                .setGeneratorName("python-blueplanet")
                .setInputSpec("src/test/resources/3_1/python/json-header-content.yaml")
                .setOutputDir(blueplanetOutput.getAbsolutePath());

        List<File> blueplanetFiles = new DefaultGenerator().opts(blueplanetConfig.toClientOptInput()).generate();
        blueplanetFiles.forEach(File::deleteOnExit);

        Path blueplanetController = Paths.get(
                blueplanetOutput.getAbsolutePath(),
                "app",
                "openapi_server",
                "controllers",
                "default_controller.py");
        assertFileExists(blueplanetController);
        assertFileContains(
                blueplanetController,
                "from app.openapi_server.models.header_arg import HeaderArg  # noqa: E501",
                // header parameters are read from the request headers, not from the body
                "x_json_arg = connexion.request.headers.get('X-Json-Arg')",
                "x_json_arg = HeaderArg.from_dict(x_json_arg)  # noqa: E501");
        assertFileNotContains(blueplanetController, "= .from_dict(");
    }
    @Test(description = "security scheme names with '-' or '.' produce valid, unique Python function names that match the x-*Func references")
    public void testSecuritySchemeNamesProduceValidPythonFunctions() throws IOException {
        String specPath = "src/test/resources/3_0/python-flask/security-scheme-names.yaml";
        String funcBase = "openapi_server.controllers.security_controller.";

        // scheme name -> extension name -> function name the extension must reference.
        // "api_key" and "api_key_2" are already valid identifiers listed after names that
        // sanitize to their suffix: they keep the unnumbered suffix (the same output as
        // before sanitization), so "api-key" is bumped to "api_key_3" and "api.key" to
        // "api_key_4". The user-provided x-apikeyInfoFunc on "alias-key" is preserved
        // instead of being overwritten with the generated "info_from_alias_key".
        Map<String, Map<String, String>> expectedFuncRefs = new LinkedHashMap<>();
        expectedFuncRefs.put("api-key", ImmutableMap.of("x-apikeyInfoFunc", "info_from_api_key_3"));
        expectedFuncRefs.put("api_key", ImmutableMap.of("x-apikeyInfoFunc", "info_from_api_key"));
        expectedFuncRefs.put("petstore.auth", ImmutableMap.of(
                "x-tokenInfoFunc", "info_from_petstore_auth",
                "x-scopeValidateFunc", "validate_scope_petstore_auth"));
        expectedFuncRefs.put("basic-auth", ImmutableMap.of("x-basicInfoFunc", "info_from_basic_auth"));
        expectedFuncRefs.put("api_key_2", ImmutableMap.of("x-apikeyInfoFunc", "info_from_api_key_2"));
        expectedFuncRefs.put("api.key", ImmutableMap.of("x-apikeyInfoFunc", "info_from_api_key_4"));
        expectedFuncRefs.put("1st-key", ImmutableMap.of("x-apikeyInfoFunc", "info_from_1st_key"));
        expectedFuncRefs.put("bearer-token", ImmutableMap.of("x-bearerInfoFunc", "info_from_bearer_token"));
        expectedFuncRefs.put("alias-key", ImmutableMap.of("x-apikeyInfoFunc", "info_from_api_key"));

        Map<String, Map<String, Object>> variants = new LinkedHashMap<>();
        variants.put("python-flask", Collections.emptyMap());
        variants.put("python-flask+useConnexion3", ImmutableMap.of("useConnexion3", "true"));
        variants.put("python-aiohttp", Collections.emptyMap());

        for (Map.Entry<String, Map<String, Object>> variant : variants.entrySet()) {
            String generatorName = variant.getKey().replace("+useConnexion3", "");
            Path outputDir = generateOutputDir(generatorName, variant.getValue(), specPath);
            String label = variant.getKey();

            Path securityController = outputDir.resolve(
                    Paths.get("openapi_server", "controllers", "security_controller.py"));
            assertFileExists(securityController);
            assertFileNotContains(
                    securityController,
                    "def info_from__1st_key(",
                    "def info_from_1st_key_2(");

            // every generated function name must be a valid Python identifier and unique
            String code = Files.readString(securityController);
            Matcher defMatcher =
                    Pattern.compile("def\\s+(\\S+)\\s*\\(").matcher(code);
            Set<String> functionNames = new HashSet<>();
            while (defMatcher.find()) {
                String functionName = defMatcher.group(1);
                assertThat(functionName)
                        .as("%s: %s must be a valid Python identifier", label, functionName)
                        .matches("[a-zA-Z_][a-zA-Z0-9_]*");
                functionNames.add(functionName);
            }
            assertThat(functionNames)
                    .as("%s: info_from_/validate_scope_ function names must be unique", label)
                    .containsExactlyInAnyOrder(
                            "info_from_api_key",
                            "info_from_api_key_2",
                            "info_from_api_key_3",
                            "info_from_api_key_4",
                            "info_from_petstore_auth",
                            "validate_scope_petstore_auth",
                            "info_from_basic_auth",
                            "info_from_1st_key",
                            "info_from_bearer_token",
                            "info_from_alias_key");

            // each scheme's x-*Func reference in the generated spec must point at the
            // function defined for it in security_controller.py
            Path openapiYaml = outputDir.resolve(
                    Paths.get("openapi_server", "openapi", "openapi.yaml"));
            assertFileExists(openapiYaml);
            OpenAPI generatedSpec = new OpenAPIParser()
                    .readLocation(openapiYaml.toString(), null, new ParseOptions())
                    .getOpenAPI();
            assertThat(generatedSpec)
                    .as("%s: generated openapi.yaml must parse as OpenAPI", label)
                    .isNotNull();
            Map<String, SecurityScheme> securitySchemes =
                    generatedSpec.getComponents().getSecuritySchemes();
            assertThat(securitySchemes)
                    .as("%s: generated spec must declare security schemes", label)
                    .isNotNull();
            assertThat(securitySchemes.keySet())
                    .as("%s: generated spec must declare every fixture scheme", label)
                    .containsExactlyInAnyOrderElementsOf(expectedFuncRefs.keySet());
            for (Map.Entry<String, Map<String, String>> schemeEntry : expectedFuncRefs.entrySet()) {
                Map<String, Object> extensions =
                        securitySchemes.get(schemeEntry.getKey()).getExtensions();
                for (Map.Entry<String, String> funcRef : schemeEntry.getValue().entrySet()) {
                    assertThat(extensions)
                            .as("%s: %s.%s must reference the generated function",
                                    label, schemeEntry.getKey(), funcRef.getKey())
                            .containsEntry(funcRef.getKey(), funcBase + funcRef.getValue());
                    assertThat(functionNames)
                            .as("%s: %s.%s target must be defined in security_controller.py",
                                    label, schemeEntry.getKey(), funcRef.getKey())
                            .contains(funcRef.getValue());
                }
            }
        }
    }

    /**
     * Generates one output directory per connexion server variant the
     * JSON-content parameter tests cover, keyed by a variant label. A
     * {@code +<option>} suffix in the label selects an additional property;
     * the generator name is the label without the suffix.
     */
    private static Map<String, Path> generateConnexionVariants(String specPath) throws IOException {
        Map<String, Map<String, Object>> variants = new LinkedHashMap<>();
        variants.put("python-flask", Collections.emptyMap());
        variants.put("python-flask+useConnexion3", ImmutableMap.of("useConnexion3", "true"));
        variants.put("python-aiohttp", Collections.emptyMap());
        variants.put("python-blueplanet", Collections.emptyMap());

        Map<String, Path> outputs = new LinkedHashMap<>();
        for (Map.Entry<String, Map<String, Object>> variant : variants.entrySet()) {
            String generatorName = variant.getKey().replace("+useConnexion3", "");
            outputs.put(variant.getKey(),
                    generateOutputDir(generatorName, variant.getValue(), specPath));
        }
        return outputs;
    }

    private static boolean isBlueplanetVariant(String variantKey) {
        return variantKey.startsWith("python-blueplanet");
    }

    /**
     * The generated test stub for the default controller in each variant's
     * output layout.
     */
    private static Path generatedTestPath(String variantKey, Path outputDir) {
        if (isBlueplanetVariant(variantKey)) {
            return outputDir.resolve(Paths.get("app", "openapi_server", "test", "test_default_controller.py"));
        }
        if (variantKey.startsWith("python-aiohttp")) {
            return outputDir.resolve(Paths.get("tests", "test_default_controller.py"));
        }
        return outputDir.resolve(Paths.get("openapi_server", "test", "test_default_controller.py"));
    }

    /**
     * The generated controller for the default controller in each variant's
     * output layout.
     */
    private static Path generatedControllerPath(String variantKey, Path outputDir) {
        if (isBlueplanetVariant(variantKey)) {
            return outputDir.resolve(Paths.get("app", "openapi_server", "controllers", "default_controller.py"));
        }
        return outputDir.resolve(Paths.get("openapi_server", "controllers", "default_controller.py"));
    }

    @Test(description = "header parameters declared with content (application/json) send a JSON string literal in generated tests instead of None")
    public void testJsonContentHeaderParamsUseJsonLiteralInGeneratedTests() throws IOException {
        String specPath = "src/test/resources/3_0/python/connexion-json-header-test-values.yaml";

        for (Map.Entry<String, Path> variant : generateConnexionVariants(specPath).entrySet()) {
            String variantKey = variant.getKey();
            boolean blueplanet = isBlueplanetVariant(variantKey);
            Path testFile = generatedTestPath(variantKey, variant.getValue());
            assertFileExists(testFile);

            // each variant carries the example in a different place of the media type;
            // whatever the source, the generated test must send a JSON string literal
            // (blueplanet renders headers as tuples, the others as dict entries)
            String mediaExample = blueplanet
                    ? "('X-Json-Media-Example', '{\"a\":1}')" : "'X-Json-Media-Example': '{\"a\":1}'";
            assertFileContains(testFile,
                    mediaExample,
                    blueplanet ? "('X-Json-Examples', '{\"b\":2}')" : "'X-Json-Examples': '{\"b\":2}'",
                    blueplanet ? "('X-Json-Ref-Example', '{\"e\":5}')" : "'X-Json-Ref-Example': '{\"e\":5}'",
                    blueplanet ? "('X-Json-Schema-Example', '{\"c\":3}')" : "'X-Json-Schema-Example': '{\"c\":3}'",
                    blueplanet ? "('X-Json-No-Example', '{}')" : "'X-Json-No-Example': '{}'");
            String noneEntry = blueplanet
                    ? "('X-Json-Media-Example', None)" : "'X-Json-Media-Example': None";
            assertFileNotContains(testFile,
                    noneEntry,
                    blueplanet ? "('X-Json-Examples', None)" : "'X-Json-Examples': None",
                    blueplanet ? "('X-Json-Schema-Example', None)" : "'X-Json-Schema-Example': None",
                    blueplanet ? "('X-Json-No-Example', None)" : "'X-Json-No-Example': None");
            // schema-based (non-content) headers keep the previous example behavior
            assertFileContains(testFile,
                    blueplanet ? "('X-Plain-Header', 'x_plain_header_example')"
                            : "'X-Plain-Header': 'x_plain_header_example'");
            // cookie parameters are sent too: aiohttp passes a cookies dict,
            // flask and blueplanet fold them into a Cookie header value
            if (variantKey.startsWith("python-aiohttp")) {
                assertFileContains(testFile,
                        "'X-Json-Inline-Cookie': '{}'",
                        "'X-Json-Model-Cookie': '{\"c\":3}'",
                        "'count': str(56)",
                        "cookies=cookies");
                assertFileContainsTrimmedLine(testFile, "'count': str(56) }");
            } else {
                assertFileContains(testFile,
                        "'X-Json-Inline-Cookie=' + '{}'",
                        "'X-Json-Model-Cookie=' + '{\"c\":3}'",
                        "'count=' + str(56)");
                // the complete closing line of the Cookie statement must
                // balance every bracket: blueplanet emitted `])]`, leaving the
                // ('Cookie', ...) tuple unclosed → SyntaxError in generated tests
                assertFileContainsTrimmedLine(testFile, blueplanet
                        ? "'count=' + str(56)]))]"
                        : "'count=' + str(56)])");
            }

            // the cookie-only operation has no header list to extend, so the
            // Cookie statement must still be complete on its own
            String cookieOnly = generatedTestMethodBody(testFile, "test_get_cookie_only");
            if (variantKey.startsWith("python-aiohttp")) {
                assertThat(cookieOnly).contains(
                        "'session_id': str('session_id_example')",
                        "'visit_count': str(7)",
                        "cookies=cookies");
            } else {
                assertThat(cookieOnly).contains(
                        "'session_id=' + str('session_id_example')",
                        "'visit_count=' + str(7)");
                if (blueplanet) {
                    assertThat(cookieOnly).contains("headers = []");
                }
            }
            assertFileContainsTrimmedLine(testFile,
                    blueplanet ? "'visit_count=' + str(7)]))]"
                            : variantKey.startsWith("python-aiohttp") ? "'visit_count': str(7) }"
                            : "'visit_count=' + str(7)])");
        }
    }

    @Test(description = "statements in generated tests keep their 8-column indent whether or not the operation declares cookie params (an empty {{#cookieParams}} section must not leak the open line's indent into the next statement)")
    public void testGeneratedTestsStatementIndentWithAndWithoutCookieParams() throws IOException {
        // /ping declares no parameters at all: the empty cookieParams section
        // must render nothing, so `response = ...` stays at the same indent as
        // the statements around it — a mid-line {{#cookieParams}} tag used to
        // leave its 8 leading spaces behind, which merged into the next
        // statement and produced an unexpected-indent IndentationError
        for (Map.Entry<String, Path> variant : generateConnexionVariants(
                "src/test/resources/3_0/python/info-title-special-chars.yaml").entrySet()) {
            if (variant.getKey().startsWith("python-aiohttp")) {
                // aiohttp's cookie block already opens the section on its own
                // line and emits a `cookies = {...}` dict instead of a header
                continue;
            }
            Path testFile = generatedTestPath(variant.getKey(), variant.getValue());
            assertFileExists(testFile);
            String ping = generatedTestMethodBody(testFile, "test_ping");
            assertStatementIndent(ping, "response = self.client.open(", 8);
            assertStatementIndent(ping, "self.assert200(response,", 8);
        }

        // get_cookie_only declares only cookie params: the Cookie statement
        // must render at statement level too — flask mutates the headers dict,
        // blueplanet appends a ('Cookie', ...) tuple (creating the list first
        // when the operation has no header params to initialize it)
        for (Map.Entry<String, Path> variant : generateConnexionVariants(
                "src/test/resources/3_0/python/connexion-json-header-test-values.yaml").entrySet()) {
            String variantKey = variant.getKey();
            if (variantKey.startsWith("python-aiohttp")) {
                continue;
            }
            Path testFile = generatedTestPath(variantKey, variant.getValue());
            assertFileExists(testFile);
            String cookieOnly = generatedTestMethodBody(testFile, "test_get_cookie_only");
            if (isBlueplanetVariant(variantKey)) {
                assertStatementIndent(cookieOnly, "headers = []", 8);
                assertStatementIndent(cookieOnly, "headers += [('Cookie',", 8);
            } else {
                assertStatementIndent(cookieOnly, "headers['Cookie'] = '; '.join([", 8);
            }
            assertStatementIndent(cookieOnly, "response = self.client.open(", 8);
            assertStatementIndent(cookieOnly, "self.assert200(response,", 8);
        }
    }

    @Test(description = "content (application/json) header/cookie params get X.from_dict() only for generated model classes; inline object schemas pass the json.loads result through")
    public void testJsonContentHeaderParamsConvertOnlyGeneratedModels() throws IOException {
        String specPath = "src/test/resources/3_0/python/connexion-json-header-test-values.yaml";

        for (Map.Entry<String, Path> variant : generateConnexionVariants(specPath).entrySet()) {
            Path controller = generatedControllerPath(variant.getKey(), variant.getValue());
            assertFileExists(controller);

            // JSON content params are still parsed from the raw string first
            assertFileContains(controller,
                    "x_json_media_example = json.loads(x_json_media_example)",
                    "x_json_inline_cookie = json.loads(x_json_inline_cookie)");
            // inline `type: object` schemas produce no model class, so the parsed
            // dict is passed through instead of calling object.from_dict
            assertFileNotContains(controller,
                    "object.from_dict",
                    "from_dict(x_json_media_example)",
                    "from_dict(x_json_examples)",
                    "from_dict(x_json_ref_example)",
                    "from_dict(x_json_no_example)",
                    "from_dict(x_json_inline_cookie)");
            // $ref model headers/cookies keep the generated model conversion
            assertFileContains(controller,
                    "x_json_schema_example = HeaderWithExample.from_dict(x_json_schema_example)",
                    "x_json_model_cookie = HeaderWithExample.from_dict(x_json_model_cookie)");
        }
    }

    /**
     * Body of one generated test method: the text from {@code def <name>} up
     * to the next method definition, the {@code __main__} guard, or the end
     * of the file — so assertions can be scoped to a single operation rather
     * than the whole generated file.
     */
    private static String generatedTestMethodBody(Path file, String methodName) throws IOException {
        String content = Files.readString(file);
        int start = content.indexOf("def " + methodName);
        assertThat(start).as("generated file %s is missing %s", file, methodName)
                .isGreaterThanOrEqualTo(0);
        int end = content.length();
        for (String marker : new String[]{"\n    def ", "\nasync def ", "\n\nif __name__"}) {
            int next = content.indexOf(marker, start + 1);
            if (next >= 0) {
                end = Math.min(end, next);
            }
        }
        return content.substring(start, end);
    }

    @Test(description = "generated controller tests send apiKey security under the scheme's keyParamName in the declared in-location (header, query, cookie)")
    public void testApiKeyLocationsInGeneratedTests() throws IOException {
        String specPath = "src/test/resources/3_0/python/connexion-apikey-locations.yaml";

        for (Map<String, Object> props : new Map[]{Collections.emptyMap(), ImmutableMap.of("useConnexion3", "true")}) {
            boolean connexion3 = props.containsKey("useConnexion3");
            Path flaskDir = generateOutputDir("python-flask", props, specPath);
            Path flaskTest = flaskDir.resolve(Paths.get("openapi_server", "test", "test_default_controller.py"));
            assertFileExists(flaskTest);
            // header apiKey goes to headers under its wire name
            assertFileContains(flaskTest, "'X-Api-Key': 'special-key'");
            // query apiKey goes to query_string under its wire name
            assertFileContains(flaskTest, "('apikey', 'special-key')", "query_string=query_string");
            // cookie apiKey goes to a Cookie header under its wire name
            assertFileContains(flaskTest, "'Cookie': 'session=special-key'");
            // the scheme names are not wire names and must not be sent
            assertFileNotContains(flaskTest, "'apikey_header'", "'apikey_query'", "'apikey_cookie'");

            // the operation secured by all three schemes must carry every
            // credential inside the same generated test method, and the
            // cookie-capable client must be the one the variant needs:
            // connexion2 disables the werkzeug client's cookie handling so
            // the manual Cookie header survives; connexion3's client passes it
            String getSecuredAll = generatedTestMethodBody(flaskTest, "test_get_secured_all");
            assertThat(getSecuredAll)
                    .contains("'X-Api-Key': 'special-key'")
                    .contains("('apikey', 'special-key')")
                    .contains("'Cookie': 'session=special-key'")
                    .contains(connexion3 ? "client = self.client"
                            : "client = self.app.test_client(use_cookies=False)");
        }

        Path aiohttpDir = generateOutputDir("python-aiohttp", Collections.emptyMap(), specPath);
        Path aiohttpTest = aiohttpDir.resolve(Paths.get("tests", "test_default_controller.py"));
        assertFileExists(aiohttpTest);
        assertFileContains(aiohttpTest, "'X-Api-Key': 'special-key'");
        assertFileContains(aiohttpTest, "('apikey', 'special-key')", "params=params");
        assertFileContains(aiohttpTest, "'Cookie': 'session=special-key'");
        assertFileNotContains(aiohttpTest, "'apikey_header'", "'apikey_query'", "'apikey_cookie'");
        String aiohttpGetSecuredAll = generatedTestMethodBody(aiohttpTest, "test_get_secured_all");
        assertThat(aiohttpGetSecuredAll)
                .contains("'X-Api-Key': 'special-key'")
                .contains("('apikey', 'special-key')")
                .contains("'Cookie': 'session=special-key'");
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

    @Test(description = "info.title is embedded in generated Python source as a Python string literal, not HTML-escaped")
    public void testInfoTitleEmbeddedAsPythonStringLiteral() throws IOException {
        // spec title: Tom & Jerry <Service> 'quoted' "dquoted" \path
        String specPath = "src/test/resources/3_0/python/info-title-special-chars.yaml";
        String singleQuotedTitle = "'Tom & Jerry <Service> \\'quoted\\' \"dquoted\" \\\\path'";
        String doubleQuotedTitle = "\"Tom & Jerry <Service> 'quoted' \\\"dquoted\\\" \\\\path\"";
        String[] htmlEscaped = {"&amp;", "&lt;", "&gt;", "&#39;", "&quot;"};

        Path flaskDir = generateOutputDir("python-flask", specPath, Collections.emptyMap());
        Path flaskMain = flaskDir.resolve(Paths.get("openapi_server", "__main__.py"));
        Path flaskSetup = flaskDir.resolve("setup.py");
        assertFileExists(flaskMain);
        assertFileExists(flaskSetup);
        assertFileContains(flaskMain, "arguments={'title': " + singleQuotedTitle + "},");
        assertFileNotContains(flaskMain, htmlEscaped);
        // the setup() kwargs must be complete Python literals evaluating to the
        // raw spec values — pinned as whole lines so a truncated literal cannot
        // pass; evaluating the setup() kwarg values is out of scope for this
        // unit test
        assertFileContainsTrimmedLine(flaskSetup, "description=" + doubleQuotedTitle + ",");
        assertFileContainsTrimmedLine(flaskSetup, "keywords=[\"OpenAPI\", " + doubleQuotedTitle + "],");
        assertFileContainsTrimmedLine(flaskSetup, "author_email=\"\\\"weird\\\" <api@example.com>\",");
        assertFileContainsTrimmedLine(flaskSetup, "long_description=\"\"\"\\");
        assertFileContainsTrimmedLine(flaskSetup, "\"\"\"");
        assertFileContains(flaskSetup,
                "He said \\\"\\\"\\\"triple quotes\\\"\\\"\\\" end.",
                "Windows path C:\\\\tmp\\\\dir",
                "null\\x00byte\\x01end");
        assertFileNotContains(flaskSetup, htmlEscaped);

        Path flask3Dir = generateOutputDir("python-flask", specPath, ImmutableMap.of("useConnexion3", "true"));
        Path flask3Main = flask3Dir.resolve(Paths.get("openapi_server", "__main__.py"));
        Path flask3Setup = flask3Dir.resolve("setup.py");
        assertFileExists(flask3Main);
        assertFileExists(flask3Setup);
        assertFileContains(flask3Main, "arguments={'title': " + singleQuotedTitle + "},");
        assertFileNotContains(flask3Main, htmlEscaped);
        assertFileContainsTrimmedLine(flask3Setup, "description=" + doubleQuotedTitle + ",");
        assertFileContainsTrimmedLine(flask3Setup, "keywords=[\"OpenAPI\", " + doubleQuotedTitle + "],");
        assertFileContainsTrimmedLine(flask3Setup, "author_email=\"\\\"weird\\\" <api@example.com>\",");
        assertFileContainsTrimmedLine(flask3Setup, "long_description=\"\"\"\\");
        assertFileContainsTrimmedLine(flask3Setup, "\"\"\"");
        assertFileContains(flask3Setup,
                "He said \\\"\\\"\\\"triple quotes\\\"\\\"\\\" end.",
                "Windows path C:\\\\tmp\\\\dir",
                "null\\x00byte\\x01end");
        assertFileNotContains(flask3Setup, htmlEscaped);

        Path aiohttpDir = generateOutputDir("python-aiohttp", specPath, Collections.emptyMap());
        Path aiohttpMain = aiohttpDir.resolve(Paths.get("openapi_server", "__init__.py"));
        Path aiohttpSetup = aiohttpDir.resolve("setup.py");
        assertFileExists(aiohttpMain);
        assertFileExists(aiohttpSetup);
        assertFileContains(aiohttpMain, "arguments={'title': " + singleQuotedTitle + "},");
        assertFileNotContains(aiohttpMain, htmlEscaped);
        assertFileContainsTrimmedLine(aiohttpSetup, "description=" + doubleQuotedTitle + ",");
        assertFileContainsTrimmedLine(aiohttpSetup, "keywords=[\"OpenAPI\", " + doubleQuotedTitle + "],");
        assertFileContainsTrimmedLine(aiohttpSetup, "author_email=\"\\\"weird\\\" <api@example.com>\",");
        assertFileContainsTrimmedLine(aiohttpSetup, "long_description=\"\"\"\\");
        assertFileContainsTrimmedLine(aiohttpSetup, "\"\"\"");
        assertFileContains(aiohttpSetup,
                "He said \\\"\\\"\\\"triple quotes\\\"\\\"\\\" end.",
                "Windows path C:\\\\tmp\\\\dir",
                "null\\x00byte\\x01end");
        assertFileNotContains(aiohttpSetup, htmlEscaped);

        Path blueplanetDir = generateOutputDir("python-blueplanet", specPath, Collections.emptyMap());
        Path blueplanetMain = blueplanetDir.resolve(Paths.get("app", "openapi_server", "__main__.py"));
        Path blueplanetSetup = blueplanetDir.resolve(Paths.get("app", "setup.py"));
        assertFileExists(blueplanetMain);
        assertFileExists(blueplanetSetup);
        assertFileContains(blueplanetMain, "arguments={'title': " + singleQuotedTitle + "})");
        assertFileNotContains(blueplanetMain, htmlEscaped);
        assertFileContainsTrimmedLine(blueplanetSetup, "description=" + doubleQuotedTitle + ",");
        assertFileContainsTrimmedLine(blueplanetSetup, "keywords=[\"Swagger\", " + doubleQuotedTitle + "],");
        assertFileContainsTrimmedLine(blueplanetSetup, "author_email=\"\\\"weird\\\" <api@example.com>\",");
        assertFileContainsTrimmedLine(blueplanetSetup, "long_description=\"\"\"\\");
        assertFileContainsTrimmedLine(blueplanetSetup, "\"\"\"");
        assertFileContains(blueplanetSetup,
                "He said \\\"\\\"\\\"triple quotes\\\"\\\"\\\" end.",
                "Windows path C:\\\\tmp\\\\dir",
                "null\\x00byte\\x01end");
        assertFileNotContains(blueplanetSetup, htmlEscaped);
    }

    @Test(description = "a caller-provided infoEmail is rendered as the setup.py author_email literal when the spec declares no contact email")
    public void testProvidedInfoEmailUsedWhenSpecHasNoContactEmail() throws IOException {
        // connexion-param-sources.yaml declares no info.contact
        for (Map.Entry<String, String> variant : Map.of(
                "python-flask", "setup.py",
                "python-aiohttp", "setup.py",
                "python-blueplanet", "app/setup.py").entrySet()) {
            Path outputDir = generateOutputDir(variant.getKey(),
                    ImmutableMap.of("infoEmail", "caller@example.com"));
            Path setupPy = outputDir.resolve(variant.getValue());
            assertFileExists(setupPy);
            assertFileContains(setupPy, "author_email=\"caller@example.com\"");
        }
    }

    @Test(description = "python-blueplanet generated swagger.yaml is not empty and contains the OpenAPI spec")
    public void testBlueplanetSwaggerYamlNotEmpty() throws IOException {
        Path blueplanetDir = generateOutputDir("python-blueplanet", Collections.emptyMap());
        Path swaggerYaml = blueplanetDir.resolve(Paths.get("app", "openapi_server", "swagger", "swagger.yaml"));
        assertFileExists(swaggerYaml);
        assertFileContains(swaggerYaml, "openapi:", "paths:");
    }

    @Test(description = "python-blueplanet pins a connexion 2.x range that can serve the generated OpenAPI 3.0 swagger.yaml")
    public void testBlueplanetRequirementsPinConnexion2() throws IOException {
        Path blueplanetDir = generateOutputDir("python-blueplanet", Collections.emptyMap());
        Path requirements = blueplanetDir.resolve(Paths.get("app", "requirements.txt"));
        Path setupPy = blueplanetDir.resolve(Paths.get("app", "setup.py"));
        assertFileExists(requirements);
        assertFileExists(setupPy);
        // swagger.yaml is an OpenAPI 3.0 document, which connexion 1.x cannot
        // serve; the pin must select a 2.x release and exclude the connexion 3
        // line the templates do not support
        assertFileContains(requirements, "connexion[swagger-ui] >= 2.6.0",
                "connexion[swagger-ui] <= 2.14.2");
        // setup.py's REQUIRES must mirror requirements.txt — the same
        // connexion markers/extra and the swagger-ui-bundle — or
        // `python setup.py install` resolves a different, unsupported
        // runtime than the requirements-based install
        assertFileContains(setupPy,
                "connexion[swagger-ui] >= 2.6.0; python_version>=\"3.6\"",
                "connexion[swagger-ui] <= 2.3.0; python_version==\"3.5\" or python_version==\"3.4\"",
                "connexion[swagger-ui] <= 2.14.2; python_version>\"3.4\"",
                "werkzeug == 0.16.1; python_version==\"3.5\" or python_version==\"3.4\"",
                "swagger-ui-bundle >= 0.0.2");
    }

    /**
     * Asserts every line in {@code text} containing {@code marker} starts
     * with exactly {@code expectedIndent} spaces — a substring check cannot
     * pin indentation, since a line indented too deeply still contains the
     * marker.
     */
    private static void assertStatementIndent(String text, String marker, int expectedIndent) {
        List<String> matching = text.lines()
                .filter(line -> line.contains(marker))
                .collect(Collectors.toList());
        assertThat(matching).as("no generated line contains: %s", marker).isNotEmpty();
        for (String line : matching) {
            int indent = 0;
            while (indent < line.length() && line.charAt(indent) == ' ') {
                indent++;
            }
            assertThat(indent)
                    .as("line must start at column %d: %s", expectedIndent, line.trim())
                    .isEqualTo(expectedIndent);
        }
    }

    /**
     * Asserts the file contains a physical line equal to {@code expected}
     * after trimming — a stronger check than substring containment, used to
     * pin the exact closing brackets of the generated Cookie statement.
     */
    private static void assertFileContainsTrimmedLine(Path file, String expected) throws IOException {
        List<String> trimmed = Files.readAllLines(file).stream()
                .map(String::trim)
                .collect(Collectors.toList());
        assertThat(trimmed)
                .as("generated file %s must contain the line: %s", file, expected)
                .contains(expected);
    }
}
