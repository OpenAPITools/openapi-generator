package org.openapitools.codegen.python;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.media.UUIDSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PythonAiohttpConnexionServerCodegen;
import org.openapitools.codegen.languages.PythonFlaskConnexionServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

public class PythonFlaskConnexionServerCodegenTest {

    // Helper function, intended to reduce boilerplate
    static private String generateFiles(DefaultCodegen codegen, String filePath) throws IOException {
        final File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        final String outputPath = output.getAbsolutePath().replace('\\', '/');

        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        final ClientOptInput input = new ClientOptInput();
        final OpenAPI openAPI = new OpenAPIParser().readLocation(filePath, null, new ParseOptions()).getOpenAPI();
        input.openAPI(openAPI);
        input.config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(input).generate();

        Assert.assertTrue(files.size() > 0);
        return outputPath + "/";
    }


    @Test(description = "UUID model properties require the standard-library UUID import (issue #23897)")
    public void testUuidModelImport() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_23897.yaml");

        final Path model = Paths.get(outputPath, "openapi_server/models/pushnotification.py");
        assertFileExists(model);
        assertFileContains(model, "from uuid import UUID", "'id': UUID", "def id(self) -> UUID:");

        final String[][] uuidModels = {
                {"uuid_list.py", "List[UUID]"},
                {"uuid_map.py", "Dict[str, UUID]"},
                {"nested_uuid_containers.py", "List[Dict[str, List[UUID]]]"},
                {"all_of_uuid.py", "UUID"}
        };
        for (String[] uuidModel : uuidModels) {
            final Path generatedModel = Paths.get(outputPath, "openapi_server/models", uuidModel[0]);
            assertFileExists(generatedModel);
            assertFileContains(generatedModel, "from uuid import UUID", "'ids': " + uuidModel[1],
                    "def ids(self) -> " + uuidModel[1] + ":");
        }

        final Path stringList = Paths.get(outputPath, "openapi_server/models/string_list.py");
        assertFileContains(stringList, "'ids': List[str]");
        assertFileNotContains(stringList, "from uuid import UUID");
    }

    @DataProvider
    public Object[][] uuidSchemas() {
        return new Object[][] {
                {"direct UUID", new UUIDSchema(), true},
                {"array items", new ArraySchema().items(new UUIDSchema()), true},
                {"map values", new MapSchema().additionalProperties(new UUIDSchema()), true},
                {"UUID beside another object", new ObjectSchema()
                        .addProperties("id", new UUIDSchema())
                        .addProperties("tail", new ObjectSchema().addProperties("name", new StringSchema())), true},
                {"allOf branch", new ComposedSchema().addAllOfItem(new StringSchema())
                        .addAllOfItem(new UUIDSchema()), true},
                {"oneOf branch", new ComposedSchema().addOneOfItem(new StringSchema())
                        .addOneOfItem(new UUIDSchema()), true},
                {"anyOf branch", new ComposedSchema().addAnyOfItem(new StringSchema())
                        .addAnyOfItem(new UUIDSchema()), true},
                {"not branch", new Schema().not(new UUIDSchema()), true},
                {"mixed nesting", new ArraySchema().items(new ObjectSchema().addProperties("value",
                        new ComposedSchema().addAnyOfItem(new StringSchema())
                                .addAnyOfItem(new MapSchema().additionalProperties(new UUIDSchema())))), true},
                {"no UUID", new ArraySchema().items(new ObjectSchema().addProperties("value",
                        new ComposedSchema().addAnyOfItem(new StringSchema())
                                .addAnyOfItem(new MapSchema().additionalProperties(new StringSchema())))), false}
        };
    }

    @Test(dataProvider = "uuidSchemas", description = "Find UUIDs in every schema branch regardless of feature support")
    public void testUuidImportInNestedSchemas(String description, Schema schema, boolean expectedImport) {
        final PythonFlaskConnexionServerCodegen codegen = new PythonFlaskConnexionServerCodegen();
        codegen.setOpenAPI(new OpenAPI());
        final CodegenProperty property = codegen.fromProperty("value", schema, false);
        final CodegenModel model = new CodegenModel();

        codegen.postProcessModelProperty(model, property);

        Assert.assertEquals(model.imports.contains("from uuid import UUID"), expectedImport, description);
    }

    @Test(description = "test requestBody")
    public void testRequestBody() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_1666.yaml");

        final Path p1 = Paths.get(outputPath + "openapi_server/controllers/test1_controller.py");
        assertFileExists(p1);
        assertFileContains(p1, "def not_required(body=None):");
        assertFileContains(p1, "test_request = body");

        final Path p2 = Paths.get(outputPath + "openapi_server/controllers/test2_controller.py");
        assertFileContains(p2, "def required(body):");
        assertFileContains(p2, "test_request = body");

        final Path p3 = Paths.get(outputPath + "openapi_server/controllers/test3_controller.py");
        assertFileContains(p3, "def with_path_param(param1, body=None):");
        assertFileContains(p3, "test_request = body");

        final Path p4 = Paths.get(outputPath + "openapi_server/controllers/test4_controller.py");
        assertFileContains(p4, "def with_path_param_required(param1, body):");
        assertFileContains(p4, "test_request = body");
    }

    @Test(description = "the defaultController option controls the module name for untagged operations (issue #1891)")
    public void testDefaultController() throws IOException {
        final PythonFlaskConnexionServerCodegen codegen = new PythonFlaskConnexionServerCodegen();
        codegen.additionalProperties().put("defaultController", "my_default_controller");
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_1891.yaml");

        // The untagged operation should land in the configured controller module...
        final Path expected = Paths.get(outputPath + "openapi_server/controllers/my_default_controller.py");
        assertFileExists(expected);
        assertFileContains(expected, "def ping():");

        // ...and connexion's routing must point at the same module, not the hardcoded default.
        final Path openapiYaml = Paths.get(outputPath + "openapi_server/openapi/openapi.yaml");
        assertFileContains(openapiYaml, "x-openapi-router-controller: openapi_server.controllers.my_default_controller");

        // The hardcoded default_controller.py must no longer be emitted.
        final Path old = Paths.get(outputPath + "openapi_server/controllers/default_controller.py");
        Assert.assertFalse(Files.exists(old),
                "Untagged operations should honor defaultController, not fall back to default_controller.py");
    }

    @Test(description = "model examples in generated controller tests use the imported class name, not the package")
    public void testControllerTestModelExampleUsesImportedName() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/model-typed-params.yaml");

        // The test module imports Filter/Trace by class name, so "openapi_server.Filter()" raised NameError.
        final Path controllerTest = Paths.get(outputPath, "openapi_server/test/test_default_controller.py");
        assertFileExists(controllerTest);
        assertFileContains(controllerTest,
                "from openapi_server.models.filter import Filter",
                "from openapi_server.models.trace import Trace",
                // a model-typed query parameter is sent as form/explode wire pairs, not a Filter() repr
                "query_string = [('name', 'name_example'),",
                "('filters', [Filter()]),",
                "('filterMap', {'key': Filter()}),",
                "filter_ = Filter()",
                // a model-typed header is sent as its simple-style wire value, not a Trace() repr
                "'X-Trace': 'id,id_example',");
        // User-supplied examples that merely look like a constructor are kept verbatim.
        assertFileContains(controllerTest, "('label', 'openapi_server.Filter()')", "('count', openapi_server.int())");
        // A model-typed parameter with a default keeps the default as its example.
        assertFileContains(controllerTest, "('status', 'open')]");
        assertFileNotContains(controllerTest, "Status()");
        assertFileNotContains(controllerTest, "('filter', openapi_server.Filter())", "[openapi_server.Filter()]", "openapi_server.Trace()");
    }

    @Test
    public void testWebhookControllerDoesNotImportPydantic() throws IOException {
        final PythonFlaskConnexionServerCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_1/python/webhooks.yaml");

        final Path controller = Paths.get(outputPath + "openapi_server/controllers/hooks_controller.py");
        assertFileExists(controller);
        assertFileContains(
                controller,
                "from openapi_server.models.header_arg import HeaderArg  # noqa: E501",
                "from openapi_server import util",
                // header parameters are read from the request, not passed in by Connexion
                "def item_event_post(filter_):",
                "x_json_arg = flask.request.headers.get('X-Json-Arg')",
                "x_json_arg = json.loads(x_json_arg)",
                "x_json_arg = HeaderArg.from_dict(x_json_arg)  # noqa: E501");
        assertFileNotContains(controller, "pydantic");

        final Path controllerTest = Paths.get(outputPath + "openapi_server/test/test_hooks_controller.py");
        assertFileExists(controllerTest);
        assertFileNotContains(controllerTest, "pydantic");
    }

    @Test
    public void testWebhookConnexion3SkipsMultipleProduces() throws IOException {
        final PythonFlaskConnexionServerCodegen codegen = new PythonFlaskConnexionServerCodegen();
        codegen.additionalProperties().put(PythonFlaskConnexionServerCodegen.USE_CONNEXION_3, true);
        final String outputPath = generateFiles(codegen, "src/test/resources/3_1/python/webhooks.yaml");

        final Path controllerTest = Paths.get(outputPath + "openapi_server/test/test_hooks_controller.py");
        assertFileExists(controllerTest);
        assertFileNotContains(controllerTest, "pydantic");
        assertFileContains(
                controllerTest,
                "@unittest.skip(\"Connexion 3 requires the handler to specify which content type to return");
    }

    @Test(description = "free-form object query/header parameters are passed through, not converted with object.from_dict")
    public void testFreeFormObjectParametersAreNotConvertedWithFromDict() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/free-form-object-params.yaml");

        // Connexion passes {} for an absent object query parameter, so "object.from_dict(meta)" raised
        // AttributeError (500) on every request.
        final Path controller = Paths.get(outputPath, "openapi_server/controllers/default_controller.py");
        assertFileExists(controller);
        assertFileContains(controller, "x_extra = flask.request.headers.get('X-Extra')");
        assertFileNotContains(controller, "object.from_dict");
        // A named model without a top-level "type" (allOf) is still converted.
        assertFileContains(controller, "composed = Composed.from_dict(composed)");
    }

    @Test(description = "handler argument names append trailing underscore for Python builtins and keywords to match Connexion pythonic_params")
    public void testBuiltinAndKeywordParameterNames() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        Assert.assertEquals(codegen.toParamName("id"), "id_");
        Assert.assertEquals(codegen.toParamName("filter"), "filter_");
        Assert.assertEquals(codegen.toParamName("type"), "type_");
        Assert.assertEquals(codegen.toParamName("format"), "format_");
        Assert.assertEquals(codegen.toParamName("from"), "from_");
        Assert.assertEquals(codegen.toParamName("aiter"), "aiter_");
        Assert.assertEquals(codegen.toParamName("anext"), "anext_");
        Assert.assertEquals(codegen.toParamName("pageSize"), "page_size");
        // Connexion keeps leading underscores and passes 'callback' unchanged;
        // parent reserved words that are not builtins/keywords stay as-is too.
        Assert.assertEquals(codegen.toParamName("_from"), "_from");
        Assert.assertEquals(codegen.toParamName("callback"), "callback");
        Assert.assertEquals(codegen.toParamName("date"), "date");
        Assert.assertEquals(codegen.toParamName("self"), "self");
        Assert.assertEquals(codegen.toParamName("json"), "json");

        final DefaultCodegen mappedCodegen = new PythonFlaskConnexionServerCodegen();
        mappedCodegen.parameterNameMapping().put("filter", "custom_filter");
        Assert.assertEquals(mappedCodegen.toParamName("filter"), "custom_filter");

        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/builtin-param-names.yaml");

        final Path controller = Paths.get(outputPath, "openapi_server/controllers/default_controller.py");
        assertFileExists(controller);
        assertFileContains(
                controller,
                "def get_thing(id_, filter_=None, type_=None, format_=None, from_=None, page_size=None):",
                // id arrives via a $ref'd components/parameters entry
                "def get_probe(id_, _from=None, callback=None, date=None, self=None):",
                // a JSON header named 'json' must not shadow the json module
                // used by the template for JSON content headers
                "_json = flask.request.headers.get('json')",
                "_json = json.loads(_json)",
                // form fields keep Connexion's pythonic names in the signature
                "def post_thing_form(filter_, type_=None):",
                "def post_thing_multipart(filter_=None):");

        final Path openapiYaml = Paths.get(outputPath, "openapi_server/openapi/openapi.yaml");
        assertFileExists(openapiYaml);
        assertFileContains(
                openapiYaml,
                "/things/{id}:",
                "/probe/{id}:",
                "name: id",
                "name: filter",
                "name: type",
                "name: format",
                "name: from",
                "name: _from",
                "name: callback",
                "name: date",
                "name: self",
                "name: json",
                "name: pageSize");
        assertFileNotContains(
                openapiYaml,
                "/things/{id_}:",
                "/probe/{id_}:",
                "name: id_",
                "name: filter_",
                "name: type_",
                "name: format_",
                "name: from_",
                "name: page_size",
                "name: _json",
                "x-python-connexion-openapi-name");

        final Path controllerTest = Paths.get(outputPath, "openapi_server/test/test_default_controller.py");
        assertFileExists(controllerTest);
        assertFileContains(
                controllerTest,
                "('filter', 'filter__example')",
                "('type', 'type__example')",
                "('format', 'format__example')",
                "('from', 'from__example')",
                "('pageSize', 56)",
                "'/v1/things/{id_}'.format(id_='id__example')",
                // generated tests send form data under the wire (original) names
                "data = { 'filter': 'filter__example',");
    }

    @Test(description = "object-typed query/header parameters are deserialized by the generated server and sent"
            + " in their OpenAPI style by the generated controller test")
    public void testObjectTypedQueryAndHeaderParameters() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/object-typed-params.yaml");

        assertFileExists(Paths.get(outputPath, "openapi_server/param_parsing.py"));
        assertFileContains(Paths.get(outputPath, "openapi_server/__main__.py"),
                "options={'uri_parser_class': param_parsing.ObjectParameterURIParser}",
                "validator_map={'parameter': param_parsing.ObjectParameterValidator}");
        assertFileContains(Paths.get(outputPath, "openapi_server/test/__init__.py"),
                "options={'uri_parser_class': param_parsing.ObjectParameterURIParser}",
                "validator_map={'parameter': param_parsing.ObjectParameterValidator}");

        // Connexion never passes header parameters to the handler, so the handler parses the header itself.
        final Path controller = Paths.get(outputPath, "openapi_server/controllers/default_controller.py");
        assertFileContains(controller, "from openapi_server import param_parsing",
                "x_trace = param_parsing.parse_simple_object(x_trace, explode=False, model=Trace)");
        // A header serialized via "content" (JSON) keeps the JSON path and is not parsed by style.
        assertFileContains(controller, "x_meta = json.loads(x_meta)");
        assertFileNotContains(controller, "parse_simple_object(x_meta");

        // style=form (exploded), style=deepObject, style=form with explode=false, and a style=simple header.
        final Path controllerTest = Paths.get(outputPath, "openapi_server/test/test_default_controller.py");
        assertFileContains(controllerTest,
                "query_string = [('name', 'abc'), ('size', '56'), ('active', 'true'), ('kind', 'small'),"
                        + " ('since', '2013-10-20'),",
                "('deep[name]', 'abc'), ('deep[size]', '56'), ('deep[active]', 'true'), ('deep[kind]', 'small'),"
                        + " ('deep[since]', '2013-10-20'),",
                "('packed', 'name,abc,size,56,active,true,kind,small,since,2013-10-20'),",
                // a property that shares its parameter's name
                "('label', 'tag'),",
                "('limit', 56),",
                "'X-Trace': 'id,id_example,hops,56,sampled,false',");
        // allOf, pipeDelimited and objects with non-scalar properties are not handled by param_parsing.py,
        // so they are not style-encoded.
        assertFileNotContains(controllerTest, "('composed[", "('piped[", "('inner'", "('nested[");
        // an explicit parameter example is used; keys are not HTML-escaped
        assertFileContains(controllerTest, "'X-Hint': 'id,from-example,hops,7',", "('amp[a&b]', 'amp')");
        assertFileNotContains(controllerTest, "&amp;");
        assertFileContains(controllerTest, "'X-O\\'Brien': ");
        // the handler reads the same, unescaped header name
        assertFileContains(Paths.get(outputPath, "openapi_server/controllers/default_controller.py"),
                "x_o_brien = flask.request.headers.get('X-O\\'Brien')");
    }

    @Test(description = "object-typed parameter parsing is registered for Connexion 3 as well")
    public void testObjectTypedParametersConnexion3() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        codegen.additionalProperties().put("useConnexion3", true);
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/object-typed-params.yaml");

        for (String file : new String[]{"openapi_server/__main__.py", "openapi_server/test/__init__.py"}) {
            assertFileContains(Paths.get(outputPath, file),
                    "uri_parser_class=param_parsing.ObjectParameterURIParser,",
                    "validator_map={'parameter': param_parsing.ObjectParameterValidator}");
        }
    }

    @Test(description = "object-typed parameter parsing is registered for python-aiohttp")
    public void testObjectTypedParametersAiohttp() throws IOException {
        final DefaultCodegen codegen = new PythonAiohttpConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/object-typed-params.yaml");

        assertFileExists(Paths.get(outputPath, "openapi_server/param_parsing.py"));
        for (String file : new String[]{"openapi_server/__init__.py", "tests/conftest.py"}) {
            assertFileContains(Paths.get(outputPath, file),
                    "\"uri_parser_class\": param_parsing.ObjectParameterURIParser",
                    "validator_map={'parameter': param_parsing.ObjectParameterValidator}");
        }
        assertFileContains(Paths.get(outputPath, "openapi_server/controllers/default_controller.py"),
                "x_trace = param_parsing.parse_simple_object(x_trace, explode=False, model=Trace)");
    }

    @Test(description = "specs without object-typed query/header parameters do not get param_parsing.py")
    public void testNoObjectTypedParametersNoParamParsing() throws IOException {
        final DefaultCodegen codegen = new PythonFlaskConnexionServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-flask/petstore.yaml");

        Assert.assertFalse(Files.exists(Paths.get(outputPath, "openapi_server/param_parsing.py")));
        assertFileNotContains(Paths.get(outputPath, "openapi_server/__main__.py"), "param_parsing");
    }
}
