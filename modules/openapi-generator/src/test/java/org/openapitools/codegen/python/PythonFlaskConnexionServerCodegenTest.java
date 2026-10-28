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
