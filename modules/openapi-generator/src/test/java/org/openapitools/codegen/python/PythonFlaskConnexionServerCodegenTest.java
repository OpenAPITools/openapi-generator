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
}
