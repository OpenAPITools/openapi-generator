package org.openapitools.codegen.python;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PythonFlaskConnexionServerCodegen;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileExists;

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
}
