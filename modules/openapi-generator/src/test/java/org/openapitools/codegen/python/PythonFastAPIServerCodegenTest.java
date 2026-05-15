package org.openapitools.codegen.python;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.PythonFastAPIServerCodegen;
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
import static org.openapitools.codegen.TestUtils.assertFileNotContains;

public class PythonFastAPIServerCodegenTest {

    /** Exposes protected toPythonExample for unit testing. */
    private static class TestableFastAPICodegen extends PythonFastAPIServerCodegen {
        public String exposeToPythonExample(CodegenProperty cp) {
            return toPythonExample(cp);
        }
    }

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


    @Test(description = "test containerType in parameters")
    public void testContainerType() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/pr_18691.json");
        final Path p = Paths.get(outputPath + "src/openapi_server/apis/default_api.py");

        assertFileExists(p);
        assertFileContains(p, "body: Optional[Dict[str, Any]] = Body(None, description=\"\"),");
    }

    @Test(description = "request body examples are rendered into FastAPI Body metadata")
    public void testRequestBodyExampleInBodyMetadata() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-fastapi/petstore-with-examples.yaml");
        final Path p = Paths.get(outputPath + "src/openapi_server/apis/user_api.py");

        assertFileExists(p);
        assertFileContains(p, "user: Annotated[List[User], Field(description=\"List of user object\")] = Body(None, description=\"List of user object\", examples=[[{\"username\": \"foo\"}, {\"username\": \"bar\"}]])");
        assertFileNotContains(p, "examples=[[[],");
    }

    @Test(description = "schema property examples are rendered into FastAPI metadata")
    public void testSchemaPropertyExamplesInMetadata() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/python-fastapi/petstore-with-examples.yaml");
        final Path model = Paths.get(outputPath + "src/openapi_server/models/pet.py");

        assertFileExists(model);
        assertFileContains(model, "name: StrictStr = Field(json_schema_extra={\"examples\": [\"doggie\"]})");
        assertFileNotContains(model, "json_schema_extra={\"examples\": [\"''\"]}");
    }

    @Test(description = "toPythonExample picks first entry from plural examples array in jsonSchema")
    public void testToPythonExampleWithPluralExamples() {
        final TestableFastAPICodegen codegen = new TestableFastAPICodegen();
        CodegenProperty cp = new CodegenProperty();
        cp.name = "nickname";
        cp.jsonSchema = "{\"type\": \"string\", \"examples\": [\"buddy\", \"pal\"]}";

        Assert.assertEquals(codegen.exposeToPythonExample(cp), "\"buddy\"");
    }

    @Test(description = "toPythonExample prefers singular example over plural examples in jsonSchema")
    public void testToPythonExamplePrefersExampleOverExamples() {
        final TestableFastAPICodegen codegen = new TestableFastAPICodegen();
        CodegenProperty cp = new CodegenProperty();
        cp.name = "nickname";
        cp.jsonSchema = "{\"type\": \"string\", \"example\": \"doggie\", \"examples\": [\"buddy\", \"pal\"]}";

        Assert.assertEquals(codegen.exposeToPythonExample(cp), "\"doggie\"");
    }
}
