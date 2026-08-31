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
        assertFileContains(p, "user: Annotated[List[User], Field(description=\"List of user object\")] = Body(..., description=\"List of user object\", examples=[[{\"username\": \"foo\"}, {\"username\": \"bar\"}]])");
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

    @Test(description = "binary multipart form fields are typed as FastAPI UploadFile")
    public void testBinaryMultipartFieldUsesUploadFile() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_20115.yaml");
        final Path api = Paths.get(outputPath + "src/openapi_server/apis/default_api.py");
        final Path baseApi = Paths.get(outputPath + "src/openapi_server/apis/default_api_base.py");

        assertFileExists(api);
        assertFileExists(baseApi);

        // Required binary form field becomes `UploadFile = File(...)`
        assertFileContains(api, "csv_file: UploadFile = File(..., description=\"The CSV file to upload\", alias=\"csv_file\")");
        // Optional binary form field becomes `Optional[UploadFile] = File(None, ...)`
        assertFileContains(api, "image: Optional[UploadFile] = File(None, description=\"Optional image upload\", alias=\"image\")");

        // Sibling non-binary form fields still use Form()
        assertFileContains(api, "collection_name: Annotated[StrictStr, Field(description=\"Name of the collection\")] = Form(..., description=\"Name of the collection\", alias=\"collection_name\")");

        // The legacy client-side bytes union must not appear for the server signature
        assertFileNotContains(api, "Union[StrictBytes, StrictStr, Tuple[StrictStr, StrictBytes]]");
        assertFileNotContains(baseApi, "Union[StrictBytes, StrictStr, Tuple[StrictStr, StrictBytes]]");

        // FastAPI File/UploadFile imports are emitted
        assertFileContains(api, "from fastapi import File, UploadFile");
        assertFileContains(baseApi, "from fastapi import File, UploadFile");

        // Abstract base class uses UploadFile directly (no Annotated wrapper)
        assertFileContains(baseApi, "csv_file: UploadFile,");
        assertFileContains(baseApi, "image: Optional[UploadFile],");
    }

    @Test(description = "multipart array of binary form fields are typed as List[UploadFile]")
    public void testMultipartArrayOfBinaryUsesListUploadFile() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");
        final Path api = Paths.get(outputPath + "src/openapi_server/apis/multipart_api.py");
        final Path baseApi = Paths.get(outputPath + "src/openapi_server/apis/multipart_api_base.py");

        assertFileExists(api);
        assertFileExists(baseApi);

        assertFileContains(api, "files: Optional[List[UploadFile]] = File(None, description=\"Many files\", alias=\"files\")");
        assertFileContains(baseApi, "files: Optional[List[UploadFile]],");

        assertFileContains(api, "file: Optional[UploadFile] = File(None, description=\"One file\", alias=\"file\")");
        assertFileContains(baseApi, "file: Optional[UploadFile],");

        assertFileNotContains(api, "files: Optional[UploadFile] = File(None, description=\"Many files\")");
        assertFileNotContains(baseApi, "files: Optional[UploadFile],");
    }

    @Test(description = "multipart Form/File use OpenAPI wire names via alias (#17111 parity for form fields)")
    public void testMultipartFormFieldsUseWireNameAlias() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String multipartPath = generateFiles(codegen, "src/test/resources/3_0/form-multipart-binary-array.yaml");
        final Path multipartApi = Paths.get(multipartPath + "src/openapi_server/apis/multipart_api.py");

        assertFileExists(multipartApi);
        assertFileContains(multipartApi, "status_array: Optional[List[MultipartMixedStatus]] = Form(None, description=\"\", alias=\"statusArray\")");

        final DefaultCodegen petstoreCodegen = new PythonFastAPIServerCodegen();
        final String petstorePath = generateFiles(petstoreCodegen, "src/test/resources/3_0/python-fastapi/petstore.yaml");
        final Path petApi = Paths.get(petstorePath + "src/openapi_server/apis/pet_api.py");

        assertFileExists(petApi);
        assertFileContains(petApi, "additional_metadata: Annotated[Optional[StrictStr], Field(description=\"Additional data to pass to server\")] = Form(None, description=\"Additional data to pass to server\", alias=\"additionalMetadata\")");
        assertFileContains(petApi, "file: Optional[UploadFile] = File(None, description=\"file to upload\", alias=\"file\")");
    }

    @Test(description = "binary response body is typed as bytes, not invalid file (#20775)")
    public void testBinaryResponseUsesBytesNotFile() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/3_0/issue_20775.yaml");
        final Path api = Paths.get(outputPath + "src/openapi_server/apis/resource_api.py");
        final Path baseApi = Paths.get(outputPath + "src/openapi_server/apis/resource_api_base.py");

        assertFileExists(api);
        assertFileExists(baseApi);

        assertFileContains(api, "-> bytes");
        assertFileContains(api, "\"model\": bytes");
        assertFileNotContains(api, "-> file");
        assertFileNotContains(api, "\"model\": file");

        assertFileContains(baseApi, "-> bytes");
        assertFileNotContains(baseApi, "-> file");
    }

    /**
     * Verifies that parameters arriving on the wire as strings (path, query, header, cookie)
     * are typed with coercible Pydantic types ({@code int}/{@code bool}/{@code str}) instead of
     * strict ones ({@code StrictInt}/{@code StrictBool}/{@code StrictStr}, {@code strict=True}),
     * which would disable Pydantic's string coercion and make FastAPI reject valid requests
     * with a 422 (#21905).
     *
     * <p>Schema constraints (e.g. {@code ge}/{@code le}) must be preserved, while JSON body
     * model properties must keep strict typing since bodies carry real JSON types.
     */
    @Test(description = "path/query/header/cookie params use coercible types, not strict types (#21905)")
    public void testWireStringParamsUseCoercibleTypes() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_21905.yaml");
        final Path api = Paths.get(outputPath + "src/openapi_server/apis/item_api.py");
        final Path baseApi = Paths.get(outputPath + "src/openapi_server/apis/item_api_base.py");
        final Path model = Paths.get(outputPath + "src/openapi_server/models/item.py");

        assertFileExists(api);
        assertFileExists(baseApi);

        // path param: coercible int
        assertFileContains(api, "itemId: int = Path(..., description=\"\")");
        // query param: coercible int, constraints kept but no strict=True
        assertFileContains(api, "limit: Optional[Annotated[int, Field(le=100, ge=1)]] = Query(None, description=\"\", alias=\"limit\", ge=1, le=100)");
        // header param: coercible bool
        assertFileContains(api, "x_verbose: Optional[bool] = Header(None, description=\"\")");
        // cookie params: values also arrive as strings on the wire, so they must be coercible too
        assertFileContains(api, "session_id: Optional[int] = Cookie(None, description=\"\")");
        assertFileContains(api, "dark_mode: Optional[bool] = Cookie(None, description=\"\")");

        // no strict types anywhere in the endpoint signatures
        assertFileNotContains(api, "StrictInt");
        assertFileNotContains(api, "StrictBool");
        assertFileNotContains(api, "StrictStr");
        assertFileNotContains(api, "strict=True");
        assertFileNotContains(baseApi, "StrictInt");
        assertFileNotContains(baseApi, "StrictBool");
        assertFileNotContains(baseApi, "StrictStr");
        assertFileNotContains(baseApi, "strict=True");

        // JSON body model properties keep strict typing (real JSON types, no wire-string coercion)
        assertFileContains(model, "count: Optional[StrictInt] = None");
    }

    /**
     * Verifies that endpoint argument commas stay at the end of the parameter line instead of
     * being wrapped onto a line of their own. The {@code endpoint_argument_definition} partial
     * is included inline (followed by {@code ,}) in api.mustache, so a trailing newline in the
     * partial leaks into the output and produces the broken ")\n," style (#22494).
     */
    @Test(description = "endpoint argument commas stay at end of line, no newline before comma (#22494)")
    public void testEndpointArgumentCommaStaysOnSameLine() throws IOException {
        final DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        final String outputPath = generateFiles(codegen, "src/test/resources/bugs/issue_21905.yaml");
        final Path api = Paths.get(outputPath + "src/openapi_server/apis/item_api.py");

        assertFileExists(api);

        // NOTE: assertFileContains linearizes away newlines, so raw content checks are required here
        final String content = Files.readString(api);

        // commas terminate the parameter line
        Assert.assertTrue(content.contains("itemId: int = Path(..., description=\"\"),\n"),
                "parameter line should end with a comma: " + api);
        Assert.assertTrue(content.contains("session_id: Optional[int] = Cookie(None, description=\"\"),\n"),
                "parameter line should end with a comma: " + api);
        Assert.assertTrue(content.contains("dark_mode: Optional[bool] = Cookie(None, description=\"\"),\n"),
                "parameter line should end with a comma: " + api);

        // the comma must never be wrapped onto its own line
        Assert.assertFalse(content.contains("\n,\n"),
                "comma wrapped onto its own line in: " + api);
    }
}
