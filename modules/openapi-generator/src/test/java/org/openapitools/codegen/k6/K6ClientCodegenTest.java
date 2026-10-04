package org.openapitools.codegen.k6;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.K6ClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class K6ClientCodegenTest {

    @Test
    public void testQueryOperationIsSkipped() throws IOException {
        // generators without supportsAdditionalOperations() must skip OpenAPI 3.2
        // `query` operations: k6's JS http module has no `query` function, so
        // emitting them would generate a script that fails at runtime with
        // `TypeError: http.query is not a function`
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getPaths().addPathItem("/search",
                new PathItem()
                        .get(new Operation().operationId("getItems")
                                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("OK"))))
                        .query(new Operation().operationId("searchItems")
                                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("OK")))));

        Path scriptJs = generate(openAPI);
        try {
            TestUtils.assertFileContains(scriptJs, "http.get(");
            TestUtils.assertFileNotContains(scriptJs, "http.query(", "searchItems");
        } finally {
            FileUtils.deleteDirectory(scriptJs.getParent().toFile());
        }
    }

    @Test
    public void testQueryOnlyPathLeavesNoEmptyGroup() throws IOException {
        // a path declaring only a `query` operation must not leave an empty
        // group() block behind; the dataextract extension on the second path
        // additionally exercises the substitute-parameter pre-scan against a
        // spec containing a skipped path
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getPaths().addPathItem("/queryOnly",
                new PathItem().query(new Operation().operationId("searchItems")
                        .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("OK")))));

        Operation getPets = new Operation().operationId("getPets")
                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().description("OK")));
        getPets.addExtension("x-k6-openapi-operation-dataextract",
                Map.of("operationId", "getPets", "valuePath", "id", "parameterName", "petId"));
        openAPI.getPaths().addPathItem("/pets", new PathItem().get(getPets));

        Path scriptJs = generate(openAPI);
        try {
            TestUtils.assertFileContains(scriptJs, "http.get(");
            TestUtils.assertFileNotContains(scriptJs, "http.query(", "searchItems", "queryOnly");
        } finally {
            FileUtils.deleteDirectory(scriptJs.getParent().toFile());
        }
    }

    private Path generate(OpenAPI openAPI) throws IOException {
        File output = Files.createTempDirectory("test").toFile();

        K6ClientCodegen codegen = new K6ClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        ClientOptInput input = new ClientOptInput();
        input.openAPI(openAPI);
        input.config(codegen);

        new DefaultGenerator().opts(input).generate();

        return output.toPath().resolve("script.js");
    }
}
