package org.openapitools.codegen.markdown;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.MarkdownDocumentationCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class MarkdownSampleGeneratorTest {

    @Test(description = "test special characters in MIME type")
    public void testSpecialCharactersInMimeType() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_15923.yaml");
        final MarkdownDocumentationCodegen codegen = new MarkdownDocumentationCodegen();
        codegen.setOpenAPI(openAPI);

        final String requestPath = "/v1/MyRequest";
        Operation textOperation = openAPI.getPaths().get(requestPath).getPut();
        CodegenOperation operation = codegen.fromOperation(requestPath, "ptu", textOperation, null);
        CodegenParameter codegenParameter = operation.allParams.get(0);
        Assert.assertNotNull(codegenParameter);
    }

}