package org.openapitools.codegen.typescript.typescriptnode;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class TypeScriptNodeClientCodegenTest {

    private TypeScriptNodeClientCodegen codegen;

    @BeforeMethod
    public void setUp() {
        codegen = new TypeScriptNodeClientCodegen();
    }

    @Test
    public void convertVarName() throws Exception {
        TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
        Assert.assertEquals(codegen.toVarName("name"), "name");
        Assert.assertEquals(codegen.toVarName("$name"), "$name");
        Assert.assertEquals(codegen.toVarName("nam$$e"), "nam$$e");
        Assert.assertEquals(codegen.toVarName("user-name"), "userName");
        Assert.assertEquals(codegen.toVarName("user_name"), "userName");
        Assert.assertEquals(codegen.toVarName("user|name"), "userName");
        Assert.assertEquals(codegen.toVarName("user !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~name"), "user$Name");
    }

    @Test
    public void testSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptNodeClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));
    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptNodeClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));
    }

    @Test(description = "prepend model filename with ./ by default")
    public void defaultModelFilenameTest() {
        Assert.assertEquals(codegen.toModelFilename("ApiResponse"), "./apiResponse");
    }

    @Test(description = "use mapped name for model filename when provided")
    public void modelFilenameWithMappingTest() {
        final String mappedName = "@namespace/dir/response";
        codegen.importMapping().put("ApiResponse", mappedName);

        Assert.assertEquals(codegen.toModelFilename("ApiResponse"), mappedName);
    }

    @Test(description = "prepend model import with ./ by default")
    public void defaultModelImportTest() {
        Assert.assertEquals(codegen.toModelImport("ApiResponse"), "model/apiResponse");
    }

    @Test(description = "use mapped name for model import when provided")
    public void modelImportWithMappingTest() {
        final String mappedName = "@namespace/dir/response";
        codegen.importMapping().put("ApiResponse", mappedName);

        Assert.assertEquals(codegen.toModelImport("ApiResponse"), mappedName);
    }

    @Test(description = "append api suffix to default api filename")
    public void emptyApiFilenameTest() {
        Assert.assertEquals(codegen.toApiFilename(""), "defaultApi");
    }

    @Test(description = "appends api suffix to api filename")
    public void defaultApiFilenameTest() {
        Assert.assertEquals(codegen.toApiFilename("Category"), "categoryApi");
    }

    @Test(description = "appends api suffix to mapped api filename")
    public void mappedApiFilenameTest() {
        final String mappedName = "@namespace/dir/category";
        codegen.importMapping().put("Category", mappedName);
        Assert.assertEquals(codegen.toApiFilename("Category"), mappedName);
    }

    @Test(description = "append api suffix to default api import")
    public void emptyApiImportTest() {
        Assert.assertEquals(codegen.toApiImport(""), "api/defaultApi");
    }

    @Test(description = "appends api suffix to api import")
    public void defaultApiImportTest() {
        Assert.assertEquals(codegen.toApiImport("Category"), "api/categoryApi");
    }

    @Test(description = "appends api suffix to mapped api filename")
    public void mappedApiImportTest() {
        final String mappedName = "@namespace/dir/category";
        codegen.importMapping().put("Category", mappedName);
        Assert.assertEquals(codegen.toApiImport("Category"), mappedName);
    }

}
