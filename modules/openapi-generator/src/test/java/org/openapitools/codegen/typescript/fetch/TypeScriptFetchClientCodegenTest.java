package org.openapitools.codegen.typescript.fetch;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;


public class TypeScriptFetchClientCodegenTest {
    @Test
    public void testSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI api = TestUtils.createOpenAPI();
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();

        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-fetch-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(api);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

    @Test
    public void toVarName() {
        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "validVar");

        codegen = new TypeScriptFetchClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PROPERTY_NAMING, "original");
        codegen.processOpts();
        Assert.assertEquals(codegen.toVarName("valid_var"), "valid_var");
    }

    @Test
    public void getTypeDeclarationTest() {
        Schema<?> childSchema = new ArraySchema().items(new StringSchema());

        OpenAPI api = TestUtils.createOpenAPI();
        api.getComponents().addSchemas("Child", childSchema);

        TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        codegen.setOpenAPI(api);

        // Cf. issue #4968: Array of Alias of Array
        Schema<?> parentSchema = new ArraySchema().items(
            new Schema().$ref("#/components/schemas/Child")
        );

        ModelUtils.setGenerateAliasAsModel(false);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "Array<Array<string>>");

        ModelUtils.setGenerateAliasAsModel(true);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "Array<Child>");

        // Same for Map
        parentSchema = new MapSchema().additionalProperties(new Schema().$ref("#/components/schemas/Child"));

        ModelUtils.setGenerateAliasAsModel(false);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "{ [key: string]: Array<string>; }");

        ModelUtils.setGenerateAliasAsModel(true);
        Assert.assertEquals(codegen.getTypeDeclaration(parentSchema), "{ [key: string]: Child; }");
    }

}
