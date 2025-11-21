package org.openapitools.codegen.nim;

import io.swagger.v3.oas.models.media.ObjectSchema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.NimClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class NimClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testUnderscoresEdgeCases() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Test model filename with trailing underscores
        String result = codegen.toModelFilename("Record_string__before_string_or_null__after_string_or_null___value");
        Assert.assertFalse(result.endsWith("_"), "Model filename should not end with underscore: " + result);

        // Test model filename with multiple consecutive underscores
        result = codegen.toModelFilename("Record_string_string_or_number__value");
        Assert.assertFalse(result.endsWith("_"), "Model filename should not end with underscore: " + result);

        // Verify no consecutive underscores remain (except the required prefix)
        Assert.assertFalse(result.contains("__"), "Model filename should not contain consecutive underscores: " + result);

        // Test model import with trailing underscores
        result = codegen.toModelImport("Record_string__before_string_or_null__after_string_or_null___value");
        Assert.assertFalse(result.endsWith("_"), "Model import should not end with underscore: " + result);

        // Test model import with multiple consecutive underscores
        result = codegen.toModelImport("Record_string_string_or_number__value");
        Assert.assertFalse(result.endsWith("_"), "Model import should not end with underscore: " + result);

        // Test API filename with trailing underscores
        result = codegen.toApiFilename("SomeApi_");
        Assert.assertFalse(result.endsWith("_"), "API filename should not end with underscore: " + result);

        // Test API import with trailing underscores
        result = codegen.toApiImport("SomeApi_");
        Assert.assertFalse(result.endsWith("_"), "API import should not end with underscore: " + result);
    }

    @Test
    public void testSanitizationPreservesNormalNames() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Verify that normal names without trailing underscores are not changed
        String result = codegen.toModelFilename("UserData");
        Assert.assertTrue(result.startsWith("model_"), "Model filename should start with model_");
        Assert.assertFalse(result.endsWith("_"), "Model filename should not end with underscore");

        result = codegen.toApiFilename("DefaultApi");
        Assert.assertTrue(result.startsWith("api_"), "API filename should start with api_");
        Assert.assertFalse(result.endsWith("_"), "API filename should not end with underscore");
    }

    @Test
    public void testObjectTypeMapping() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Test that object type is mapped to JsonNode to avoid Nim keyword conflict
        ObjectSchema objectSchema = new ObjectSchema();
        String result = codegen.getTypeDeclaration(objectSchema);

        // object types without properties should map to JsonNode
        Assert.assertEquals(result, "JsonNode",
            "Free-form object type should map to JsonNode to avoid Nim 'object' keyword conflict");
    }

    @Test
    public void testTypeNameConsistency() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Test that response type names don't have underscores between number and text
        String result = codegen.toModelName("GetComments_200_response");
        Assert.assertFalse(result.contains("_200_"), "Type name should not contain _200_: " + result);
        Assert.assertTrue(result.contains("200"), "Type name should contain 200: " + result);

        // The filename should also be consistent
        String filename = codegen.toModelFilename("GetComments_200_response");
        String importName = codegen.toModelImport("GetComments_200_response");

        // Extract the type name from the filename and import
        String filenameTypePart = filename.replace("model_", "");
        String importTypePart = importName.replace("model_", "");

        Assert.assertEquals(filenameTypePart, importTypePart,
            "Filename and import should reference the same type");
    }

    @Test
    public void testImportConsistencyAfterProcessing() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Simulate what happens during model processing:
        // 1. Model is created with original schema name
        String schemaName = "AddDomainConfig_200_response_anyOf";
        String typeName = codegen.toModelName(schemaName);  // Camelizes to AddDomainConfig200ResponseAnyOf
        String filename = codegen.toModelFilename(schemaName);  // Creates model_add_domain_config_200_response_any_of

        // 2. When another model imports this type, it uses the camelized type name
        String importFromTypeName = codegen.toModelImport(typeName);

        // The import should match the filename (or at least be loadable)
        Assert.assertEquals(filename, importFromTypeName,
            "Import generated from type name should match filename generated from schema name");
    }

    @Test
    public void testNormalizeSchemaName() throws Exception {
        final NimClientCodegen codegen = new NimClientCodegen();

        // Test that schema names with _200_ are normalized
        String result = codegen.toModelName("GetComments_200_response");
        Assert.assertEquals(result, "GetComments200response",
            "Should normalize _200_ to 200: " + result);

        // Test that schema names with trailing _1 are normalized
        result = codegen.toModelName("Config_anyOf_1");
        Assert.assertEquals(result, "ConfigAnyOf1",
            "Should normalize _1 to 1: " + result);

        // Verify consistency between filename and import
        String filename = codegen.toModelFilename("GetComments_200_response");
        String importPath = codegen.toModelImport("GetComments200response");
        Assert.assertEquals(filename, importPath,
            "Filename and import should match after normalization");
    }
}
