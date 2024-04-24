package org.openapitools.codegen.typescript.typescriptangular;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;


@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_ANGULAR})
public class TypeScriptAngularClientCodegenTest {
    @Test
    public void toVarName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.processOpts();
        Assertions.assertEquals(codegen.toVarName("valid_var"), "valid_var");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PROPERTY_NAMING, "camelCase");
        codegen.processOpts();
        Assertions.assertEquals(codegen.toVarName("valid_var"), "validVar");
    }

    @Test
    public void toEnumVarName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        // unspecified option should default to PascalCase
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumVarName("valid_id", "string"), "ValidId");
        Assertions.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "IllegalId");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumVarName("valid_id", "string"), "valid_id");
        Assertions.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumVarName("valid_id", "string"), "VALID_ID");
        Assertions.assertEquals(codegen.toEnumVarName("illegal-id+", "string"), "ILLEGAL_ID");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumVarName("valid_ID", "string"), "valid_id");
        Assertions.assertEquals(codegen.toEnumVarName("Illegal-Id+", "string"), "illegal_id");
    }

    @Test
    public void testModelSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assertions.assertEquals(codegen.toModelName("TestName"), "TestNameMySuffix");
        Assertions.assertEquals(codegen.toModelName("Error"), "ErrorMySuffix");
    }

    @Test
    public void testToEnumName() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "Enum");
        codegen.processOpts();

        Assertions.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestNameEnum");
        Assertions.assertEquals(codegen.toEnumName(makeEnumProperty("123")), "_123Enum");

        // enum value should not use model suffix
        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.MODEL_SUFFIX, "Model");
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "Enum2");
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestNameEnum2");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.ENUM_NAME_SUFFIX, "");
        codegen.processOpts();
        Assertions.assertEquals(codegen.toEnumName(makeEnumProperty("TestName")), "TestName");
    }

    private CodegenProperty makeEnumProperty(String name) {
        CodegenProperty enumProperty = new CodegenProperty();
        enumProperty.name = name;
        return enumProperty;
    }

    @Test
    public void testModelFileSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("modelFileSuffix", "MySuffix");
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assertions.assertEquals("./testNameMySuffix", codegen.toModelFilename("testName"));
    }

    @Test
    public void testOperationIdParser() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation1 = new Operation().operationId("123_test_@#$%_special_tags").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")));
        openAPI.path("another-fake/dummy/", new PathItem().get(operation1));
        final TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenOperation co1 = codegen.fromOperation("/another-fake/dummy/", "get", operation1, null);
        org.testng.Assertions.assertEquals(co1.operationId, "_123testSpecialTags");

    }

    @Test
    public void testSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assertions.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

    @Test
    public void testRemovePrefixSuffix() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        // simple noop test
        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));

        codegen.setModelNamePrefix("abc");
        codegen.setModelNameSuffix("def");
        codegen.additionalProperties().put("modelSuffix", "Ghi");
        codegen.processOpts();

        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));
        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameGhi"));
        Assertions.assertEquals("TestNameghi", codegen.removeModelPrefixSuffix("TestNameghi"));
        Assertions.assertEquals("abcTestName", codegen.removeModelPrefixSuffix("abcTestName"));
        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("AbcTestName"));
        Assertions.assertEquals("AbcTestName", codegen.removeModelPrefixSuffix("AbcAbcTestName"));
        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDef"));
        Assertions.assertEquals("TestNamedef", codegen.removeModelPrefixSuffix("TestNamedef"));
        Assertions.assertEquals("TestNamedefghi", codegen.removeModelPrefixSuffix("TestNamedefghi"));
        Assertions.assertEquals("TestNameDefghi", codegen.removeModelPrefixSuffix("TestNameDefghi"));
        Assertions.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDefGhi"));
    }

    @Test
    public void testSchema() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();

        ComposedSchema composedSchema = new ComposedSchema();

        Schema<Object> schema1 = new Schema<>();
        schema1.set$ref("SchemaOne");
        Schema<Object> schema2 = new Schema<>();
        schema2.set$ref("SchemaTwo");
        Schema<Object> schema3 = new Schema<>();
        schema3.set$ref("SchemaThree");

        composedSchema.addAnyOfItem(schema1);
        composedSchema.addAnyOfItem(schema2);
        composedSchema.addAnyOfItem(schema3);

        String schemaType = codegen.getSchemaType(composedSchema);
        Assertions.assertEquals(schemaType, "SchemaOne | SchemaTwo | SchemaThree");
    }

    @Test
    public void testKebabCasedModelFilenames() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(TypeScriptAngularClientCodegen.FILE_NAMING, "kebab-case");
        codegen.processOpts();

        final String modelName = "FooResponse__links";
        final Schema schema = new Schema()
                .name(modelName)
                .description("an inline model with name previously prefixed with underscore")
                .addRequiredItem("self")
                .addProperty("self", new StringSchema());

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("test", schema);
        codegen.setOpenAPI(openAPI);

        Assertions.assertEquals(codegen.toModelImport(modelName), "../model/foo-response-links");
        Assertions.assertEquals(codegen.toModelFilename(modelName), "./foo-response-links");
    }

    @Test
    public void testToParamNaming() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        // unspecified option should default to camelcase
        codegen.processOpts();
        Assertions.assertEquals(codegen.toParamName("valid_id"), "validId");
        Assertions.assertEquals(codegen.toParamName("illegal-id+"), "illegalId");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.original.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toParamName("valid_id"), "valid_id");
        Assertions.assertEquals(codegen.toParamName("illegal-id+"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.snake_case.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toParamName("valid_ID"), "valid_id");
        Assertions.assertEquals(codegen.toParamName("Illegal-Id+"), "illegal_id");

        codegen = new TypeScriptAngularClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_TYPE.PascalCase.name());
        codegen.processOpts();
        Assertions.assertEquals(codegen.toParamName("valid_id"), "ValidId");
        Assertions.assertEquals(codegen.toParamName("illegal-id+"), "IllegalId");
    }

    @Test
    public void testCorrectlyProducesImportsWithImportMapping() {
        TypeScriptAngularClientCodegen codegen = new TypeScriptAngularClientCodegen();
        final String importedModel = "SharedApiModel";
        final String importName = "@lib/custom/model";
        codegen.importMapping().put(importedModel, importName);
        Assertions.assertEquals(codegen.toModelImport(importedModel), importName);
    }

    @Test
    public void testTaggedUnionImports() throws Exception {
        final String specPath = "src/test/resources/3_0/allOf_composition_discriminator_recursive.yaml";

        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, "true");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        TestUtils.assertFileContains(
                Paths.get(output + "/model/expressionToken.ts"),
                "import { Token } from './token'", // imports the parent schema
                "import { TokenMetadata } from './tokenMetadata'", // imports a schema referenced in an inherited property
                "export interface ExpressionToken {" // no inheritance
        );

        TestUtils.assertFileNotContains(
                Paths.get(output + "/model/stringToken.ts"),
                "import { Token } from './token'"
        );

        TestUtils.assertFileContains(
                Paths.get(output + "/model/token.ts"),
                "import { ExpressionToken } from './expressionToken'",
                "export type Token = ExpressionToken | StringToken"
        );
    }

    @Test
    public void testModelNameMappings() throws Exception {
        final String specPath = "src/test/resources/2_0/issue_8289.json";

        Map<String, Object> properties = new HashMap<>();
        properties.put(TypeScriptAngularClientCodegen.TAGGED_UNIONS, "true");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        Map<String, String> modelNames = new HashMap<>();
        modelNames.put("File", "SystemFile");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setModelNameMappings(modelNames)
                .setGeneratorName("typescript-angular")
                .setInputSpec(specPath)
                .setAdditionalProperties(properties)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator();
        generator.opts(clientOptInput).generate();

        TestUtils.assertFileContains(
                Paths.get(output + "/model/folder.ts"),
                "files?: Array<SystemFile>;" // ensure it's an array of SystemFile (not Any)
        );
    }
}
