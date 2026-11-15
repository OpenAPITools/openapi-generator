package org.openapitools.codegen.typescript.typescriptnestjs;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptNestjsClientCodegen;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.assertj.core.api.Assertions.assertThat;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_NESTJS})
public class TypeScriptNestjsClientCodegenTest {
    @Test
    public void testModelFileSuffix() {
        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();
        codegen.additionalProperties().put("modelFileSuffix", "MySuffix");
        codegen.additionalProperties().put("modelSuffix", "MySuffix");
        codegen.processOpts();

        Assert.assertEquals("testNameMySuffix", codegen.toModelFilename("testName"));
    }

    @Test
    public void testOperationIdParser() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        Operation operation1 = new Operation().operationId("123_test_@#$%_special_tags").responses(new ApiResponses().addApiResponse("201", new ApiResponse().description("OK")));
        openAPI.path("another-fake/dummy/", new PathItem().get(operation1));
        final TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();
        codegen.setOpenAPI(openAPI);

        CodegenOperation co1 = codegen.fromOperation("/another-fake/dummy/", "get", operation1, null);
        org.testng.Assert.assertEquals(co1.operationId, "_123testSpecialTags");

    }

    @Test
    public void testSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-nestjs-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

        codegen = new TypeScriptNestjsClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-nestjs-petstore");
        codegen.additionalProperties().put("snapshot", true);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

    }

    @Test
    public void testWithoutSnapshotVersion() {
        OpenAPI openAPI = TestUtils.createOpenAPI();

        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-nestjs-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

        codegen = new TypeScriptNestjsClientCodegen();
        codegen.additionalProperties().put("npmName", "@openapi/typescript-nestjs-petstore");
        codegen.additionalProperties().put("snapshot", false);
        codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
        codegen.processOpts();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

    }

    @Test
    public void testRemovePrefixSuffix() {
        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

        // simple noop test
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));

        codegen.setModelNamePrefix("abc");
        codegen.setModelNameSuffix("def");
        codegen.additionalProperties().put("modelSuffix", "Ghi");
        codegen.processOpts();

        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameGhi"));
        Assert.assertEquals("TestNameghi", codegen.removeModelPrefixSuffix("TestNameghi"));
        Assert.assertEquals("abcTestName", codegen.removeModelPrefixSuffix("abcTestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("AbcTestName"));
        Assert.assertEquals("AbcTestName", codegen.removeModelPrefixSuffix("AbcAbcTestName"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDef"));
        Assert.assertEquals("TestNamedef", codegen.removeModelPrefixSuffix("TestNamedef"));
        Assert.assertEquals("TestNamedefghi", codegen.removeModelPrefixSuffix("TestNamedefghi"));
        Assert.assertEquals("TestNameDefghi", codegen.removeModelPrefixSuffix("TestNameDefghi"));
        Assert.assertEquals("TestName", codegen.removeModelPrefixSuffix("TestNameDefGhi"));
    }

    @Test
    public void testSchema() {
        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

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
        Assert.assertEquals(schemaType, "SchemaOne | SchemaTwo | SchemaThree");
    }

    @Test
    public void testPathPropertyName() {
        TypeScriptNestjsClientCodegen codegen = new TypeScriptNestjsClientCodegen();

        CodegenOperation codegenOperation = new CodegenOperation();

        CodegenParameter pathParam1 = createPathParam("pathParam1", "path_param1");
        CodegenParameter pathParam2 = createPathParam("pathParam2", "pathParam2");

        codegenOperation.httpMethod = "GET";
        codegenOperation.path = "test/{path_param1}/{pathParam2}";
        codegenOperation.allParams.addAll(Arrays.asList(pathParam1, pathParam2));
        codegenOperation.pathParams.addAll(Arrays.asList(pathParam1, pathParam2));

        OperationMap operations = new OperationMap();
        operations.setOperation(codegenOperation);
        operations.setClassname("TestService");

        OperationsMap objs = new OperationsMap();
        objs.setOperation(operations);
        objs.setImports(List.of());

        var res = codegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assert.assertEquals(res.getOperations().getOperation().get(0).path,
            "test/${encodeURIComponent(String(pathParam1))}/${encodeURIComponent(String(pathParam2))}");
    }

    @Test
    public void testBooleanOptionsPassedAsStrings() throws IOException {
        // --additional-properties and gradle configOptions pass boolean options as strings
        for (Object option : new Object[]{null, false, "false", true, "true"}) {
            boolean enabled = Boolean.parseBoolean(String.valueOf(option));
            String label = option == null ? "unset" : option + " (" + option.getClass().getSimpleName() + ")";

            File output = Files.createTempDirectory("nestjs-boolean-options").toFile();
            output.deleteOnExit();
            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("typescript-nestjs")
                    .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                    .setOutputDir(output.getAbsolutePath());
            if (option != null) {
                configurator.addAdditionalProperty(TypeScriptNestjsClientCodegen.WITH_INTERFACES, option);
                configurator.addAdditionalProperty(TypeScriptNestjsClientCodegen.USE_SINGLE_REQUEST_PARAMETER, option);
            }
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            String service = Files.readString(output.toPath().resolve("api/multipart.service.ts"));
            assertThat(output.toPath().resolve("api/multipart.serviceInterface.ts").toFile().exists())
                    .as("interface file, withInterfaces=" + label).isEqualTo(enabled);
            assertThat(service.contains("implements MultipartServiceInterface"))
                    .as("implements clause, withInterfaces=" + label).isEqualTo(enabled);
            assertThat(service.contains("export interface MultipartServiceMultipartArrayRequest"))
                    .as("useSingleRequestParameter=" + label).isEqualTo(enabled);

            output = Files.createTempDirectory("nestjs-tagged-unions").toFile();
            output.deleteOnExit();
            configurator = new CodegenConfigurator()
                    .setGeneratorName("typescript-nestjs")
                    .setInputSpec("src/test/resources/3_0/typescript-angular/composed-schemas.yaml")
                    .setOutputDir(output.getAbsolutePath());
            if (option != null) {
                configurator.addAdditionalProperty(TypeScriptNestjsClientCodegen.TAGGED_UNIONS, option);
            }
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
            String model = Files.readString(output.toPath().resolve("model/catInherited.ts"));
            assertThat(model.contains("export interface CatInherited extends PetWithSimpleDiscriminator"))
                    .as("plain interface, taggedUnions=" + label).isEqualTo(!enabled);
        }
    }

    private CodegenParameter createPathParam(String paramName, String baseName) {
        CodegenParameter codegenParameter = new CodegenParameter();
        codegenParameter.paramName = paramName;
        codegenParameter.baseName = baseName;
        codegenParameter.dataType = "String";
        codegenParameter.isPathParam = true;
        return codegenParameter;
    }

}
