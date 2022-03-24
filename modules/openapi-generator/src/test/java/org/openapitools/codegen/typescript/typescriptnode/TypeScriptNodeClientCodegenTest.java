package org.openapitools.codegen.typescript.typescriptnode;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.*;

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

    @Test(description = "prepend model import with ../model by default")
    public void defaultModelImportTest() {
        Assert.assertEquals(codegen.toModelImport("ApiResponse"), "../model/apiResponse");
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

    @Test(description = "correctly produces imports without import mapping")
    public void postProcessOperationsWithModelsTestWithoutImportMapping() {
        final String importName = "../model/pet";
        Map<String, Object> operations = createPostProcessOperationsMapWithImportName(importName);

        codegen.postProcessOperationsWithModels(operations, Collections.emptyList());
        List<Map<String, Object>> extractedImports = (List<Map<String, Object>>) operations.get("imports");
        Assert.assertEquals(extractedImports.get(0).get("filename"), importName);
    }

    @Test(description = "correctly produces imports with import mapping")
    public void postProcessOperationsWithModelsTestWithImportMapping() {
        final String importName = "@namespace/dir/category";
        Map<String, Object> operations = createPostProcessOperationsMapWithImportName(importName);

        codegen.postProcessOperationsWithModels(operations, Collections.emptyList());
        List<Map<String, Object>> extractedImports = (List<Map<String, Object>>) operations.get("imports");

        Assert.assertEquals(extractedImports.get(0).get("filename"), importName);
    }

    @Test(description = "correctly produces imports with model name suffix")
    public void postProcessOperationsWithModelsTestWithModelNameSuffix() {
        final OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema rootSchema = new ObjectSchema()
            .addProperties("child", new Schema().$ref("Child"));
        final Schema childSchema = new ObjectSchema()
            .addProperties("key", new StringSchema());

        openAPI.getComponents()
            .addSchemas("Root", rootSchema)
            .addSchemas("Child", childSchema);

        final TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
        codegen.setModelNameSuffix("Suffix");

        final HashMap<String, Object> allModels = createParameterForPostProcessAllModels(
            codegen.fromModel("Root", rootSchema),
            codegen.fromModel("Child", childSchema)
        );
        final Map<String, Object> results = codegen.postProcessAllModels(allModels);
        final Map<String, Object> root = (Map<String, Object>) results.get("Root");
        final List<Map<String, Object>> modelsOfRoot = (List<Map<String, Object>>) root.get("models");
        final List<HashMap<String, String>> tsImports = (List<HashMap<String, String>>) modelsOfRoot.get(0)
            .get("tsImports");

        Assert.assertEquals(tsImports.size(), 1);
        Assert.assertEquals(tsImports.get(0).get("filename"), "./childSuffix");
    }

    @Test(description = "correctly produces imports with model name prefix")
    public void postProcessOperationsWithModelsTestWithModelNamePrefix() {
        final OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema rootSchema = new ObjectSchema()
            .addProperties("child", new Schema().$ref("Child"));
        final Schema childSchema = new ObjectSchema()
            .addProperties("key", new StringSchema());

        openAPI.getComponents()
            .addSchemas("Root", rootSchema)
            .addSchemas("Child", childSchema);

        final TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
        codegen.setModelNamePrefix("Prefix");

        final HashMap<String, Object> allModels = createParameterForPostProcessAllModels(
            codegen.fromModel("Root", rootSchema),
            codegen.fromModel("Child", childSchema)
        );
        final Map<String, Object> results = codegen.postProcessAllModels(allModels);
        final Map<String, Object> root = (Map<String, Object>) results.get("Root");
        final List<Map<String, Object>> modelsOfRoot = (List<Map<String, Object>>) root.get("models");
        final List<HashMap<String, String>> tsImports = (List<HashMap<String, String>>) modelsOfRoot.get(0)
            .get("tsImports");

        Assert.assertEquals(tsImports.size(), 1);
        Assert.assertEquals(tsImports.get(0).get("filename"), "./prefixChild");
    }

    private Map<String, Object> createPostProcessOperationsMapWithImportName(String importName) {
        Map<String, Object> operations = new HashMap<String, Object>() {{
            put("operation", Collections.emptyList());
            put("classname", "Pet");
        }};

        Map<String, Object> importList = new HashMap<String, Object>() {{
            put("import", importName);
            put("classname", "Pet");
        }};
        List<Map<String, Object>> imports = new ArrayList<>();
        imports.add(importList);
        return new HashMap<String, Object>() {{
            put("operations", operations);
            put("imports", imports);
        }};
    }

    private HashMap<String, Object> createParameterForPostProcessAllModels(CodegenModel root, CodegenModel child) {
        return new HashMap<String, Object>() {{
            put("Child", new HashMap<String, Object>() {{
                put("models", Collections.singletonList(
                    new HashMap<String, Object>() {{
                        put("importPath", "../model/child");
                        put("model", child);
                    }}
                ));
            }});
            put("Root", new HashMap<String, Object>() {{
                put("models", Collections.singletonList(
                    new HashMap<String, Object>() {{
                        put("importPath", "../model/root");
                        put("model", root);
                    }}
                ));
                put("imports", Collections.singletonList(
                    new HashMap<String, Object>() {{
                        put("import", "../model/child");
                    }}
                ));
            }});
        }};
    }
}
