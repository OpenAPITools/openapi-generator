package org.openapitools.codegen.typescript.typescriptnode;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.*;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_NODE})
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
        OperationsMap operations = createPostProcessOperationsMapWithImportName(importName);

        codegen.postProcessOperationsWithModels(operations, Collections.emptyList());
        List<Map<String, String>> extractedImports = operations.getImports();
        Assert.assertEquals(extractedImports.get(0).get("filename"), importName);
    }

    @Test(description = "correctly produces imports with import mapping")
    public void postProcessOperationsWithModelsTestWithImportMapping() {
        final String importName = "@namespace/dir/category";
        OperationsMap operations = createPostProcessOperationsMapWithImportName(importName);

        codegen.postProcessOperationsWithModels(operations, Collections.emptyList());
        List<Map<String, String>> extractedImports = operations.getImports();

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
        codegen.setOpenAPI(openAPI);
        codegen.setModelNameSuffix("Suffix");

        final HashMap<String, ModelsMap> allModels = createParameterForPostProcessAllModels(
            codegen.fromModel("Root", rootSchema),
            codegen.fromModel("Child", childSchema)
        );
        final Map<String, ModelsMap> results = codegen.postProcessAllModels(allModels);
        final List<ModelMap> rootModelMaps = results.get("Root")
            .getModels();
        final List<Map<String, String>> tsImports = (List<Map<String, String>>) rootModelMaps.get(0)
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
        codegen.setOpenAPI(openAPI);
        codegen.setModelNamePrefix("Prefix");

        final HashMap<String, ModelsMap> allModels = createParameterForPostProcessAllModels(
            codegen.fromModel("Root", rootSchema),
            codegen.fromModel("Child", childSchema)
        );
        final Map<String, ModelsMap> results = codegen.postProcessAllModels(allModels);
        final List<ModelMap> rootModelMaps = results.get("Root")
            .getModels();
        final List<Map<String, String>> tsImports = (List<Map<String, String>>) rootModelMaps.get(0)
            .get("tsImports");

        Assert.assertEquals(tsImports.size(), 1);
        Assert.assertEquals(tsImports.get(0).get("filename"), "./prefixChild");
    }

    private OperationsMap createPostProcessOperationsMapWithImportName(String importName) {
        OperationMap operations = new OperationMap();
        operations.setClassname("Pet");
        operations.setOperation(new ArrayList<>());

        Map<String, String> importList = new HashMap<String, String>() {{
            put("import", importName);
            put("classname", "Pet");
        }};
        List<Map<String, String>> imports = new ArrayList<>();
        imports.add(importList);

        OperationsMap operationsMap = new OperationsMap();
        operationsMap.setImports(imports);
        operationsMap.setOperation(operations);
        return operationsMap;
    }

    private HashMap<String, ModelsMap> createParameterForPostProcessAllModels(CodegenModel root, CodegenModel child) {
        final ModelsMap rootModelsMap = new ModelsMap();
        final ModelMap rootModelMap = new ModelMap();
        rootModelMap.setModel(root);
        rootModelsMap.setModels(Collections.singletonList(rootModelMap));
        rootModelsMap.setImports(Collections.singletonList(Collections.singletonMap("import", "../model/Child")));

        final ModelsMap childModelsMap = new ModelsMap();
        final ModelMap childModelMap = new ModelMap();
        childModelMap.setModel(child);
        childModelsMap.setModels(Collections.singletonList(childModelMap));

        return new HashMap<String, ModelsMap>() {{
            put("Child", childModelsMap);
            put("Root", rootModelsMap);
        }};
    }
}
