package org.openapitools.codegen.typescript;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.TypeScriptClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Test(groups = {TypeScriptGroups.TYPESCRIPT})
public class TypeScriptClientCodegenTest {
    @Test
    public void getTypeDeclarationTest() {
        Schema<?> childSchema = new ArraySchema().items(new StringSchema());

        OpenAPI api = TestUtils.createOpenAPI();
        api.getComponents().addSchemas("Child", childSchema);

        TypeScriptClientCodegen codegen = new TypeScriptClientCodegen();
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

    @Test
    public void testComposedSchemasImportTypesIndividually() {
        final TypeScriptClientCodegen codegen = new TypeScriptClientCodegen();
        final OpenAPI openApi = TestUtils.parseFlattenSpec("src/test/resources/3_0/composed-schemas.yaml");
        codegen.setOpenAPI(openApi);
        PathItem path = openApi.getPaths().get("/pets");
        CodegenOperation operation = codegen.fromOperation("/pets", "patch", path.getPatch(), path.getServers());
        // TODO revise the commented test below as oneOf is no longer defined inline
        //but instead defined using $ref with the new inline model resolver in 6.x
        //Assert.assertEquals(operation.imports, Sets.newHashSet("Cat", "Dog"));
        Assert.assertEquals(operation.imports, Sets.newHashSet("PetsPatchRequest"));

    }

    @Test
    public void testArrayWithUniqueItems() {
        final Schema uniqueArray = new ArraySchema()
                .items(new StringSchema())
                .uniqueItems(true);
        final Schema model = new ObjectSchema()
                .description("an object has an array with uniqueItems")
                .addProperty("uniqueArray", uniqueArray)
                .addRequiredItem("uniqueArray");

        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        final OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);

        final CodegenModel codegenModel = codegen.fromModel("sample", model);

        Assert.assertFalse(codegenModel.imports.contains("Set"));
    }

    @Test
    public void testWithAdditionalProperties() {
        final Schema inner = new ObjectSchema();
        inner.setAdditionalProperties(true);

        final Schema root = new ObjectSchema()
                .addProperty("inner", inner);

        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        final OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", root);
        codegen.setOpenAPI(openAPI);

        try {
            // TypeScriptClientCodegen can generate codes without throwing exception.
            codegen.fromModel("sample", root);
        } catch (Exception e) {
            Assert.fail("Exception was thrown.");
        }
    }

    @Test
    public void defaultModelImportTest() {
        final DefaultCodegen codegen = new TypeScriptClientCodegen();

        final CodegenModel cm = new CodegenModel();
        cm.setImports(Collections.singleton("ApiResponse"));
        final ModelsMap models = new ModelsMap();
        final ModelMap model = new ModelMap();
        model.setModel(cm);
        models.setModels(Collections.singletonList(model));

        final ModelsMap processedModels = codegen.postProcessModels(models);
        final List<Map<String, String>> tsImports = (List<Map<String, String>>) processedModels.getModels().get(0).get("tsImports");
        Assert.assertEquals(tsImports.get(0).get("filename"), "../models/ApiResponse");
        Assert.assertEquals(tsImports.get(0).get("classname"), "ApiResponse");
    }

    @Test
    public void modelImportWithMappingTest() {
        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        final String mappedName = "@namespace/dir/response";
        codegen.importMapping().put("ApiResponse", mappedName);

        final CodegenModel cm = new CodegenModel();
        cm.setImports(Collections.singleton("ApiResponse"));
        final ModelsMap models = new ModelsMap();
        final ModelMap model = new ModelMap();
        model.setModel(cm);
        models.setModels(Collections.singletonList(model));

        final ModelsMap processedModels = codegen.postProcessModels(models);
        final List<Map<String, String>> tsImports = (List<Map<String, String>>) processedModels.getModels().get(0).get("tsImports");
        Assert.assertEquals(tsImports.get(0).get("filename"), mappedName);
        Assert.assertEquals(tsImports.get(0).get("classname"), "ApiResponse");
    }

    @Test
    public void testCompilePattern() {
        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        final StringSchema prop = new StringSchema();
        prop.setPattern("[A-Z]{3}");
        final Schema root = new ObjectSchema().addProperty("stringPattern", prop);
        final OpenAPI openApi = TestUtils.createOpenAPIWithOneSchema("sample", root);
        codegen.setOpenAPI(openApi);

        try {
            final CodegenModel model = codegen.fromModel("sample", root);
            Assert.assertEquals(model.getAllVars().get(0).getPattern(), "/[A-Z]{3}/");
        } catch (Exception ex) {
            Assert.fail("Exception was thrown.");
        }
    }

    @Test
    public void arrayItemsCanBeNullable() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/array-nullable-items.yaml");
        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        codegen.setOpenAPI(openAPI);
        final ArraySchema schema = (ArraySchema) openAPI.getComponents().getSchemas().get("ArrayWithNullableItemsModel")
                .getProperties()
                .get("foo");
        Assert.assertEquals(codegen.getTypeDeclaration(schema), "Array<string | null>");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        String licenseName = "Apache 2.0";

        TypeScriptClientCodegen codegen = new TypeScriptClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.LICENSE_NAME, licenseName);
        codegen.processOpts();

        OpenAPI openAPI = TestUtils.createOpenAPI();
        codegen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(codegen.getLicenseName(), licenseName);
    }

    @Test
    public void testForAllSanitizedEnum() throws Exception {
        final File output = Files.createTempDirectory("typescriptnodeclient_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript")
                .setInputSpec("src/test/resources/bugs/typescript_enum_var_name_all_sanitized.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(
                Paths.get(output + "/models/Greeting.ts"),
                "export enum Greeting {\n" +
                        "    _こんにちは = 'こんにちは',\n" +
                        "    _你好 = '你好',\n" +
                        "    _안녕하세요 = '안녕하세요',\n" +
                        "    STRING = '!@#%',\n" +
                        "    STRING2 = '^&*\uD83C\uDF63'",
                "}"
        );
    }
}
