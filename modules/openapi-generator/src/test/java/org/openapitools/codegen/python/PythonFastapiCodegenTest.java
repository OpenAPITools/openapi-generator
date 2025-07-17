package org.openapitools.codegen.python;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PythonFastAPIServerCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

public class PythonFastapiCodegenTest {
    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final String IMPL_PKG = "impl_package";
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python-fastapi")
                .setPackageName("nodesc")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .setInputSpec("src/test/resources/3_1/nodesc.yaml")
                .addAdditionalProperty(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE, IMPL_PKG);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output.getAbsolutePath(), "/src", "/nodesc", IMPL_PKG, "__init__.py"));
    }

    @Test
    public void testEndpointSpecsWithoutDescription() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python-fastapi")
                .setPackageName("nodesc")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .setInputSpec("src/test/resources/3_1/nodesc.yaml");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileContains(Paths.get(output + "/src/nodesc/apis/nodesc_api.py"),
                "return await BaseNodescApi.subclasses[0]().nodesc()\n");
        TestUtils.assertFileContains(Paths.get(output + "/src/nodesc/apis/desc_api.py"),
                "return await BaseDescApi.subclasses[0]().desc()\n");
    }

    @Test(description = "additionalProperties should not let container type inherit their type")
    public void testAdditionalProperties() {
        Schema model = new ArraySchema()
                .items(new Schema().type("object").additionalProperties(new Schema().type("string")))
                .description("model with additionalProperties");
        DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "model with additionalProperties");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertNull(cm.parent, null);
        Assert.assertEquals(cm.imports.size(), 0);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet()).size(), 0);
    }

    @Test(description = "oneOf discriminator mapping values are propagated to vars")
    public void testOneOfDiscriminator() {
        TreeMap<String, Schema> properties1 = new TreeMap<>();
        properties1.put("objectType", new Schema().type("string"));
        TreeMap<String, Schema> properties2 = new TreeMap<>(properties1);
        properties1.put("someProp", new Schema().type("string"));
        Schema typeA = new Schema().type("object").properties(properties1);
        Schema typeB = new Schema().type("object").properties(properties2);
        Schema typeC = new ComposedSchema().oneOf(List.of(typeA, typeB))
                .discriminator(new Discriminator()
                        .propertyName("objectType")
                        .mapping("type-a", "#/components/schemas/TypeA"));
        Schema typeD = new Schema().type("object").properties(properties2);

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.setComponents(new Components());
        openAPI.getComponents().addSchemas("TypeA", typeA);
        openAPI.getComponents().addSchemas("TypeB", typeB);
        openAPI.getComponents().addSchemas("TypeC", typeC);
        openAPI.getComponents().addSchemas("TypeD", typeD);

        DefaultCodegen codegen = new PythonFastAPIServerCodegen();
        codegen.setOpenAPI(openAPI);

        TreeMap<String, ModelsMap> allModels = new TreeMap<>();
        String[] typeNames = new String[]{"TypeA", "TypeB", "TypeC", "TypeD"};
        CodegenModel[] models = new CodegenModel[]{null, null, null, null};
        for (int i = 0; i < typeNames.length; i++) {
            String key = typeNames[i];
            CodegenModel cm = codegen.fromModel(key, openAPI.getComponents().getSchemas().get(key));
            if (key.equals("TypeC")) {
                cm.oneOf = Set.of("TypeA", "TypeB");
            }
            ModelMap mo = new ModelMap();
            mo.setModel(cm);
            ModelsMap objs = new ModelsMap();
            objs.setModels(List.of(mo));
            allModels.put(key, objs);
            models[i] = cm;
        }

        codegen.postProcessAllModels(allModels);

        CodegenModel typeAModel = models[0];
        CodegenModel typeBModel = models[1];
        CodegenModel typeDModel = models[3];
        Assert.assertEquals(typeAModel.vars.size(), 2);
        Assert.assertTrue(typeAModel.vars.get(0).isDiscriminator);
        Assert.assertEquals(typeAModel.vars.get(0).discriminatorValue, "type-a"); // explicitly mapped value
        Assert.assertTrue(typeBModel.vars.get(0).isDiscriminator);
        Assert.assertEquals(typeBModel.vars.get(0).discriminatorValue, "TypeB"); // implicit value
        Assert.assertNull(typeAModel.parent);
        Assert.assertEquals(typeAModel.imports.size(), 0);
        Assert.assertEquals(Sets.intersection(typeAModel.imports, Sets.newHashSet()).size(), 0);

        Assert.assertEquals(typeDModel.vars.size(), 1);
        Assert.assertFalse(typeDModel.vars.get(0).isDiscriminator);
        Assert.assertNull(typeDModel.vars.get(0).discriminatorValue);
    }
}
