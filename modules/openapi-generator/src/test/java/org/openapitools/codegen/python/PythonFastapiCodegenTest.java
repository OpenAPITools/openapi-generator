package org.openapitools.codegen.python;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

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
    public void additionalPropertiesModelTest() {
        final Schema model = new ArraySchema()
                //.description()
                .items(new Schema().type("object").additionalProperties(new Schema().type("string")))
                .description("model with additionalProperties");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "model with additionalProperties");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, "null<Dict>");
        Assert.assertEquals(cm.imports.size(), 0);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet()).size(), 0);
    }
}
