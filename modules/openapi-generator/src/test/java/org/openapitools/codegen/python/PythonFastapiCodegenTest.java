package org.openapitools.codegen.python;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
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
}
