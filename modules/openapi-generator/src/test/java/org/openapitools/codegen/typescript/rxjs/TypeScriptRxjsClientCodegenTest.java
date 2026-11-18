package org.openapitools.codegen.typescript.rxjs;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

@Test(groups = {TypeScriptGroups.TYPESCRIPT})
public class TypeScriptRxjsClientCodegenTest {

    @Test(description = "Issue #22113 - an npm package must not fall back to .gitignore, which excludes dist")
    public void testNpmPackageHasNpmignore() throws Exception {
        final File output = Files.createTempDirectory("typescript_rxjs_npmignore_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-rxjs")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .addAdditionalProperty("npmName", "@openapitools/typescript-rxjs-petstore")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        TestUtils.assertFileExists(Paths.get(output + "/package.json"));
        TestUtils.assertFileExists(Paths.get(output + "/.npmignore"));
    }
}
