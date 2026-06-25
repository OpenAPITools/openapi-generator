package org.openapitools.codegen.typescript.inversify;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_INVERSIFY})
public class TypeScriptInversifyClientCodegenTest {

    @Test(description = "Verify multipart file arrays use repeated form fields")
    public void testMultipartFileArrayUsesRepeatedFormFields() throws Exception {
        final File output = Files.createTempDirectory("typescript_inversify_multipart_file_array_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("typescript-inversify")
                .setInputSpec("src/test/resources/3_0/form-multipart-binary-array.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        final DefaultGenerator generator = new DefaultGenerator();
        final List<File> files = generator.opts(clientOptInput).generate();
        files.forEach(File::deleteOnExit);

        Path api = Paths.get(output + "/api/multipart.service.ts");
        TestUtils.assertFileExists(api);
        TestUtils.assertFileContains(api, "files?: Array<Blob>");
        TestUtils.assertFileContains(api, "files.forEach((element) => {");
        TestUtils.assertFileContains(api, "formData.append('files', <any>element);");
        TestUtils.assertFileNotContains(api, "files.join(COLLECTION_FORMATS['csv'])");
    }
}
