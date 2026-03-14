package org.openapitools.codegen.rust;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.openapitools.codegen.TestUtils.linearize;

public class RustAxumServerCodegenTest {
    @Test
    public void testObjectStructFieldIsPublic() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path typesPath = Path.of(target.toString(), "/src/types.rs");
        TestUtils.assertFileExists(typesPath);
        TestUtils.assertFileContains(typesPath, "pub struct Object(pub serde_json::Value);");
    }

    @Test
    public void testPreventDuplicateOperationDeclaration() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/issue_21144.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path outputPath = Path.of(target.toString(), "/src/server/mod.rs");
        String routerSpec = linearize("Router::new() " +
                ".route(\"/api/test\", " +
                "delete(test_delete::<I, A, E, C>).post(test_post::<I, A, E, C>) ) " +
                ".route(\"/api/test/{test_id}\", " +
                "get(test_get::<I, A, E, C>) ) " +
                ".with_state(api_impl)");
        TestUtils.assertFileExists(outputPath);
        TestUtils.assertFileContains(outputPath, routerSpec);
    }
}