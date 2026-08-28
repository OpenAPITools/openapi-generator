package org.openapitools.codegen.rust;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.RustSalvoServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.openapitools.codegen.TestUtils.linearize;

public class RustSalvoServerCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RustSalvoServerCodegen codegen = new RustSalvoServerCodegen();
        codegen.processOpts();

        // Test default values
        Assert.assertEquals(codegen.getName(), "rust-salvo");
        Assert.assertEquals(codegen.additionalProperties().get("packageName"), "salvo_openapi");
        Assert.assertTrue(codegen.additionalProperties().containsKey("enableRequestValidation"));
        Assert.assertTrue(codegen.additionalProperties().containsKey("enableAuthMiddleware"));
        Assert.assertTrue(codegen.additionalProperties().containsKey("enableCorsMiddleware"));
    }

    @Test
    public void testBasicGeneration() throws IOException {
        Path target = Files.createTempDirectory("salvo-test");
        System.out.println("📁 生成目录: " + target.toAbsolutePath());
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-basic-test.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        System.out.println("✅ 生成了 " + files.size() + " 个文件:");
        files.stream().limit(10).forEach(f -> System.out.println("  📄 " + f.getName()));
        System.out.println("🗂️  完整目录结构:");
        printDirectoryTree(target.toFile(), "  ");
        // 注释掉删除文件的代码，这样我们可以检查生成的文件
        // files.forEach(File::deleteOnExit);

        // Verify core files are generated
        TestUtils.assertFileExists(Path.of(target.toString(), "Cargo.toml"));
        TestUtils.assertFileExists(Path.of(target.toString(), "src/lib.rs"));
        TestUtils.assertFileExists(Path.of(target.toString(), "src/main.rs"));
        TestUtils.assertFileExists(Path.of(target.toString(), "src/models.rs"));
        TestUtils.assertFileExists(Path.of(target.toString(), "src/routes.rs"));
        TestUtils.assertFileExists(Path.of(target.toString(), "src/handlers/mod.rs"));
    }

    @Test
    public void testHandlerGeneration() throws IOException {
        Path target = Files.createTempDirectory("salvo-handlers-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-basic-test.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify handler files are generated
        Path handlersModPath = Path.of(target.toString(), "src/handlers/mod.rs");
        TestUtils.assertFileExists(handlersModPath);

        // Check that the handlers module exports are present.
        // The basic test spec is tagged "pets", so the per-tag module should be `pets`.
        TestUtils.assertFileContains(handlersModPath, "pub mod pets;");
        TestUtils.assertFileContains(handlersModPath, "pub use pets::*;");
    }

    @Test
    public void testCargoTomlGeneration() throws IOException {
        Path target = Files.createTempDirectory("salvo-cargo-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-basic-test.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify Cargo.toml contains required dependencies
        Path cargoPath = Path.of(target.toString(), "Cargo.toml");
        TestUtils.assertFileExists(cargoPath);
        TestUtils.assertFileContains(cargoPath, "salvo = { version = \"0.93\"");
        TestUtils.assertFileContains(cargoPath, "serde = { version = \"1\"");
        TestUtils.assertFileContains(cargoPath, "tokio = { version = \"1\"");
        TestUtils.assertFileContains(cargoPath, "serde_json = \"1\"");
    }

    @Test
    public void testMiddlewareGeneration() throws IOException {
        Path target = Files.createTempDirectory("salvo-middleware-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-auth-test.yaml")
                .setSkipOverwrite(false)
                .addAdditionalProperty("enableAuthMiddleware", "true")
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // The auth-test spec declares ApiKey, Basic, and Bearer schemes; all three
        // structs plus their per-route factory functions should appear.
        Path middlewarePath = Path.of(target.toString(), "src/middleware.rs");
        TestUtils.assertFileExists(middlewarePath);
        TestUtils.assertFileContains(middlewarePath, "pub struct ApiKeyAuth");
        TestUtils.assertFileContains(middlewarePath, "pub struct BasicAuth");
        TestUtils.assertFileContains(middlewarePath, "pub struct BearerAuth");
        TestUtils.assertFileContains(middlewarePath, "pub fn auth_middleware()");
        TestUtils.assertFileContains(middlewarePath, "pub fn api_key_auth()");
        TestUtils.assertFileContains(middlewarePath, "pub fn basic_auth_hoop()");
        TestUtils.assertFileContains(middlewarePath, "pub fn bearer_auth()");
        // Case-insensitive Authorization scheme prefix matching.
        TestUtils.assertFileContains(middlewarePath, "eq_ignore_ascii_case");

        // Routes file should wire auth per-route (no global hoop in lib.rs).
        Path routesPath = Path.of(target.toString(), "src/routes.rs");
        TestUtils.assertFileContains(routesPath, ".hoop(api_key_auth())");
        TestUtils.assertFileContains(routesPath, ".hoop(bearer_auth())");
        TestUtils.assertFileContains(routesPath, ".hoop(basic_auth_hoop())");

        Path libPath = Path.of(target.toString(), "src/lib.rs");
        TestUtils.assertFileNotContains(libPath, ".hoop(auth_middleware())");
    }

    @Test
    public void testOAuth2ScopesPropagateToEndpointAnnotation() throws IOException {
        Path target = Files.createTempDirectory("salvo-oauth-scopes-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Petstore's `petstore_auth` OAuth2 scheme grants `write:pets` and
        // `read:pets`; the generated endpoint annotation must carry those
        // exact scopes rather than an empty list.
        Path petHandlersPath = Path.of(target.toString(), "src/handlers/pet.rs");
        TestUtils.assertFileExists(petHandlersPath);
        TestUtils.assertFileContains(petHandlersPath,
                "security((\"OAuth2\" = [\"write:pets\", \"read:pets\"]))");
    }

    @Test
    public void testSerdeRenameOnCamelCaseFields() throws IOException {
        Path target = Files.createTempDirectory("salvo-rename-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Petstore has `petId`, `shipDate`, `photoUrls` — Rust snake_cases them
        // to `pet_id` / `ship_date` / `photo_urls`; a serde rename must preserve
        // the OpenAPI wire name.
        Path modelsPath = Path.of(target.toString(), "src/models.rs");
        TestUtils.assertFileExists(modelsPath);
        TestUtils.assertFileContains(modelsPath, "#[serde(rename = \"petId\"");
        TestUtils.assertFileContains(modelsPath, "#[serde(rename = \"shipDate\"");
        TestUtils.assertFileContains(modelsPath, "#[serde(rename = \"photoUrls\"");
        // Identical names (e.g. `id`, `name`) should NOT get a redundant rename.
        TestUtils.assertFileNotContains(modelsPath, "#[serde(rename = \"id\"");
    }

    @Test
    public void testValidationSupport() throws IOException {
        Path target = Files.createTempDirectory("salvo-validation-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-validation-test.yaml")
                .setSkipOverwrite(false)
                .addAdditionalProperty("enableRequestValidation", "true")
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify validation dependencies are included when enabled
        Path cargoPath = Path.of(target.toString(), "Cargo.toml");
        TestUtils.assertFileExists(cargoPath);
        TestUtils.assertFileContains(cargoPath, "validator = { version = \"0.20\"");
    }

    @Test
    public void testRouteGeneration() throws IOException {
        Path target = Files.createTempDirectory("salvo-routes-test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-salvo")
                .setInputSpec("src/test/resources/3_0/rust/rust-salvo-basic-test.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        // Verify routes file contains expected Salvo routing code
        Path routesPath = Path.of(target.toString(), "src/routes.rs");
        TestUtils.assertFileExists(routesPath);
        TestUtils.assertFileContains(routesPath, "use salvo::prelude::*;");
        TestUtils.assertFileContains(routesPath, "pub fn create_router()");
        TestUtils.assertFileContains(routesPath, "Router::new()");
    }

    private static void printDirectoryTree(File dir, String indent) {
        if (dir.isDirectory()) {
            System.out.println(indent + "📁 " + dir.getName() + "/");
            File[] files = dir.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        printDirectoryTree(file, indent + "  ");
                    } else {
                        System.out.println(indent + "  📄 " + file.getName());
                    }
                }
            }
        }
    }
}
