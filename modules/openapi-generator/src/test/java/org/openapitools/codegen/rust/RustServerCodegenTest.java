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
import java.util.regex.Pattern;

/**
 * Tests for RustServerCodegen.
 */
public class RustServerCodegenTest {

    /**
     * Test that integer parameters with minimum/maximum constraints are assigned appropriate Rust types.
     * This tests that integer parameter type fitting logic is applied to CodegenParameter objects.
     */
    @Test
    public void testIntegerParameterTypeFitting() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-server")
                .setInputSpec("src/test/resources/3_0/rust-server/integer-params.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path libPath = Path.of(target.toString(), "/src/lib.rs");
        TestUtils.assertFileExists(libPath);

        // Verify that parameters with known min/max ranges get appropriate types
        // age: 0-150 should fit in u8
        TestUtils.assertFileContains(libPath, "age: u8");

        // temperature: -50 to 50 should fit in i8
        TestUtils.assertFileContains(libPath, "temperature: i8");

        // count: 0-65535 should fit in u16
        TestUtils.assertFileContains(libPath, "count: u16");

        // offset: -32768 to 32767 should fit in i16
        TestUtils.assertFileContains(libPath, "offset: i16");

        // large_unsigned: 0-4294967295 should be u32
        TestUtils.assertFileContains(libPath, "large_unsigned: u32");

        // Verify integer with int32 format and minimum >= 0 becomes u32
        TestUtils.assertFileContains(libPath, "positive_int32: u32");

        // Verify integer with int64 format and minimum >= 0 becomes u64
        TestUtils.assertFileContains(libPath, "positive_int64: u64");

        // Clean up
        target.toFile().deleteOnExit();
    }

    /**
     * Test that two operations whose operationIds normalize to the same snake_case string
     * (e.g., "fooBar" and "foo_bar" both become "foo_bar") receive distinct x-operation-id
     * values. All operations are emitted as handle_<x-operation-id>() free functions in a
     * single mod.rs, so duplicates would cause a Rust compile error.
     */
    @Test
    public void testDuplicateOperationIdDeduplication() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-server")
                .setInputSpec("src/test/resources/3_0/rust-server/duplicate-operation-id.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path serverModPath = Path.of(target.toString(), "/src/server/mod.rs");
        TestUtils.assertFileExists(serverModPath);

        // Both operations produce snake_case "foo_bar". The second should be renamed to
        // "foo_bar_0" so there are two distinct handle_*() functions, not a duplicate.
        TestUtils.assertFileContains(serverModPath, "handle_foo_bar(");
        TestUtils.assertFileContains(serverModPath, "handle_foo_bar_0(");

        // Clean up
        target.toFile().deleteOnExit();
    }

    /**
     * Test that required query params without examples disable the client example.
     */
    @Test
    public void testRequiredQueryParamWithoutExampleDisablesClientExample() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-server")
                .setInputSpec("src/test/resources/3_0/rust-server/openapi-v3.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path exampleClientMain = Path.of(target.toString(), "/examples/client/main.rs");
        TestUtils.assertFileExists(exampleClientMain);
        TestUtils.assertFileContains(exampleClientMain, "Disabled because there's no example.");
        TestUtils.assertFileContains(exampleClientMain, "Some(\"QueryExampleGet\")");

        // Clean up
        target.toFile().deleteOnExit();
    }

    /**
     * Test that the /multiple-response-content-types operation in openapi-v3.yaml still
     * produces the classic x-produces-json vendor extension for its single-content-type
     * 201 response (no OneOf wrapper), and produces a swagger::OneOf2 return type with
     * Content-Type dispatch code for its two-content-type 403 response, as required by
     * issue #24097.
     */
    @Test
    public void testMultipleResponseContentTypes() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-server")
                .setInputSpec("src/test/resources/3_0/rust-server/openapi-v3.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path clientModPath = Path.of(target.toString(), "/src/client/mod.rs");
        TestUtils.assertFileExists(clientModPath);

        // Single-type 201 response must deserialize directly into the model, with no
        // OneOf wrapper, i.e. the classic x-produces-json behavior.
        TestUtils.assertFileContains(clientModPath,
                "let body = serde_json::from_str::<models::AnyOfObject>(body)\n" +
                "                    .map_err(|e| ApiError(format!(\"Response body did not match the schema: {e}\")))?;\n\n" +
                "                Ok(MultipleResponseContentTypesResponse::Created\n" +
                "                    (body)\n" +
                "                )");

        // Two-content-type 403 response must use a OneOf2 return type.
        TestUtils.assertFileContains(clientModPath, "swagger::OneOf2::<");
        // Both variant content types must appear as case-insensitive match arms in the dispatch
        // code (HTTP media types are case-insensitive per RFC 7231).
        TestUtils.assertFileContains(clientModPath, "ct.eq_ignore_ascii_case(\"text/plain\")");
        TestUtils.assertFileContains(clientModPath, "ct.eq_ignore_ascii_case(\"application/json\")");
        // Content-Type header must be read for dispatch.
        TestUtils.assertFileContains(clientModPath, "CONTENT_TYPE");

        target.toFile().deleteOnExit();
    }

    /**
     * Test that binary/byte request bodies are passed through as raw bytes rather than being
     * coerced to UTF-8, which panics on any non-UTF-8 payload (see issue #24094).
     */
    @Test
    public void testBinaryRequestBodyNotCoercedToUtf8() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-server")
                .setInputSpec("src/test/resources/3_0/rust-server/openapi-v3.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path clientModPath = Path.of(target.toString(), "/src/client/mod.rs");
        TestUtils.assertFileExists(clientModPath);

        // format: binary request body should pass raw bytes through.
        TestUtils.assertFileContains(clientModPath, "*request.body_mut() = body_from_bytes(param_body.0);");
        // The bytes helper must be generated.
        TestUtils.assertFileContains(clientModPath, "fn body_from_bytes(b: Vec<u8>) -> BoxBody<Bytes, Infallible> {");
        // The request body must no longer be coerced to UTF-8 (would panic on non-UTF-8 payloads).
        TestUtils.assertFileNotContains(clientModPath, "let body = String::from_utf8(param_body.0).expect(\"Body was not valid UTF8\");");

        // Clean up
        target.toFile().deleteOnExit();
    }
}
