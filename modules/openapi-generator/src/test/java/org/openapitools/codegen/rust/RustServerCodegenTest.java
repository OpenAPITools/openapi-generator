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
}
