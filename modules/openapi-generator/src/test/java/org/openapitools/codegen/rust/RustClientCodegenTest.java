/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.rust;

import io.swagger.v3.oas.models.media.IntegerSchema;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.RustClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static org.openapitools.codegen.TestUtils.linearize;

public class RustClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.PREFER_UNSIGNED_INT), Boolean.FALSE);
        Assert.assertFalse(codegen.getPreferUnsignedInt());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.BEST_FIT_INT), Boolean.FALSE);
        Assert.assertFalse(codegen.getBestFitInt());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.setPreferUnsignedInt(true);
        codegen.setBestFitInt(true);
        codegen.setAvoidBoxedModels(true);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.PREFER_UNSIGNED_INT), Boolean.TRUE);
        Assert.assertTrue(codegen.getPreferUnsignedInt());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.BEST_FIT_INT), Boolean.TRUE);
        Assert.assertTrue(codegen.getBestFitInt());

        Assert.assertEquals(codegen.additionalProperties().get(RustClientCodegen.AVOID_BOXED_MODELS), Boolean.TRUE);
        Assert.assertTrue(codegen.getAvoidBoxedModels());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testLowercaseParameterName() throws Exception {
        final RustClientCodegen codegen = new RustClientCodegen();

        Assert.assertEquals(codegen.toParamName("TESTING"), "testing");
    }

    @Test
    public void testWithIntegerDefaults() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(false);
        codegen.setPreferUnsignedInt(false);
        codegen.processOpts();

        s.setMinimum(BigDecimal.valueOf(0));
        s.setMaximum(BigDecimal.valueOf(1));

        s.setFormat("int8");
        Assert.assertEquals(codegen.getSchemaType(s), "i8");

        s.setFormat("int16");
        Assert.assertEquals(codegen.getSchemaType(s), "i16");

        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "i64");

        s.setFormat("uint8");
        Assert.assertEquals(codegen.getSchemaType(s), "u8");

        s.setFormat("uint16");
        Assert.assertEquals(codegen.getSchemaType(s), "u16");

        s.setFormat("uint32");
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        s.setFormat("uint64");
        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        // Clear format - should use default of i32
        s.setFormat(null);

        s.setMaximum(BigDecimal.valueOf(Byte.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Short.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Integer.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");
    }

    @Test
    public void testWithIntegerFitting() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.setPreferUnsignedInt(false);
        codegen.processOpts();

        // No bounds
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        // Set Bounds
        s.setMinimum(BigDecimal.valueOf(0));
        s.setMaximum(BigDecimal.valueOf(1));

        // Should respect hardcoded format
        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        // Should respect hardcoded format
        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "i64");

        // No format - use best fitting
        s.setFormat(null);

        s.setMaximum(BigDecimal.valueOf(Byte.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i8");

        s.setMaximum(BigDecimal.valueOf(Short.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i16");

        s.setMaximum(BigDecimal.valueOf(Integer.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i64");
    }

    @Test
    public void testWithPreferUnsigned() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(false);
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        // Minimum of zero, should fit in unsigned
        s.setMinimum(BigDecimal.valueOf(0));

        // No integer fitting, but prefer unsigned
        s.setMaximum(BigDecimal.valueOf(Byte.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        // Should respect hardcoded 8-bits, but prefer unsigned
        s.setFormat("int8");
        Assert.assertEquals(codegen.getSchemaType(s), "u8");

        // Should respect hardcoded 16-bits, but prefer unsigned
        s.setFormat("int16");
        Assert.assertEquals(codegen.getSchemaType(s), "u16");

        // Should respect hardcoded 32-bits, but prefer unsigned
        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        // Should respect hardcoded 64-bits, but prefer unsigned
        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        // Unknown minimum - should not use unsigned
        s.setMinimum(null);

        s.setFormat(null);
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "i64");
    }

    @Test
    public void testWithIntegerFittingAndPreferUnsigned() {
        final IntegerSchema s = new IntegerSchema();
        final RustClientCodegen codegen = new RustClientCodegen();
        codegen.setBestFitInt(true);
        codegen.setPreferUnsignedInt(true);
        codegen.processOpts();

        // Minimum of zero, should fit in unsigned
        s.setMinimum(BigDecimal.valueOf(0));
        s.setMaximum(BigDecimal.valueOf(1));

        // Should respect hardcoded 32-bits, but prefer unsigned
        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        // Should respect hardcoded 64-bits, but prefer unsigned
        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        // No format - use best fitting
        s.setFormat(null);

        s.setMaximum(BigDecimal.valueOf(Byte.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u8");

        s.setMaximum(BigDecimal.valueOf(Short.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u16");

        s.setMaximum(BigDecimal.valueOf(Integer.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u32");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "u64");

        // Unknown minimum - unable to use unsigned
        s.setMinimum(null);

        s.setMaximum(BigDecimal.valueOf(Integer.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setMaximum(BigDecimal.valueOf(Long.MAX_VALUE));
        Assert.assertEquals(codegen.getSchemaType(s), "i64");

        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "i64");
    }

    @Test
    public void testMultipleArrayTypesEnum() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setInputSpec("src/test/resources/3_1/issue_18527.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path outputPath = Path.of(target.toString(), "/src/models/option1_or_option2_options.rs");
        String enumSpec = linearize("pub enum Option1OrOption2Options { " +
                "ArrayVecString(Vec<String>), " +
                "ArrayVeci32(Vec<i32>)," +
                "}");
        TestUtils.assertFileExists(outputPath);
        TestUtils.assertFileContains(outputPath, enumSpec);
    }

    @Test
    public void testIntegerPropertyEnum() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setInputSpec("src/test/resources/3_1/rust_integer_property_enum.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "/src/models/signed_message_signature.rs");
        TestUtils.assertFileExists(outputPath);
        // The integer-enum property must use serde_repr (not string rename)
        TestUtils.assertFileContains(outputPath, "use serde_repr::{Serialize_repr,Deserialize_repr}");
        TestUtils.assertFileContains(outputPath, "Serialize_repr, Deserialize_repr");
        TestUtils.assertFileContains(outputPath, "= 0");
        TestUtils.assertFileContains(outputPath, "= 1");
        // Must NOT contain a string rename for an integer variant
        TestUtils.assertFileNotContains(outputPath, linearize("#[serde(rename = \"0\")]"));
    }

    @Test
    public void testArrayWithObjectEnumValues() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setInputSpec("src/test/resources/3_1/issue_23278.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "/src/models/object_arrays_options.rs");
        String enumSpec = linearize("pub enum ObjectArraysOptions { " +
                "ArrayVecTestObject(Vec<models::TestObject>), " +
                "ArrayVecTestArray(Vec<models::TestArray>)," +
                "}");
        TestUtils.assertFileExists(outputPath);
        TestUtils.assertFileContains(outputPath, enumSpec);
    }

    @Test
    public void testReqwestTraitUuidParamsUseNamedLifetimes() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("reqwest-trait")
                .addAdditionalProperty("mockall", true)
                .setInputSpec("src/test/resources/3_0/rust/reqwest-trait-uuid-params.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "/src/apis/widget_api.rs");
        TestUtils.assertFileExists(outputPath);
        // mockall's #[automock] cannot elide the lifetime of a reference nested in Option<..>
        TestUtils.assertFileContains(outputPath,
                "async fn list_widget_items<'id, 'run_id>(&self, id: &'id str, run_id: Option<&'run_id str>)");
        TestUtils.assertFileNotContains(outputPath, "Option<&str>");
    }

    @Test
    public void testReqwestOpenApi32OperationsAndQueryStringParam() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("reqwest")
                .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "src/apis/default_api.rs");
        TestUtils.assertFileExists(outputPath);
        String generated = new String(Files.readAllBytes(outputPath), StandardCharsets.UTF_8);
        // standard methods still use the typed constants
        Assert.assertTrue(generated.contains("reqwest::Method::GET"),
                "list_pets should use reqwest::Method::GET");
        // query and additionalOperations methods are emitted verbatim via Method::from_bytes
        for (String method : new String[]{"QUERY", "PURGE", "customMethod", "CHECK&FETCH"}) {
            Assert.assertTrue(generated.contains("reqwest::Method::from_bytes(b\"" + method + "\")"),
                    "expected verbatim method literal for " + method);
        }
        // `in: querystring` is appended to the URI verbatim, not sent through .query()
        Assert.assertTrue(generated.contains("uri_str.push_str(&p_qs);"),
                "querystring param should be appended verbatim");
        Assert.assertTrue(generated.contains("qs: &str"),
                "query_pets signature must expose the querystring parameter");
        Assert.assertFalse(generated.contains("\"qs\""),
                "querystring param must not be serialized as a name=value query pair");
    }

    @Test
    public void testReqwestOpenApi32QueryStringParamGrouped() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("reqwest")
                .addAdditionalProperty("useSingleRequestParameter", true)
                .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "src/apis/default_api.rs");
        TestUtils.assertFileExists(outputPath);
        // grouped params struct stores String fields; the borrow is required to compile
        TestUtils.assertFileContains(outputPath, "uri_str.push_str(&params.qs);");
    }

    @Test
    public void testReqwestSkipsInvalidMethodNames() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("reqwest")
                .setInputSpec("src/test/resources/3_2/rust-invalid-method.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "src/apis/default_api.rs");
        TestUtils.assertFileExists(outputPath);
        String generated = new String(Files.readAllBytes(outputPath), StandardCharsets.UTF_8);
        Assert.assertTrue(generated.contains("fn list_pets"), "GET operation should be kept");
        Assert.assertFalse(generated.contains("bad_method"),
                "operation with an invalid RFC 9110 method name must be skipped");
    }

    @Test
    public void testReqwestWebhookOpenApi32Operations() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("reqwest")
                .setInputSpec("src/test/resources/3_2/go-webhook-operations.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path apiDir = Path.of(target.toString(), "src/apis");
        File webhookFile = null;
        for (File f : Objects.requireNonNull(apiDir.toFile().listFiles())) {
            String content = new String(Files.readAllBytes(f.toPath()), StandardCharsets.UTF_8);
            if (content.contains("on_pet_custom")) {
                webhookFile = f;
                break;
            }
        }
        Assert.assertNotNull(webhookFile, "expected a generated file containing webhook ops");
        String generated = new String(Files.readAllBytes(webhookFile.toPath()), StandardCharsets.UTF_8);
        Assert.assertTrue(generated.contains("reqwest::Method::from_bytes(b\"QUERY\")"),
                "webhook query op should emit a verbatim method");
        Assert.assertTrue(generated.contains("reqwest::Method::from_bytes(b\"customMethod\")"),
                "webhook additionalOperations should emit verbatim methods");
        Assert.assertTrue(generated.contains("uri_str.push_str(&p_qs);"),
                "webhook querystring param should be appended verbatim");
    }

    @Test
    public void testHyperSkipsOpenApi32Operations() throws IOException {
        Path target = Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setLibrary("hyper")
                .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        Path outputPath = Path.of(target.toString(), "src/apis/default_api.rs");
        TestUtils.assertFileExists(outputPath);
        String generated = new String(Files.readAllBytes(outputPath), StandardCharsets.UTF_8);
        // hyper templates cannot express arbitrary method names -> warned and skipped
        Assert.assertTrue(generated.contains("list_pets"), "GET operation should be kept");
        for (String op : new String[]{"query_pets", "purge_pets", "custom_pets", "check_fetch_pets"}) {
            Assert.assertFalse(generated.contains("fn " + op),
                    "hyper library must skip unsupported 3.2 operation " + op);
        }
    }

    /**
     * End-to-end check: builds the generated reqwest (blocking) client with cargo and
     * verifies on a raw TCP listener that QUERY/additionalOperations methods and
     * `in: querystring` reach the wire verbatim. Skipped when cargo is unavailable.
     */
    @Test
    public void testReqwestGeneratedClientSendsVerbatimMethods() throws IOException, InterruptedException {
        if (!isCommandAvailable("cargo")) {
            throw new org.testng.SkipException("cargo is not on PATH; skipping generated-client verification");
        }
        if (!isCratesIoReachable() && !System.getenv().containsKey("CARGO_NET_OFFLINE")) {
            // a warm CARGO_HOME registry cache may still make the offline build work,
            // but without one the build would stall on dependency resolution
            throw new org.testng.SkipException("crates.io is unreachable; skipping generated-client verification");
        }
        Path target = Files.createTempDirectory("rust32-verify");
        try {
            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("rust")
                    .setLibrary("reqwest")
                    .addAdditionalProperty("reqwestDefaultFeatures", "rustls")
                    .addAdditionalProperty("supportAsync", false)
                    .setInputSpec("src/test/resources/3_2/query-operation.yaml")
                    .setSkipOverwrite(false)
                    .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
            new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

            Path binDir = target.resolve("src/bin");
            Files.createDirectories(binDir);
            Files.copy(Path.of("src/test/resources/3_2/rust-reqwest-capture/capture.rs"),
                    binDir.resolve("capture.rs"));

            runCargo(target, "build");
            String out = runCargo(target, "run", "--bin", "capture");
            Assert.assertTrue(out.contains("CAPTURE-PASS"),
                    "generated client did not send verbatim 3.2 methods/querystring:\n" + out);
        } finally {
            // cargo's target dir is hundreds of MB; deleteOnExit cannot remove non-empty dirs
            FileUtils.deleteDirectory(target.toFile());
        }
    }

    private boolean isCommandAvailable(String command) {
        try {
            Process p = new ProcessBuilder(command, "--version")
                    .redirectErrorStream(true).start();
            // wait before draining: a child that never exits would otherwise block
            // the stream read forever
            if (!p.waitFor(10, TimeUnit.SECONDS)) {
                p.destroyForcibly();
                return false;
            }
            p.getInputStream().transferTo(java.io.OutputStream.nullOutputStream());
            return p.exitValue() == 0;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }

    private boolean isCratesIoReachable() {
        try (java.net.Socket socket = new java.net.Socket()) {
            socket.connect(new java.net.InetSocketAddress("index.crates.io", 443), 5000);
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    private String runCargo(Path projectDir, String... args) throws IOException, InterruptedException {
        List<String> cmd = new ArrayList<>();
        cmd.add("cargo");
        cmd.addAll(Arrays.asList(args));
        ProcessBuilder pb = new ProcessBuilder(cmd)
                .directory(projectDir.toFile())
                .redirectErrorStream(true);
        // keep the cargo target dir inside the temp project
        pb.environment().put("CARGO_TARGET_DIR",
                projectDir.resolve("target").toAbsolutePath().toString());
        // the capture listener is local; never let proxy env vars intercept it
        pb.environment().put("NO_PROXY", "127.0.0.1,localhost");
        pb.environment().put("no_proxy", "127.0.0.1,localhost");
        Process p = pb.start();
        boolean finished = p.waitFor(15, TimeUnit.MINUTES);
        if (!finished) {
            p.destroyForcibly();
        }
        String output = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        Assert.assertTrue(finished, "cargo " + String.join(" ", args) + " timed out:\n" + output);
        Assert.assertEquals(p.exitValue(), 0, "cargo " + String.join(" ", args) + " failed:\n" + output);
        return output;
    }
}
