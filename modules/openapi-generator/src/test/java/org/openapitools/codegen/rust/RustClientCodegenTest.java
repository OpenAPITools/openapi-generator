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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

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

        s.setFormat("int32");
        Assert.assertEquals(codegen.getSchemaType(s), "i32");

        s.setFormat("int64");
        Assert.assertEquals(codegen.getSchemaType(s), "i64");

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
    public void testAnyOfSupport() throws IOException {
        Path target = Files.createTempDirectory("test-anyof");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust")
                .setInputSpec("src/test/resources/3_0/rust/rust-anyof-test.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        
        // Test that ModelIdentifier generates an untagged enum, not an empty struct
        Path modelIdentifierPath = Path.of(target.toString(), "/src/models/model_identifier.rs");
        TestUtils.assertFileExists(modelIdentifierPath);
        
        // Should generate an untagged enum
        TestUtils.assertFileContains(modelIdentifierPath, "#[serde(untagged)]");
        TestUtils.assertFileContains(modelIdentifierPath, "pub enum ModelIdentifier");
        
        // Should have String variant (for anyOf with string types)
        TestUtils.assertFileContains(modelIdentifierPath, "String(String)");
        
        // Should NOT generate an empty struct
        TestUtils.assertFileNotContains(modelIdentifierPath, "pub struct ModelIdentifier {");
        TestUtils.assertFileNotContains(modelIdentifierPath, "pub fn new()");
        
        // Test AnotherAnyOfTest with mixed types
        Path anotherTestPath = Path.of(target.toString(), "/src/models/another_any_of_test.rs");
        TestUtils.assertFileExists(anotherTestPath);
        TestUtils.assertFileContains(anotherTestPath, "#[serde(untagged)]");
        TestUtils.assertFileContains(anotherTestPath, "pub enum AnotherAnyOfTest");
    }
}
