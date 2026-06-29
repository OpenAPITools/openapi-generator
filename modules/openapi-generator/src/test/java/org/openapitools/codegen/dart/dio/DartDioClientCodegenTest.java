/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.dart.dio;

import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.DartDioClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class DartDioClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testInitialFeatures() {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.processOpts();

        Assert.assertNotNull(codegen.getFeatureSet().getSecurityFeatures());
        Assert.assertFalse(codegen.getFeatureSet().getSecurityFeatures().isEmpty());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testKeywords() {
        final DartDioClientCodegen codegen = new DartDioClientCodegen();

        List<String> reservedWordsList = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("src/main/resources/dart/dart-keywords.txt"), StandardCharsets.UTF_8));
            while (reader.ready()) {
                reservedWordsList.add(reader.readLine());
            }
            reader.close();
        } catch (Exception e) {
            String errorString = String.format(Locale.ROOT, "Error reading dart keywords: %s", e);
            Assert.fail(errorString, e);
        }

        Assert.assertTrue(reservedWordsList.size() > 20);
        Assert.assertEquals(codegen.reservedWords().size(), reservedWordsList.size());
        for (String keyword : reservedWordsList) {
            // reserved words are stored in lowercase
            Assert.assertTrue(codegen.reservedWords().contains(keyword.toLowerCase(Locale.ROOT)), String.format(Locale.ROOT, "%s, part of %s, was not found in %s", keyword, reservedWordsList, codegen.reservedWords().toString()));
        }
    }

    @Test
    public void testImportMappingsInSerializersAndBarrelFile() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final String pubName = "my_api";
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("dart-dio")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setInputSpec("src/test/resources/3_0/dart-dio/import_mapping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        configurator.addAdditionalProperty("pubName", pubName);
        configurator.addTypeMapping("Address", "CustomAddress");
        configurator.addImportMapping("CustomAddress", "package:my_api/src/custom_models/custom_address.dart");

        ClientOptInput opts = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator().opts(opts);
        List<File> files = generator.generate();
        files.forEach(File::deleteOnExit);

        // The model file for the mapped type should NOT be generated
        TestUtils.assertFileNotExists(output.toPath().resolve("lib/src/model/custom_address.dart"));

        // order_out.dart should use the custom import path (this already works per the issue report)
        Path orderOutPath = output.toPath().resolve("lib/src/model/order_out.dart");
        TestUtils.assertFileContains(orderOutPath,
                "package:my_api/src/custom_models/custom_address.dart");
        TestUtils.assertFileNotContains(orderOutPath,
                "package:my_api/src/model/custom_address.dart");

        // serializers.dart should use the custom import path, not the hardcoded model/ path
        Path serializersPath = output.toPath().resolve("lib/src/serializers.dart");
        TestUtils.assertFileContains(serializersPath,
                "package:my_api/src/custom_models/custom_address.dart");
        TestUtils.assertFileNotContains(serializersPath,
                "package:my_api/src/model/custom_address.dart");

        // The barrel file should use the custom export path, not the hardcoded model/ path
        Path barrelPath = output.toPath().resolve("lib/my_api.dart");
        TestUtils.assertFileContains(barrelPath,
                "package:my_api/src/custom_models/custom_address.dart");
        TestUtils.assertFileNotContains(barrelPath,
                "package:my_api/src/model/custom_address.dart");
    }

    /**
     * Regression test for the dart-dio built_value generator's handling
     * of optional non-nullable model properties.
     *
     * Before the fix, the generator emitted a Dart getter typed as
     * {@code String?} (because in Dart, an optional field can always be
     * absent, which is observably {@code null}) but the matching
     * deserializer used {@code FullType(String)} and cast the result
     * {@code as String}. As a consequence, the moment the JSON payload
     * carried the field as an explicit {@code null} the cast threw and
     * the entire enclosing object failed to deserialize -- silently in
     * many call paths.
     *
     * The fix: in {@code deserialize_properties.mustache} the cast
     * follows the same rule that {@code class_members.mustache} already
     * uses for the getter type, i.e. nullable when
     * {@code isNullable || !required}. This test asserts that.
     */
    @Test
    public void testOptionalNonNullablePropertyDeserializesAsNullable() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("dart-dio")
                .setInputSpec("src/test/resources/3_0/dart-dio/built_value_optional_nullable.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        ClientOptInput opts = configurator.toClientOptInput();
        Generator generator = new DefaultGenerator().opts(opts);
        List<File> files = generator.generate();
        files.forEach(File::deleteOnExit);

        Path widget = output.toPath().resolve("lib/src/model/widget.dart");

        // Required fields stay strict: cast as a non-nullable type with a
        // non-nullable FullType. Otherwise the existing behaviour for
        // required fields would change.
        TestUtils.assertFileContains(widget,
                "case r'id':",
                "specifiedType: const FullType(int),",
                "as int;");
        TestUtils.assertFileContains(widget,
                "case r'name':",
                "specifiedType: const FullType(String),",
                "as String;");

        // Optional non-nullable: getter is `String?` (existing), and the
        // deserializer now matches -- FullType.nullable + cast as `T?`
        // + skip-on-null guard so we never reach `result.x = valueDes`
        // with a null.
        TestUtils.assertFileContains(widget, "String? get iconUrl;");
        TestUtils.assertFileContains(widget,
                "case r'iconUrl':",
                "specifiedType: const FullType.nullable(String),",
                "as String?;",
                "if (valueDes == null) continue;");

        TestUtils.assertFileContains(widget, "int? get priority;");
        TestUtils.assertFileContains(widget,
                "case r'priority':",
                "specifiedType: const FullType.nullable(int),",
                "as int?;",
                "if (valueDes == null) continue;");

        // Explicitly nullable still works (regression guard).
        TestUtils.assertFileContains(widget,
                "case r'explicitlyNullable':",
                "specifiedType: const FullType.nullable(String),",
                "as String?;",
                "if (valueDes == null) continue;");
    }

    @Test
    public void verifyDartDioGeneratorRuns() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("dart-dio")
                .setGitUserId("my-user")
                .setGitRepoId("my-repo")
                .setPackageName("my-package")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        ClientOptInput opts = configurator.toClientOptInput();

        Generator generator = new DefaultGenerator().opts(opts);
        List<File> files = generator.generate();
        files.forEach(File::deleteOnExit);

        TestUtils.ensureContainsFile(files, output, "README.md");
        TestUtils.ensureContainsFile(files, output, "lib/src/api.dart");
    }
}
