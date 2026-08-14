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

package org.openapitools.codegen.dart;

import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.DartClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class DartClientCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final DartClientCodegen codegen = new DartClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertTrue(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final DartClientCodegen codegen = new DartClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final DartClientCodegen codegen = new DartClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertFalse(codegen.isHideGenerationTimestamp());
    }

    @Test
    public void testKeywords() throws Exception {
        final DartClientCodegen codegen = new DartClientCodegen();

        List<String> reservedWordsList = new ArrayList<String>();
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


    @Test(description = "Enum value with quotes (#17582)")
    public void testEnumPropertyWithQuotes() {
        final DartClientCodegen codegen = new DartClientCodegen();

        Assert.assertEquals(codegen.toEnumValue("enum-value", "string"), "'enum-value'");
        Assert.assertEquals(codegen.toEnumValue("won't fix", "string"), "'won\\'t fix'");
        Assert.assertEquals(codegen.toEnumValue("\"", "string"), "'\\\"'");
        Assert.assertEquals(codegen.toEnumValue("1.0", "number"), "1.0");
        Assert.assertEquals(codegen.toEnumValue("1", "int"), "1");
    }

    private List<File> generateDartNativeFromSpec(String specPath) throws Exception {
        return generateDartNativeFromSpec(specPath, Map.of());
    }

    private List<File> generateDartNativeFromSpec(String specPath, Map<String, Object> additionalProperties) throws Exception {
        File output = Files.createTempDirectory("dart-native-test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("dart")
                .setInputSpec(specPath)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        additionalProperties.forEach(configurator::addAdditionalProperty);

        ClientOptInput opts = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(opts).generate();
        files.forEach(File::deleteOnExit);
        return files;
    }

    @Test(description = "Object-typed arrays should not produce Object.listFromJson()")
    public void testObjectArrayDoesNotUseListFromJson() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("object_array_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("object_array_model.dart not found in generated files"));

        // Object is a primitive type and has no listFromJson — should use cast<Object>() instead
        TestUtils.assertFileNotContains(modelFile.toPath(), "Object.listFromJson");
        TestUtils.assertFileContains(modelFile.toPath(), "cast<Object>");
    }

    @Test(description = "Enum properties with defaults should emit enum constructor, not string literal")
    public void testEnumDefaultUsesEnumConstructor() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("enum_default_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("enum_default_model.dart not found in generated files"));

        // Default should use const EnumType._('value'), not a bare string literal
        TestUtils.assertFileNotContains(modelFile.toPath(), "?? 'active'");
        TestUtils.assertFileContains(modelFile.toPath(),
                "?? const EnumDefaultModelStatusEnum._('active')");
    }

    @Test(description = "Required+nullable fields should not assert non-null")
    public void testRequiredNullableFieldsDoNotAssertNonNull() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("nullable_required_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("nullable_required_model.dart not found in generated files"));

        // Required key 'name' (non-nullable) should assert both presence and non-null
        TestUtils.assertFileContains(modelFile.toPath(),
                "json.containsKey(r'name')");
        TestUtils.assertFileContains(modelFile.toPath(),
                "json[r'name'] != null");

        // Required key 'nickname' (nullable) should assert presence but NOT non-null
        TestUtils.assertFileContains(modelFile.toPath(),
                "json.containsKey(r'nickname')");
        TestUtils.assertFileNotContains(modelFile.toPath(),
                "json[r'nickname'] != null");
    }

    @Test(description = "Optional numeric fields should guard against null values before parsing")
    public void testOptionalNumericFieldsGuardAgainstNull() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("optional_number_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("optional_number_model.dart not found in generated files"));

        TestUtils.assertFileContains(modelFile.toPath(),
                "problem: json[r'problem'] == null");
        TestUtils.assertFileContains(modelFile.toPath(),
                ": num.parse('${json[r'problem']}')");
    }

    @Test(description = "Nullable nested arrays of complex types should preserve null entries")
    public void testNullableNestedComplexArraysPreserveNullEntries() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("complex_nested_array_nullable_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("complex_nested_array_nullable_model.dart not found in generated files"));

        TestUtils.assertFileContains(modelFile.toPath(),
                "e == null ? null : NullableRequiredModel.listFromJson(e)");
    }

    // DateFormat('yyyy-MM-dd') does no timezone conversion, it just formats the y/m/d the
    // DateTime already carries. So toUtc() only shifts the value across a day boundary and
    // the date comes out off by one - back a day east of UTC, forward a day west of it.
    @Test(description = "format: date must not be converted to UTC before formatting")
    public void testDateOnlyFieldsAreNotConvertedToUtc() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml");

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("date_only_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("date_only_model.dart not found in generated files"));

        TestUtils.assertFileContains(modelFile.toPath(),
                "json[r'requiredDate'] = _dateFormatter.format(this.requiredDate);");
        TestUtils.assertFileContains(modelFile.toPath(),
                "json[r'optionalDate'] = _dateFormatter.format(this.optionalDate!);");
        // A pattern routes it through the _isEpochMarker ternary, non-epoch side formats the same way
        TestUtils.assertFileContains(modelFile.toPath(),
                ": _dateFormatter.format(this.patternedDate!);");
    }

    @Test(description = "format: date must not be converted to UTC in the Optional<T> path either")
    public void testDateOnlyFieldsAreNotConvertedToUtcWithUseOptional() throws Exception {
        List<File> files = generateDartNativeFromSpec(
                "src/test/resources/3_0/dart/dart-native-deserialization-bugs.yaml",
                Map.of("useOptional", true));

        File modelFile = files.stream()
                .filter(f -> f.getName().equals("date_only_model.dart"))
                .findFirst()
                .orElseThrow(() -> new AssertionError("date_only_model.dart not found in generated files"));

        TestUtils.assertFileContains(modelFile.toPath(),
                "json[r'optionalDate'] = value == null ? null : _dateFormatter.format(value);");
        TestUtils.assertFileContains(modelFile.toPath(),
                ": _dateFormatter.format(value));");
    }
}
