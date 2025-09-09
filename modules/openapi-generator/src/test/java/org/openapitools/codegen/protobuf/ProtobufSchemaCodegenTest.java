/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.protobuf;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ProtobufSchemaCodegen;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.createCodegenModelWrapper;
import static org.openapitools.codegen.languages.ProtobufSchemaCodegen.USE_SIMPLIFIED_ENUM_NAMES;
import static org.testng.Assert.assertEquals;
import static org.openapitools.codegen.languages.ProtobufSchemaCodegen.START_ENUMS_WITH_UNSPECIFIED;

public class ProtobufSchemaCodegenTest {

    @Test
    public void testFeatureSet() {
        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        FeatureSet featureSet = codegen.getGeneratorMetadata().getFeatureSet();

        Assert.assertTrue(featureSet.getWireFormatFeatures().contains(WireFormatFeature.PROTOBUF));
        Assert.assertEquals(featureSet.getWireFormatFeatures().size(), 1);
    }

    @Test
    public void testCodeGenWithAllOf() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/allOf_composition_discriminator.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");

        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/pet.proto"));

        output.deleteOnExit();
    }

    private void assertFileEquals(Path generatedFilePath, Path expectedFilePath) throws IOException {
        String generatedFile = new String(Files.readAllBytes(generatedFilePath), StandardCharsets.UTF_8)
                .replace("\n", "").replace("\r", "");
        String expectedFile = new String(Files.readAllBytes(expectedFilePath), StandardCharsets.UTF_8)
                .replace("\n", "").replace("\r", "");

        assertEquals(generatedFile, expectedFile);
    }

    @Test
    public void testCodeGenWithPrimitiveOneOf() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/oneOf.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "models/fruit.proto");
        Path path = Paths.get(output + "/models/fruit.proto");

        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/fruitOneOf.proto"));

        output.deleteOnExit();
    }

    @Test
    public void testCodeGenWithPrimitiveAnyOf() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/anyOf.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "models/fruit.proto");
        Path path = Paths.get(output + "/models/fruit.proto");

        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/fruitAnyOf.proto"));

        output.deleteOnExit();
    }

    @Test(description = "convert a model with dollar signs")
    public void modelTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/dollar-in-names-pull14359.yaml");
        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();

        codegen.setOpenAPI(openAPI);
        final CodegenModel simpleName = codegen.fromModel("$DollarModel$", openAPI.getComponents().getSchemas().get("$DollarModel$"));
        Assert.assertEquals(simpleName.name, "$DollarModel$");
        Assert.assertEquals(simpleName.classname, "DollarModel");
        Assert.assertEquals(simpleName.classVarName, "dollar_model");
    }

    @Test(description = "support complex enum values")
    public void supportComplexEnumValues() {
        testEnumValues(false);
    }

    @Test(description = "support simple enum values")
    public void supportSimpleEnumValues() {
        testEnumValues(true);
    }

    private void testEnumValues(boolean simpleEnumValue) {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("testStringEnum", new StringSchema()._enum(Arrays.asList("foo", "bar")))
                .addProperties("testIntEnum", new IntegerSchema().addEnumItem(1).addEnumItem(2));
        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.additionalProperties().put(USE_SIMPLIFIED_ENUM_NAMES, simpleEnumValue);
        codegen.processOpts();
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "testStringEnum");
        Assert.assertEquals(property1.dataType, "string");
        Assert.assertEquals(property1.baseType, "string");
        Assert.assertEquals(property1.datatypeWithEnum, "Test_string_enum");
        Assert.assertEquals(property1.name, "test_string_enum");
        Assert.assertTrue(property1.isEnum);
        Assert.assertEquals(property1.allowableValues.size(), 2);
        Assert.assertEquals(((List<String>) property1.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars1 = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars1.size(), 2);

        Assert.assertEquals(enumVars1.get(0).get("name"), simpleEnumValue ? "FOO" : "TEST_STRING_ENUM_FOO");
        Assert.assertEquals(enumVars1.get(0).get("value"), simpleEnumValue ? "FOO" : "\"TEST_STRING_ENUM_FOO\"");
        Assert.assertEquals(enumVars1.get(0).get("isString"), false);

        Assert.assertEquals(enumVars1.get(1).get("name"), simpleEnumValue ? "BAR" : "TEST_STRING_ENUM_BAR");
        Assert.assertEquals(enumVars1.get(1).get("value"), simpleEnumValue ? "BAR" : "\"TEST_STRING_ENUM_BAR\"");
        Assert.assertEquals(enumVars1.get(1).get("isString"), false);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "testIntEnum");
        Assert.assertEquals(property2.dataType, "int32");
        Assert.assertEquals(property2.baseType, "int32");
        Assert.assertEquals(property2.datatypeWithEnum, "Test_int_enum");
        Assert.assertEquals(property2.name, "test_int_enum");
        Assert.assertTrue(property2.isEnum);
        Assert.assertEquals(property2.allowableValues.size(), 2);
        Assert.assertEquals(((List<String>) property2.allowableValues.get("values")).size(), 2);
        List<Map<String, Object>> enumVars2 = (List<Map<String, Object>>) property2.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars2.size(), 2);

        Assert.assertEquals(enumVars2.get(0).get("name"), simpleEnumValue ? "_1" : "TEST_INT_ENUM__1");
        Assert.assertEquals(enumVars2.get(0).get("value"), simpleEnumValue ? "_1" : "\"TEST_INT_ENUM__1\"");
        Assert.assertEquals(enumVars2.get(0).get("isString"), false);

        Assert.assertEquals(enumVars2.get(1).get("name"), simpleEnumValue ? "_2" : "TEST_INT_ENUM__2");
        Assert.assertEquals(enumVars2.get(1).get("value"), simpleEnumValue ? "_2" : "\"TEST_INT_ENUM__2\"");
        Assert.assertEquals(enumVars2.get(1).get("isString"), false);
    }

    @SuppressWarnings("unchecked")
    @Test(description = "Validate that unspecified enum values are added when the option is selected")
    public void unspecifiedEnumValuesAreAdded() {
        String enumKey = "aValidEnumWithoutUnspecifiedValues";

        final Schema<?> model = new Schema<>()
                .description("a sample model")
                .addProperty(enumKey, new StringSchema()._enum(Arrays.asList("foo", "bar")));

        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.additionalProperties().put(USE_SIMPLIFIED_ENUM_NAMES, true);
        codegen.additionalProperties().put(START_ENUMS_WITH_UNSPECIFIED, true);

        codegen.processOpts();
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, enumKey);
        Assert.assertEquals(property1.dataType, "string");
        Assert.assertEquals(property1.baseType, "string");
        Assert.assertEquals(property1.datatypeWithEnum, "A_valid_enum_without_unspecified_values");
        Assert.assertEquals(property1.name, "a_valid_enum_without_unspecified_values");
        Assert.assertTrue(property1.isEnum);
        Assert.assertEquals(property1.allowableValues.size(), 2);
        List<Map<String, Object>> enumVars1 = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars1.size(), 3);

        Assert.assertEquals(enumVars1.get(0).get("name"), "UNSPECIFIED");
        Assert.assertEquals(enumVars1.get(0).get("value"), "UNSPECIFIED");
        Assert.assertEquals(Boolean.valueOf((String) enumVars1.get(0).get("isString")), false);

        Assert.assertEquals(enumVars1.get(1).get("name"), "FOO");
        Assert.assertEquals(enumVars1.get(1).get("value"), "FOO");
        Assert.assertEquals(enumVars1.get(1).get("isString"), false);

        Assert.assertEquals(enumVars1.get(2).get("name"), "BAR");
        Assert.assertEquals(enumVars1.get(2).get("value"), "BAR");
        Assert.assertEquals(enumVars1.get(2).get("isString"), false);
    }

    @SuppressWarnings("unchecked")
    @Test(description = "Validate that unspecified enum values are NOT added when the option is selected if they are already present")
    public void unspecifiedEnumValuesIgnoredIfAlreadyPresent() {
        String enumKey = "aValidEnumWithUnspecifiedValues";

        final Schema<?> model = new Schema<>()
                .description("a sample model")
                .addProperty(enumKey, new StringSchema()._enum(Arrays.asList( "UNSPECIFIED", "foo")));

        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.additionalProperties().put(USE_SIMPLIFIED_ENUM_NAMES, true);
        codegen.additionalProperties().put(START_ENUMS_WITH_UNSPECIFIED, true);

        codegen.processOpts();
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, enumKey);
        Assert.assertEquals(property1.dataType, "string");
        Assert.assertEquals(property1.baseType, "string");
        Assert.assertEquals(property1.datatypeWithEnum, "A_valid_enum_with_unspecified_values");
        Assert.assertEquals(property1.name, "a_valid_enum_with_unspecified_values");
        Assert.assertTrue(property1.isEnum);
        Assert.assertEquals(property1.allowableValues.size(), 2);
        List<Map<String, Object>> enumVars1 = (List<Map<String, Object>>) property1.allowableValues.get("enumVars");
        Assert.assertEquals(enumVars1.size(), 2);

        Assert.assertEquals(enumVars1.get(0).get("name"), "UNSPECIFIED");
        Assert.assertEquals(enumVars1.get(0).get("value"), "UNSPECIFIED");
        Assert.assertEquals(enumVars1.get(0).get("isString"), false);

        Assert.assertEquals(enumVars1.get(1).get("name"), "FOO");
        Assert.assertEquals(enumVars1.get(1).get("value"), "FOO");
        Assert.assertEquals(enumVars1.get(1).get("isString"), false);
    }
}
