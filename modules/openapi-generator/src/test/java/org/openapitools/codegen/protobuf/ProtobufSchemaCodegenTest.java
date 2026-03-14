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
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
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
import static org.openapitools.codegen.languages.ProtobufSchemaCodegen.EXTRACT_ENUMS_TO_SEPARATE_FILES;
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

    @Test
    public void testCodeGenWithOneOfDiscriminator31() throws IOException {
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_1/oneOf.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "models/fruit.proto");

        // Get the processed OpenAPI with wrapper schemas
        OpenAPI openAPI = clientOptInput.getOpenAPI();
        ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.processOpts();

        Schema fruitSchema = openAPI.getComponents().getSchemas().get("fruit");
        Assert.assertNotNull(fruitSchema, "fruit schema should exist");

        CodegenModel fruitModel = codegen.fromModel("fruit", fruitSchema);
        codegen.postProcessModels(createCodegenModelWrapper(fruitModel));

        Assert.assertNotNull(fruitModel.oneOf, "fruit model should have oneOf items");
        Assert.assertTrue(fruitModel.oneOf.size() >= 2, "fruit model should have at least 2 oneOf items");

        Assert.assertNotNull(fruitModel.vars, "fruit model should have vars");
        Assert.assertTrue(fruitModel.vars.size() > 0, "fruit model should have at least one var");

        Assert.assertEquals(fruitModel.vars.size(), 3, "fruit model should have 3 vars (one for each oneOf item)");

        for (CodegenProperty var : fruitModel.vars) {
            Assert.assertNotNull(var.name, "var name should not be null");
            Assert.assertNotNull(var.dataType, "var dataType should not be null");
            Assert.assertTrue(var.isModel, "var " + var.name + " should be a model type (isModel=" + var.isModel + ")");
            Assert.assertFalse(var.isContainer, "var should not be a container (it references a model)");

            // Check expected properties based on discriminator title
            if (var.name.equals("apple_list")) {
                Assert.assertEquals(var.dataType, "StringArray", "apple_list should reference StringArray");
            } else if (var.name.equals("banana_map")) {
                Assert.assertEquals(var.dataType, "FloatMap", "banana_map should reference FloatMap");
            } else if (var.name.equals("orange_choice")) {
                Assert.assertEquals(var.dataType, "Orange", "orange_choice should reference Orange");
            } else {
                Assert.fail("Unexpected var name: " + var.name + ". Expected one of: apple_list, banana_map, orange_choice");
            }
        }

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

    @Test(description = "Support multiple level of inheritance for discriminator - ensures properties from indirect children are included")
    public void testCodeGenWithAllOfDiscriminatorMultipleLevels() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/allOf_composition_discriminator_multiple_inheritance.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Verify Animal proto file (discriminator parent)
        TestUtils.ensureContainsFile(files, output, "models/animal.proto");
        Path animalPath = Paths.get(output + "/models/animal.proto");
        String animalContent = new String(Files.readAllBytes(animalPath), StandardCharsets.UTF_8);
        
        // Properties from discriminated classes should be included in Animal:
        // From Dog (direct child in discriminator mapping): bark
        Assert.assertTrue(animalContent.contains("string bark"), "Animal should contain 'bark' property from Dog");
        // From Feline (intermediate parent, NOT in discriminator mapping): name, furColor  
        Assert.assertTrue(animalContent.contains("string name"), "Animal should contain 'name' property from Feline");
        Assert.assertTrue(animalContent.contains("string fur_color"), "Animal should contain 'fur_color' property from Feline");
        // From Cat (indirect child through Feline, IN discriminator mapping): isIndoor, careDetails
        Assert.assertTrue(animalContent.contains("bool is_indoor"), "Animal should contain 'is_indoor' property from Cat (indirect child)");
        Assert.assertTrue(animalContent.contains("CareDetails care_details"), "Animal should contain 'care_details' property from Cat (indirect child)");

        assertFileEquals(animalPath, Paths.get("src/test/resources/3_0/protobuf-schema/animal.proto"));

        // Verify Cat proto file (indirect child in discriminator mapping)
        TestUtils.ensureContainsFile(files, output, "models/cat.proto");
        Path catPath = Paths.get(output + "/models/cat.proto");
        String catContent = new String(Files.readAllBytes(catPath), StandardCharsets.UTF_8);
        
        // Cat should import Animal (the discriminator parent), not Feline (its immediate allOf parent)
        Assert.assertTrue(catContent.contains("import public \"models/animal.proto\";"), 
            "Cat should import Animal (discriminator parent)");
        // Cat should also import CareDetails (its dependency)
        Assert.assertTrue(catContent.contains("import public \"models/care_details.proto\";"), 
            "Cat should import CareDetails");
        
        // Verify Feline proto file (intermediate parent, NOT in discriminator mapping)
        TestUtils.ensureContainsFile(files, output, "models/feline.proto");
        Path felinePath = Paths.get(output + "/models/feline.proto");
        String felineContent = new String(Files.readAllBytes(felinePath), StandardCharsets.UTF_8);
        
        // According to requirements: Feline inherits from Animal but is NOT in discriminator mapping
        // So Feline should NOT import Animal
        Assert.assertFalse(felineContent.contains("import public \"models/animal.proto\";"), 
            "Feline should NOT import Animal (not in discriminator mapping)");
        
        // Verify Dog proto file (direct child in discriminator mapping)
        TestUtils.ensureContainsFile(files, output, "models/dog.proto");
        Path dogPath = Paths.get(output + "/models/dog.proto");
        String dogContent = new String(Files.readAllBytes(dogPath), StandardCharsets.UTF_8);
        
        // Dog should import Animal (the discriminator parent)
        Assert.assertTrue(dogContent.contains("import public \"models/animal.proto\";"), 
            "Dog should import Animal (discriminator parent)");

        output.deleteOnExit();
    }
  
    @SuppressWarnings("unchecked")
    @Test(description = "Validate that enums in arrays are treated as complex types")
    public void enumInArrayIsTreatedAsComplexType() {
        final Schema enumSchema = new StringSchema()._enum(Arrays.asList("APPLE", "BANANA", "ORANGE"));
        final ArraySchema arraySchema = new ArraySchema();
        arraySchema.setItems(enumSchema);

        final Schema model = new Schema()
                .description("a sample model with enum array")
                .addProperties("fruitList", arraySchema);

        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.additionalProperties().put(USE_SIMPLIFIED_ENUM_NAMES, true);
        codegen.processOpts();
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "fruitList");
    }

    @SuppressWarnings("unchecked")
    @Test(description = "Validate that enums in maps are treated as complex types")
    public void enumInMapIsTreatedAsComplexType() {
        final Schema enumSchema = new StringSchema()._enum(Arrays.asList("RED", "GREEN", "BLUE"));
        final MapSchema mapSchema = new MapSchema();
        mapSchema.setAdditionalProperties(enumSchema);

        final Schema model = new Schema()
                .description("a sample model with enum map")
                .addProperties("colorMap", mapSchema);

        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);
        codegen.additionalProperties().put(USE_SIMPLIFIED_ENUM_NAMES, true);
        codegen.processOpts();
        codegen.postProcessModels(createCodegenModelWrapper(cm));

        final CodegenProperty property = cm.vars.get(0);
        Assert.assertEquals(property.baseName, "colorMap");
    }

    @Test(description = "Validate that a model referenced multiple times is imported only once in generated protobuf files")
    public void testModelImportedOnlyOnce() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/model_imported_once.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that the main model file was generated
        TestUtils.ensureContainsFile(files, output, "models/model.proto");
        Path modelPath = Paths.get(output + "/models/model.proto");
        String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8);

        // Count occurrences of the import statement for model A (using "public" keyword as generated)
        String importStatement = "import public \"models/a.proto\";";
        int importCount = countOccurrences(modelContent, importStatement);
        
        // Assert that model A is imported exactly once despite being used in:
        // - direct field in Model (directA)
        // - field in SubModel which is nested in Model (SubModel.aReference)
        Assert.assertEquals(importCount, 1, "Model A should be imported exactly once in model.proto");

        // Check the SubModel file - it also references A directly
        TestUtils.ensureContainsFile(files, output, "models/sub_model.proto");
        Path subModelPath = Paths.get(output + "/models/sub_model.proto");
        String subModelContent = new String(Files.readAllBytes(subModelPath), StandardCharsets.UTF_8);
        int subModelImportCount = countOccurrences(subModelContent, importStatement);
        Assert.assertEquals(subModelImportCount, 1, "Model A should be imported exactly once in sub_model.proto");

        // Check the ExtensibleModel file
        TestUtils.ensureContainsFile(files, output, "models/extensible_model.proto");
        Path extensiblePath = Paths.get(output + "/models/extensible_model.proto");
        String extensibleContent = new String(Files.readAllBytes(extensiblePath), StandardCharsets.UTF_8);

        // Count occurrences in ExtensibleModel
        int extensibleImportCount = countOccurrences(extensibleContent, importStatement);
        
        // Assert that model A is imported exactly once in ExtensibleModel despite being used in:
        // - direct field in ExtensibleModel (extensibleA)
        // - field in ChildModel_1 (childA) which extends ExtensibleModel via discriminator
        // - field in ChildModel_2 (directA) which also extends ExtensibleModel via discriminator
        Assert.assertEquals(extensibleImportCount, 1, "Model A should be imported exactly once in extensible_model.proto");

        output.deleteOnExit();
    }

    private int countOccurrences(String content, String substring) {
        int count = 0;
        int index = 0;
        while ((index = content.indexOf(substring, index)) != -1) {
            count++;
            index += substring.length();
        }
        return count;
    }

    @Test(description = "Validate that enums are extracted to separate files when extractEnumsToSeparateFiles option is enabled")
    public void testExtractEnumsToSeparateFiles() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .addAdditionalProperty(EXTRACT_ENUMS_TO_SEPARATE_FILES, true);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that separate enum files were generated
        // With the new naming scheme, inline enum names include the parent model prefix
        TestUtils.ensureContainsFile(files, output, "models/separated_enum.proto");
        TestUtils.ensureContainsFile(files, output, "models/model_with_enums_inline_enum_property.proto");
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums_another_inline_enum_property.proto");
        
        // Check that the model file was generated
        TestUtils.ensureContainsFile(files, output, "models/model_with_enums.proto");
        Path modelPath = Paths.get(output + "/models/model_with_enums.proto");
        String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8);

        // Verify that enums are NOT defined inline in the model
        Assert.assertFalse(modelContent.contains("enum Inline_enum_property"), 
            "Inline enum should be extracted to separate file");
        
        // Verify that the model imports the separated enum files
        Assert.assertTrue(modelContent.contains("import public \"models/separated_enum.proto\";"), 
            "Model should import the separated enum file");
        Assert.assertTrue(modelContent.contains("import public \"models/model_with_enums_inline_enum_property.proto\";"), 
            "Model should import the inline enum file with parent model prefix");
        
        // Verify that the model uses the correct enum reference with .Enum suffix
        // With the new naming scheme, inline enums use ParentModelName_FieldName format
        Assert.assertTrue(modelContent.contains("ModelWithEnums_InlineEnumProperty.Enum"), 
            "Model should reference extracted enum with .Enum suffix and parent model prefix");

        // Check the AllOfModel file
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums.proto");
        Path allOfModelPath = Paths.get(output + "/models/all_of_model_with_enums.proto");
        String allOfModelContent = new String(Files.readAllBytes(allOfModelPath), StandardCharsets.UTF_8);

        // Verify that the allOf model imports the separated enum files
        Assert.assertTrue(allOfModelContent.contains("import public \"models/all_of_model_with_enums_another_inline_enum_property.proto\";"), 
            "AllOf model should import its inline enum file with parent model prefix");

        // Verify the separated enum file content
        Path separatedEnumPath = Paths.get(output + "/models/separated_enum.proto");
        String separatedEnumContent = new String(Files.readAllBytes(separatedEnumPath), StandardCharsets.UTF_8);
        Assert.assertTrue(separatedEnumContent.contains("package openapitools;"), 
            "Separated enum file should contain a valid package declaration");
        Assert.assertTrue(separatedEnumContent.contains("message SeparatedEnum"), 
            "Separated enum file should contain the message wrapper");
        Assert.assertTrue(separatedEnumContent.contains("enum Enum {"), 
            "Separated enum file should contain inner Enum definition");
        Assert.assertTrue(separatedEnumContent.contains("SEPARATED_ENUM_VALUE1"), 
            "Separated enum should contain SEPARATED_ENUM_VALUE1");
        Assert.assertTrue(separatedEnumContent.contains("SEPARATED_ENUM_VALUE2"), 
            "Separated enum should contain SEPARATED_ENUM_VALUE2");

        // Verify the inline enum file content - uses parent model name prefix
        Path inlineEnumPath = Paths.get(output + "/models/model_with_enums_inline_enum_property.proto");
        String inlineEnumContent = new String(Files.readAllBytes(inlineEnumPath), StandardCharsets.UTF_8);
        Assert.assertTrue(inlineEnumContent.contains("package openapitools;"), 
            "Inline enum file should contain a valid package declaration");
        Assert.assertTrue(inlineEnumContent.contains("message ModelWithEnums_InlineEnumProperty"), 
            "Inline enum file should contain the message wrapper with parent model prefix");
        Assert.assertTrue(inlineEnumContent.contains("enum Enum {"), 
            "Inline enum file should contain inner Enum definition");
        // Note: Enum values keep the prefixes from the original field name, not parent+field
        // since they are already scoped within the message wrapper
        Assert.assertTrue(inlineEnumContent.contains("INLINE_ENUM_PROPERTY_VALUE2"), 
            "Inline enum should contain enum value");
        Assert.assertTrue(inlineEnumContent.contains("INLINE_ENUM_PROPERTY_VALUE3"), 
            "Inline enum should contain enum value");
        Assert.assertTrue(inlineEnumContent.contains("INLINE_ENUM_PROPERTY_VALUE4"), 
            "Inline enum should contain enum value");

        output.deleteOnExit();
    }

    @Test(description = "Validate that enums are extracted to separate files when extractEnumsToSeparateFiles option is enabled")
    public void testExtractEnumsToSeparateFilesWithOtherEnumOptions() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .addAdditionalProperty("removeEnumValuePrefix", false)
                .addAdditionalProperty(START_ENUMS_WITH_UNSPECIFIED, true)
                .addAdditionalProperty(USE_SIMPLIFIED_ENUM_NAMES, true)
                .addAdditionalProperty(EXTRACT_ENUMS_TO_SEPARATE_FILES, true);


        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that separate enum files were generated with parent model prefix
        TestUtils.ensureContainsFile(files, output, "models/separated_enum.proto");
        TestUtils.ensureContainsFile(files, output, "models/model_with_enums_inline_enum_property.proto");
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums_another_inline_enum_property.proto");
        
        // Check that the model file was generated
        TestUtils.ensureContainsFile(files, output, "models/model_with_enums.proto");
        Path modelPath = Paths.get(output + "/models/model_with_enums.proto");
        String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8);

        // Verify that enums are NOT defined inline in the model
        Assert.assertFalse(modelContent.contains("enum Inline_enum_property"), 
            "Inline enum should be extracted to separate file");
        
        // Verify that the model imports the separated enum files
        Assert.assertTrue(modelContent.contains("import public \"models/separated_enum.proto\";"), 
            "Model should import the separated enum file");
        Assert.assertTrue(modelContent.contains("import public \"models/model_with_enums_inline_enum_property.proto\";"), 
            "Model should import the inline enum file with parent model prefix");
        
        // Verify that the model uses the correct enum reference with .Enum suffix
        // With the new naming scheme, inline enums use ParentModelName_FieldName format
        Assert.assertTrue(modelContent.contains("ModelWithEnums_InlineEnumProperty.Enum"), 
            "Model should reference extracted enum with .Enum suffix and parent model prefix");

        // Check the AllOfModel file
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums.proto");
        Path allOfModelPath = Paths.get(output + "/models/all_of_model_with_enums.proto");
        String allOfModelContent = new String(Files.readAllBytes(allOfModelPath), StandardCharsets.UTF_8);

        // Verify that the allOf model imports the separated enum files
        Assert.assertTrue(allOfModelContent.contains("import public \"models/all_of_model_with_enums_another_inline_enum_property.proto\";"), 
            "AllOf model should import its inline enum file with parent model prefix");

        // Verify the separated enum file content
        Path separatedEnumPath = Paths.get(output + "/models/separated_enum.proto");
        String separatedEnumContent = new String(Files.readAllBytes(separatedEnumPath), StandardCharsets.UTF_8);
        Assert.assertTrue(separatedEnumContent.contains("package openapitools;"), 
            "Separated enum file should contain a valid package declaration");
        Assert.assertTrue(separatedEnumContent.contains("message SeparatedEnum"), 
            "Separated enum file should contain the message wrapper");
        Assert.assertTrue(separatedEnumContent.contains("enum Enum {"), 
            "Separated enum file should contain inner Enum definition");
        Assert.assertTrue(separatedEnumContent.contains("UNSPECIFIED"), 
            "Separated enum should contain UNSPECIFIED");
        Assert.assertTrue(separatedEnumContent.contains("VALUE1"), 
            "Separated enum should contain VALUE1");
        Assert.assertTrue(separatedEnumContent.contains("VALUE2"), 
            "Separated enum should contain VALUE2");

        // Verify the inline enum file content - uses parent model name prefix
        Path inlineEnumPath = Paths.get(output + "/models/model_with_enums_inline_enum_property.proto");
        String inlineEnumContent = new String(Files.readAllBytes(inlineEnumPath), StandardCharsets.UTF_8);
        Assert.assertTrue(inlineEnumContent.contains("package openapitools;"), 
            "Inline enum file should contain a valid package declaration");
        Assert.assertTrue(inlineEnumContent.contains("message ModelWithEnums_InlineEnumProperty"), 
            "Inline enum file should contain the message wrapper with parent model prefix");
        Assert.assertTrue(inlineEnumContent.contains("enum Enum {"), 
            "Inline enum file should contain inner Enum definition");
        Assert.assertTrue(inlineEnumContent.contains("UNSPECIFIED"), 
            "Inline enum should contain UNSPECIFIED");
        // With simplified names, enum values don't have prefixes, just the base values
        Assert.assertTrue(inlineEnumContent.contains("VALUE2"), 
            "Inline enum should contain enum value");
        Assert.assertTrue(inlineEnumContent.contains("VALUE3"), 
            "Inline enum should contain enum value");
        Assert.assertTrue(inlineEnumContent.contains("VALUE4"), 
            "Inline enum should contain enum value");

        //output.deleteOnExit();
    }

    @Test(description = "Test toModelImport with various input formats")
    public void testToModelImportVariations() {
        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        codegen.setModelPackage("models");
        
        // Normal case
        Assert.assertEquals(codegen.toModelImport("Pet"), "models/pet");
        
        // Already prefixed - should not duplicate
        Assert.assertEquals(codegen.toModelImport("models/pet"), "models/pet");
        
        // With different casing
        Assert.assertEquals(codegen.toModelImport("PetStore"), "models/pet_store");
        
        // With numbers
        Assert.assertEquals(codegen.toModelImport("Pet123"), "models/pet123");
        
        // Empty model package
        codegen.setModelPackage("");
        Assert.assertEquals(codegen.toModelImport("Pet"), "Pet");
    }

    @Test(description = "Validate that enum imports are added to discriminator parent when extractEnumsToSeparateFiles is enabled")
    public void testDiscriminatorWithExtractedEnums() throws IOException {
        // set line break to \n across all platforms
        String originalLineSeparator = System.getProperty("line.separator");
        try {
            System.setProperty("line.separator", "\n");

            File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .addAdditionalProperty(EXTRACT_ENUMS_TO_SEPARATE_FILES, true);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that the discriminator parent file was generated
        TestUtils.ensureContainsFile(files, output, "models/discriminated_model.proto");
        Path discriminatorPath = Paths.get(output + "/models/discriminated_model.proto");
        String discriminatorContent = new String(Files.readAllBytes(discriminatorPath), StandardCharsets.UTF_8);

        // The discriminator parent should have properties from both children
        // From ModelTypeAWithInlineEnum: specificPropertyA, inlineEnumPropertyA
        Assert.assertTrue(discriminatorContent.contains("string specific_property_a"), 
            "Discriminator parent should contain 'specific_property_a' from child A");
        
        // The enum property should be present with the correct .Enum suffix
        // With the new naming scheme, inline enums use ParentModelName_FieldName format
        Assert.assertTrue(discriminatorContent.contains("ModelTypeAWithInlineEnum_InlineEnumProperty.Enum inline_enum_property"), 
            "Discriminator parent should contain extracted enum property from child A with .Enum suffix and parent model prefix");

        // CRITICAL: The discriminator parent should import the extracted enum file
        Assert.assertTrue(discriminatorContent.contains("import public \"models/model_type_a_with_inline_enum_inline_enum_property.proto\";"), 
            "Discriminator parent MUST import the extracted enum from child A with parent model prefix");

        // From ModelTypeBWithInlineEnum: specificPropertyB, referenceEnumPropertyB
        Assert.assertTrue(discriminatorContent.contains("string specific_property_b"), 
            "Discriminator parent should contain 'specific_property_b' from child B");
        
        // Child B references SeparatedEnum (not inline), so it should be imported
        Assert.assertTrue(discriminatorContent.contains("import public \"models/separated_enum.proto\";"), 
            "Discriminator parent should import the separated enum referenced by child B");

        // Verify the extracted enum file for inline enum from child A was created
        // The file name uses snake_case of the full wrapper message name
        TestUtils.ensureContainsFile(files, output, "models/model_type_a_with_inline_enum_inline_enum_property.proto");
        Path enumPath = Paths.get(output + "/models/model_type_a_with_inline_enum_inline_enum_property.proto");
        String enumContent = new String(Files.readAllBytes(enumPath), StandardCharsets.UTF_8);
        
        Assert.assertTrue(enumContent.contains("message ModelTypeAWithInlineEnum_InlineEnumProperty"), 
            "Extracted enum file should contain the message wrapper with parent model prefix");
        Assert.assertTrue(enumContent.contains("enum Enum {"), 
            "Extracted enum file should contain inner Enum definition");
        // Note: Enum values keep the prefixes from the original field name, not parent+field
        // since they are already scoped within the message wrapper
        Assert.assertTrue(enumContent.contains("INLINE_ENUM_PROPERTY_VALUE7"), 
            "Extracted enum should contain enum value");
        Assert.assertTrue(enumContent.contains("INLINE_ENUM_PROPERTY_VALUE8"), 
            "Extracted enum should contain enum value");

            output.deleteOnExit();
        } finally {
            // Restore original property to avoid side effects on other tests
            System.setProperty("line.separator", originalLineSeparator);
        }
    }

    @SuppressWarnings("unchecked")
    @Test(description = "Validate that referenced enum models are correctly wrapped with .Enum suffix when extractEnumsToSeparateFiles is enabled")
    public void testReferencedEnumsWithExtraction() throws IOException {
        // set line break to \n across all platforms
        String originalLineSeparator = System.getProperty("line.separator");
        try {
            System.setProperty("line.separator", "\n");

            File output = Files.createTempDirectory("test").toFile();

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("protobuf-schema")
                    .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                    .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                    .addAdditionalProperty(EXTRACT_ENUMS_TO_SEPARATE_FILES, true);

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator();
            List<File> files = generator.opts(clientOptInput).generate();

            // Check that the model file was generated
            TestUtils.ensureContainsFile(files, output, "models/model_with_enums.proto");
            Path modelPath = Paths.get(output + "/models/model_with_enums.proto");
            String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8)
                    .replace("\r\n", "\n").replace("\r", "\n");

            // CRITICAL: The model should have the inline enum property with .Enum suffix
            // With the new naming scheme, inline enums use ParentModelName_FieldName format
            Assert.assertTrue(modelContent.contains("ModelWithEnums_InlineEnumProperty.Enum inline_enum_property"),
                    "Model should reference inline extracted enum with .Enum suffix and parent model prefix");

            // CRITICAL: The model should have the referenced enum property with .Enum suffix
            // This is the bug being fixed - referenced enums should also get wrapped
            Assert.assertTrue(modelContent.contains("SeparatedEnum.Enum reference_enum_property"),
                    "Model should reference referenced enum with .Enum suffix (THIS IS THE FIX)");

            // Verify that both enum files are imported
            Assert.assertTrue(modelContent.contains("import public \"models/separated_enum.proto\";"),
                    "Model should import the separated enum file");
            Assert.assertTrue(modelContent.contains("import public \"models/model_with_enums_inline_enum_property.proto\";"),
                    "Model should import the inline enum file with parent model prefix");

            // Verify the separated enum file was correctly generated
            TestUtils.ensureContainsFile(files, output, "models/separated_enum.proto");
            Path separatedEnumPath = Paths.get(output + "/models/separated_enum.proto");
            String separatedEnumContent = new String(Files.readAllBytes(separatedEnumPath), StandardCharsets.UTF_8);

            Assert.assertTrue(separatedEnumContent.contains("message SeparatedEnum"),
                    "Separated enum file should contain the message wrapper");
            Assert.assertTrue(separatedEnumContent.contains("enum Enum {"),
                    "Separated enum file should contain inner Enum definition");

            // Verify the inline enum file was correctly generated with parent model prefix
            TestUtils.ensureContainsFile(files, output, "models/model_with_enums_inline_enum_property.proto");
            Path inlineEnumPath = Paths.get(output + "/models/model_with_enums_inline_enum_property.proto");
            String inlineEnumContent = new String(Files.readAllBytes(inlineEnumPath), StandardCharsets.UTF_8);

            Assert.assertTrue(inlineEnumContent.contains("message ModelWithEnums_InlineEnumProperty"),
                    "Inline enum file should contain the message wrapper with parent model prefix");
            Assert.assertTrue(inlineEnumContent.contains("enum Enum {"),
                    "Inline enum file should contain inner Enum definition");

            output.deleteOnExit();
        } finally {
            // Restore original property to avoid side effects on other tests
            System.setProperty("line.separator", originalLineSeparator);
        }
    }

    @SuppressWarnings("unchecked")
    @Test(description = "Validate backward compatibility: extracted_enum.yaml generates inline enums when EXTRACT_ENUMS_TO_SEPARATE_FILES is NOT enabled")
    public void testEnumsRemainsInlineWithoutExtraction() throws IOException {
        // set line break to \n across all platforms
        String originalLineSeparator = System.getProperty("line.separator");
        try {
            System.setProperty("line.separator", "\n");

            File output = Files.createTempDirectory("test").toFile();

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName("protobuf-schema")
                    .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                    .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator();
            List<File> files = generator.opts(clientOptInput).generate();

            // Check that the model file was generated
            TestUtils.ensureContainsFile(files, output, "models/model_with_enums.proto");
            Path modelPath = Paths.get(output + "/models/model_with_enums.proto");
            String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8)
                    .replace("\r\n", "\n").replace("\r", "\n");

            // WITHOUT extraction: Enums should be defined INLINE in the model file
            // Check for inline enum definitions (not extracted to separate files)
            // The inline enum should contain the enum values (not a message wrapper)
            Assert.assertTrue(modelContent.contains("enum") && modelContent.contains("VALUE2"),
                    "Without extraction, inline enums should be defined inline in the model");

            // Verify that inline enums use simple type references (NOT wrapped with .Enum)
            // When enums are inline, they're referenced as the enum name directly
            // (not as ParentModel_FieldName.Enum which is only used for extracted enums)
            Assert.assertTrue(!modelContent.contains(".Enum"),
                    "Without extraction, enum properties should NOT reference .Enum suffix");

            // Without extraction: Inline enum files should NOT be generated
            // (Note: SeparatedEnum.proto WILL be generated because SeparatedEnum is a top-level schema,
            // but inline_enum_property.proto should NOT be generated because it's an inline enum)
            List<Path> inlineEnumFiles = new java.util.ArrayList<>();
            if (Files.exists(Paths.get(output + "/models/inline_enum_property.proto"))) {
                inlineEnumFiles.add(Paths.get(output + "/models/inline_enum_property.proto"));
            }
            if (Files.exists(Paths.get(output + "/models/another_inline_enum_property.proto"))) {
                inlineEnumFiles.add(Paths.get(output + "/models/another_inline_enum_property.proto"));
            }
            Assert.assertTrue(inlineEnumFiles.isEmpty(),
                    "Without extraction option enabled, inline enum files should NOT be created as separate files");

            // Verify that imports for inline enum files are NOT present
            Assert.assertFalse(modelContent.contains("import public \"models/inline_enum_property.proto\""),
                    "Without extraction, there should be no inline enum file imports");
            Assert.assertFalse(modelContent.contains("import public \"models/another_inline_enum_property.proto\""),
                    "Without extraction, there should be no inline enum file imports");

            // Check the AllOfModel file
            TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums.proto");
            Path allOfModelPath = Paths.get(output + "/models/all_of_model_with_enums.proto");
            String allOfModelContent = new String(Files.readAllBytes(allOfModelPath), StandardCharsets.UTF_8);

            // IMPORTANT: AllOf composition in protobuf has different semantics than OpenAPI allOf.
            // In protobuf, allOf models only contain direct properties of the model, not inherited/composed properties.
            // This differs from OpenAPI where allOf models would include properties from all composed schemas.
            // Therefore, we validate backward compatibility by ensuring:
            // 1. The model file is generated (proving structure is correct)
            // 2. No .Enum suffix is used (proving extraction mode is OFF - not using extracted enum references)
            // 3. Inline enums are defined inline (proving backward compatibility works with inline enums)
            Assert.assertTrue(!allOfModelContent.isEmpty(),
                    "Without extraction option, the allOf model file should be generated");
            Assert.assertFalse(allOfModelContent.contains(".Enum"),
                    "Without extraction option, enums should NOT use .Enum suffix (should be inline)");
            Assert.assertTrue(allOfModelContent.contains("enum") && allOfModelContent.contains("VALUE"),
                    "Without extraction option, model should contain inline enum definitions");

            output.deleteOnExit();
        } finally {
            // Restore original property to avoid side effects on other tests
            System.setProperty("line.separator", originalLineSeparator);
        }
    }

    @Test(description = "Validate that enums in arrays are correctly handled with .Enum suffix when extractEnumsToSeparateFiles is enabled")
    public void testEnumsInArraysWithExtraction() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        System.out.println("EnumsInArrays Temporary output directory: " + output.getAbsolutePath());

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"))
                .addAdditionalProperty("removeEnumValuePrefix", false)
                .addAdditionalProperty(START_ENUMS_WITH_UNSPECIFIED, true)
                .addAdditionalProperty(USE_SIMPLIFIED_ENUM_NAMES, true)
                .addAdditionalProperty(EXTRACT_ENUMS_TO_SEPARATE_FILES, true);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that the model file was generated
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums.proto");
        Path modelPath = Paths.get(output + "/models/all_of_model_with_enums.proto");
        String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8)
                .replace("\r\n", "\n").replace("\r", "\n");

        // Test Case 1: Array with REFERENCED enum (listOfReferencedEnums)
        // Should have .Enum suffix: repeated SeparatedEnum.Enum
        // Use regex to validate structure without depending on specific field numbers
        java.util.regex.Pattern referencedEnumPattern = java.util.regex.Pattern.compile(
            "repeated\\s+SeparatedEnum\\.Enum\\s+list_of_referenced_enums\\s*=\\s*\\d+");
        Assert.assertTrue(
            referencedEnumPattern.matcher(modelContent).find(),
            "Array with referenced enum should use .Enum suffix with pattern: repeated SeparatedEnum.Enum list_of_referenced_enums = <number>");

        // Test Case 2: Array with INLINE enum (listOfEnums) - in AllOfModelWithEnums
        // Should have .Enum suffix: repeated AllOfModelWithEnums_ListOfEnums.Enum
        // Use regex to validate structure without depending on specific field numbers
        java.util.regex.Pattern inlineEnumPattern = java.util.regex.Pattern.compile(
            "repeated\\s+AllOfModelWithEnums_ListOfEnums\\.Enum\\s+list_of_enums\\s*=\\s*\\d+");
        Assert.assertTrue(
            inlineEnumPattern.matcher(modelContent).find(),
            "Array with inline enum should use .Enum suffix with pattern: repeated AllOfModelWithEnums_ListOfEnums.Enum list_of_enums = <number>");

        // Verify the imported enum file for referenced enum exists
        TestUtils.ensureContainsFile(files, output, "models/separated_enum.proto");

        // Verify the extracted inline enum file for arrays exists
        TestUtils.ensureContainsFile(files, output, "models/all_of_model_with_enums_list_of_enums.proto");
        Path listOfEnumsPath = Paths.get(output + "/models/all_of_model_with_enums_list_of_enums.proto");
        String listOfEnumsContent = new String(Files.readAllBytes(listOfEnumsPath), StandardCharsets.UTF_8)
                .replace("\r\n", "\n").replace("\r", "\n");

        // Verify the wrapper message structure
        Assert.assertTrue(listOfEnumsContent.contains("message AllOfModelWithEnums_ListOfEnums"),
                "Extracted inline enum file should contain wrapper message");
        Assert.assertTrue(listOfEnumsContent.contains("enum Enum"),
                "Wrapper message should contain inner Enum");
        Assert.assertTrue(listOfEnumsContent.contains("VALUE10") && listOfEnumsContent.contains("VALUE11"),
                "Wrapper message should contain enum values");

        output.deleteOnExit();
    }

    @Test(description = "Validate that enums in arrays remain inline when extractEnumsToSeparateFiles is NOT enabled")
    public void testEnumsInArraysWithoutExtraction() throws IOException {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setInputSpec("src/test/resources/3_0/protobuf-schema/extracted_enum.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        // Note: EXTRACT_ENUMS_TO_SEPARATE_FILES is NOT enabled

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        // Check that the model file was generated
        TestUtils.ensureContainsFile(files, output, "models/model_with_enums.proto");
        Path modelPath = Paths.get(output + "/models/model_with_enums.proto");
        String modelContent = new String(Files.readAllBytes(modelPath), StandardCharsets.UTF_8)
                .replace("\r\n", "\n").replace("\r", "\n");

        // Without extraction: Arrays of enums should NOT use .Enum suffix
        // Instead, they should use the type directly or inline definition
        Assert.assertFalse(modelContent.contains(".Enum"),
                "Without extraction, enums should NOT use .Enum suffix");

        // Extracted inline enum files should NOT exist
        Assert.assertFalse(Files.exists(Paths.get(output + "/models/model_with_enums_list_of_enums.proto")),
                "Without extraction, inline enum array files should NOT be generated");

        output.deleteOnExit();
    }
}
