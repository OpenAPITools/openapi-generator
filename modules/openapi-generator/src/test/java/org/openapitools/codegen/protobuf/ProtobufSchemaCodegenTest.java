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

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ProtobufSchemaCodegen;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class ProtobufSchemaCodegenTest {

    /*@Test
    public void testCodeGenWithOneOf() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/oneOf.yaml");
        TestUtils.ensureContainsFile(files, output, "models/shape.proto");
        Path path = Paths.get(output + "/models/shape.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/shape.proto"));

        output.delete();
    }

    @Test
    public void testCodeGenWithOneOfSimple() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/oneOfSimple.yaml");
        TestUtils.ensureContainsFile(files, output, "models/shape.proto");
        Path path = Paths.get(output + "/models/shape.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/shape.proto"));

        output.delete();
    }*/

    @Test
    public void testCodeGen() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        assertThatThrownBy(() ->
                generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/allOf.yaml"))
                .isInstanceOf(RuntimeException.class)
                .hasMessage("At least components 'offerPayload_allOf_offerData_offerSet, travelOfferPayload_allOf_offerData_offerSet' are duplicated. Maybe not listed components are duplicated too.");
    }

    @Test
    public void testFeatureSet() {
        final ProtobufSchemaCodegen codegen = new ProtobufSchemaCodegen();
        FeatureSet featureSet = codegen.getGeneratorMetadata().getFeatureSet();

        Assert.assertTrue(featureSet.getWireFormatFeatures().contains(WireFormatFeature.PROTOBUF));
        Assert.assertEquals(featureSet.getWireFormatFeatures().size(), 1);
    }

    @Test
    public void testCodeGenWithAllOf() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/allOf_composition_discriminator.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/pet.proto"));

        output.delete();
    }

    @Test
    public void testExtensionFieldNumber() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-field-number.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-field-number.proto"));

        output.delete();
    }

    @Test
    public void testAutomaticOrderedIndexGeneration() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("numberedFieldNumberList", "True");
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/automatic-ordered-index-generation.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/automatic-ordered-index-generation.proto"));

        output.delete();
    }

    @Test
    public void testExtensionNegativeIndex() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-negative-index.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Only strictly positive field numbers are allowed");
        }        
    }

    @Test
    public void testExtensionNonIntegerIndex() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-non-integer-index.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "java.lang.String cannot be cast to java.lang.Integer");
        }        
    }

    @Test
    public void testExtensionDuplicateIndexes() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-duplicate-indexes.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "A same field number is used multiple times");
        }        
    }

    @Test
    public void testExtensionAutoGeneratedDuplicateIndexes() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-auto-generated-duplicate-indexes.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "A same field number is used multiple times");
        }        
    }

    @Test
    void testCodeGenWithEnum() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("startEnumsWithUnspecified", true);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/enum.yaml");
        TestUtils.ensureContainsFile(files, output, "models/status.proto");
        Path path = Paths.get(output + "/models/status.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/enum.proto"));

        output.delete();
    }

    @Test
    public void testNameSnakeCase() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("fieldNamesInSnakeCase", true);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/name-snakecase.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/name-snakecase.proto"));

        output.delete();       
    }

    @Test
    public void testExtensionName() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-name.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-name.proto"));

        output.delete();       
    }

    @Test
    public void testExtensionFieldName() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-field-name.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-field-name.proto"));

        output.delete();       
    }

    @Test
    public void testExtensionDuplicateNames() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-duplicate-names.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "A same field name is used multiple times");
        }      
    }

    @Test
    public void testExtensionEnumIndexes() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-indexes.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-enum-indexes.proto"));

        output.delete();
    }

    @Test
    public void testExtensionEnumMissingIndex() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-missing-index.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-enum-missing-index.proto"));

        output.delete();
    }

    @Test
    public void testExtensionEnumDuplicateIndexes() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-duplicate-indexes.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Enum indexes must be unique");
        }        
    }

    @Test
    public void testExtensionEnumNegativeIndex() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-negative-index.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Negative enum field numbers are not allowed");
        }        
    }

    @Test
    public void testExtensionEnumNonIntegerIndex() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-non-integer-index.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "java.lang.String cannot be cast to java.lang.Integer");
        }        
    }

    @Test
    public void testExtensionEnumFirstValueZero() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-first-value-zero.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Enum definitions must start with enum value zero");
        }        
    }

    @Test
    public void testExtensionEnumZeroReserved() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            properties.put("startEnumsWithUnknown", true);
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-zero-reserved.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Field number zero reserved for first enum value");
        }        
    }

    @Test
    public void testExtensionEnumDescriptions() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-descriptions.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-enum-descriptions.proto"));

        output.delete();
    }

    @Test
    public void testExtensionEnumDuplicateNames() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();

            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-enum-duplicate-names.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Duplicate enum name");
        }     
    }

    @Test
    public void testExtensionAmaEnum() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-ama-enum.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-ama-enum.proto"));

        output.delete();
    }

    @Test
    public void testExtensionAmaEnumNonMatchingItem() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-ama-enum-non-matching-item.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-ama-enum-non-matching-item.proto"));

        output.delete();
    }

    @Test
    public void testExtensionAmaEnumDuplicateValues() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/extension-ama-enum-duplicate-values.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "Duplicate value in x-ama-enum.values");
        }        
    }

    @Test
    public void testPropertyAnyType1() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/property-any-type-1.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/property-any-type.proto"));

        output.delete();   
    }

    @Test
    public void testPropertyAnyType2() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/property-any-type-2.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/property-any-type.proto"));

        output.delete();   
    }

    @Test
    public void testOperationAnyType() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/operation-any-type.yaml");
        TestUtils.ensureContainsFile(files, output, "services/default_service.proto");
        Path path = Paths.get(output + "/services/default_service.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/operation-any-type.proto"));

        output.delete();       
    }

    @Test
    public void testNoDuplicateEnumUnknownValueAllOf() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("startEnumsWithUnknown", true);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/no-duplicate-enum-unknown-value-allOf.yaml");
        TestUtils.ensureContainsFile(files, output, "models/cat.proto");
        Path path = Paths.get(output + "/models/cat.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/no-duplicate-enum-unknown-value-allOf.proto"));

        output.delete(); 
    }

    @Test
    public void testCustomOptions() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("customOptionsSpec", "src/test/resources/3_0/protobuf-schema/ama_custom_options.yaml");
        properties.put("enumStructNameAsPrefix", true);
        properties.put("fieldNamesInSnakeCase", true);
        properties.put("startEnumsWithUnspecified", true);
        properties.put("removeEnumValuePrefix", false);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/custom-options.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        TestUtils.ensureContainsFile(files, output, "custom_options/ama_custom_options.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/custom-options.proto"));
        path = Paths.get(output + "/custom_options/ama_custom_options.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/ama_custom_options.proto"));

        output.delete();
    }

    @Test
    public void testCustomOptionArray() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("customOptionsSpec", "src/test/resources/3_0/protobuf-schema/ama_custom_options.yaml");
        properties.put("enumStructNameAsPrefix", true);
        properties.put("fieldNamesInSnakeCase", true);
        properties.put("startEnumsWithUnspecified", true);
        properties.put("removeEnumValuePrefix", false);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/custom-option-array.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/custom-option-array.proto"));

        output.delete();
    }
    
    @Test
    public void testCustomOptionNoCategory() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("customOptionsSpec", "src/test/resources/3_0/protobuf-schema/ama_custom_options.yaml");
        properties.put("enumStructNameAsPrefix", true);
        properties.put("fieldNamesInSnakeCase", true);
        properties.put("startEnumsWithUnspecified", true);
        properties.put("removeEnumValuePrefix", false);
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/custom-option-no-category.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/custom-option-no-category.proto"));

        output.delete();
    }

    @Test
    public void testCustomOptionsNotValidValue() throws IOException {

        try {
            Map<String, Object> properties = new HashMap<>();
            properties.put("customOptionsSpec", "src/test/resources/3_0/protobuf-schema/ama_custom_options.yaml");
            properties.put("enumStructNameAsPrefix", true);
            properties.put("fieldNamesInSnakeCase", true);
            properties.put("startEnumsWithUnspecified", true);
            Map<String, String> globalProperties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/custom-options-not-valid-value.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertTrue(e.getCause().getMessage().startsWith("value \"PERSONAL_DATA_FIELD_WRONG\" is not part of allowed enum values"));
        }    
    }

    @Test
    public void testExtensionModelPackage() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/package_structure/my_package/foo/extension-package-name.yaml");
        // generation in different folders according to x-package-name
        TestUtils.ensureContainsFile(files, output, "my_package/foo/pet.proto");
        TestUtils.ensureContainsFile(files, output, "my_package/bar/photo.proto");
        TestUtils.ensureContainsFile(files, output, "models/tag.proto");
        
        Path path = Paths.get(output + "/my_package/foo/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/package_structure/my_package/foo/pet.proto"));
        path = Paths.get(output + "/my_package/bar/photo.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/package_structure/my_package/bar/photo.proto"));
        path = Paths.get(output + "/models/tag.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/package_structure/models/tag.proto"));

        output.delete();
    }

    @Test
    public void testTimeTypes() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        Map<String, String> globalProperties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, globalProperties, "src/test/resources/3_0/protobuf-schema/time-types.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        TestUtils.ensureContainsFile(files, output, "google/type/date.proto");
        TestUtils.ensureContainsFile(files, output, "google/type/timeofday.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/time-types.proto"));

        output.delete();       
    }

    private void assertFileEquals(Path generatedFilePath, Path expectedFilePath) throws IOException {
        String generatedFile = new String(Files.readAllBytes(generatedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");
        String expectedFile = new String(Files.readAllBytes(expectedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");

        assertEquals(generatedFile, expectedFile);
    }

    private List<File> generate(File output, Map<String, Object> properties, Map<String, String> globalProperties, String inputFile) {
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setAdditionalProperties(properties)
                .setGlobalProperties(globalProperties)
                .setInputSpec(inputFile)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        
        return generator.opts(clientOptInput).generate();
    }
}
