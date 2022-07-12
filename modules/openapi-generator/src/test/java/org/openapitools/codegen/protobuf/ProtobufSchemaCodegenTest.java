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
        Map<String, Object> properties = new HashMap<>();
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, "src/test/resources/3_0/allOf_composition_discriminator.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/pet.proto"));

        output.delete();
    }

    @Test
    public void testExtensionFieldNumber() throws IOException {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/extension-field-number.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/extension-field-number.proto"));

        output.delete();
    }

    @Test
    public void testAutomaticOrderedIndexGeneration() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("numberedFieldNumberList", "True");

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/automatic-ordered-index-generation.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/automatic-ordered-index-generation.proto"));

        output.delete();
    }

    @Test
    public void testExtensionNegativeIndex() throws IOException {
        try {
            Map<String, Object> properties = new HashMap<>();
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/extension-negative-index.yaml");
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
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/extension-non-integer-index.yaml");
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
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/extension-duplicate-indexes.yaml");
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
            File output = Files.createTempDirectory("test").toFile();
            List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/extension-auto-generated-duplicate-indexes.yaml");
            fail("No exception thrown!");
        }
        catch (RuntimeException e) {
            assertEquals(e.getCause().getMessage(), "A same field number is used multiple times");
        }        
    }

    @Test
    public void testNameSnakeCase() throws IOException {
        Map<String, Object> properties = new HashMap<>();
        properties.put("fieldNamesInSnakeCase", true);

        File output = Files.createTempDirectory("test").toFile();
        List<File> files = generate(output, properties, "src/test/resources/3_0/protobuf-schema/name-snakecase.yaml");
        TestUtils.ensureContainsFile(files, output, "models/pet.proto");
        Path path = Paths.get(output + "/models/pet.proto");
        assertFileEquals(path, Paths.get("src/test/resources/3_0/protobuf-schema/name-snakecase.proto"));

        output.delete();       
    }

    private void assertFileEquals(Path generatedFilePath, Path expectedFilePath) throws IOException {
        String generatedFile = new String(Files.readAllBytes(generatedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");
        String expectedFile = new String(Files.readAllBytes(expectedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");

        assertEquals(generatedFile, expectedFile);
    }

    private List<File> generate(File output, Map<String, Object> properties, String inputFile) {        
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("protobuf-schema")
                .setAdditionalProperties(properties)
                .setInputSpec(inputFile)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        
        return generator.opts(clientOptInput).generate();
    }
}
