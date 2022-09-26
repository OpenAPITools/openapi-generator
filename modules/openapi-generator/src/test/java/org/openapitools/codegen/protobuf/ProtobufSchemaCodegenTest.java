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

        output.delete();
    }

    private void assertFileEquals(Path generatedFilePath, Path expectedFilePath) throws IOException {
        String generatedFile = new String(Files.readAllBytes(generatedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");
        String expectedFile = new String(Files.readAllBytes(expectedFilePath), StandardCharsets.UTF_8)
            .replace("\n", "").replace("\r", "");

        assertEquals(generatedFile, expectedFile);
    }
}
