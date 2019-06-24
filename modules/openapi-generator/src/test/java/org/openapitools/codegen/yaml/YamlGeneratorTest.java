/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.yaml;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.OpenAPIYamlGenerator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

public class YamlGeneratorTest {

    @Test
    public void testGeneratePing() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi-yaml")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 4);
        TestUtils.ensureContainsFile(generatedFiles, output, "openapi/openapi.yaml");
        TestUtils.ensureContainsFile(generatedFiles, output, "README.md");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator/VERSION");

        output.deleteOnExit();
    }


    @Test
    public void testGeneratePingOtherOutputFile() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(OpenAPIYamlGenerator.OUTPUT_NAME, "ping.yaml");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("openapi-yaml")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 4);
        TestUtils.ensureContainsFile(generatedFiles, output, "ping.yaml");
        TestUtils.ensureContainsFile(generatedFiles, output, "README.md");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator-ignore");
        TestUtils.ensureContainsFile(generatedFiles, output, ".openapi-generator/VERSION");

        output.deleteOnExit();
    }
}
