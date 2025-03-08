/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.cpppistache;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Abstract test class to make one or multiple generators generate files for one specific input spec
 */
public abstract class AbstractGeneratorsTest {
    /**
     * Test each with a given input spec
     *
     * @param inputSpec The input spec to use
     * @return Map of generator and files
     * @throws IOException if the test directory cannot be created
     */
    protected Map<String, List<File>> eachWith(String inputSpec) throws IOException {
        Objects.requireNonNull(inputSpec, "Specify an inputspec to run that test");
        Map<String, List<File>> generatedFilesByGenerator = new HashMap<>();

        for (final CodegenConfig codegenConfig : CodegenConfigLoader.getAll()) {
            generatedFilesByGenerator.put(codegenConfig.getName(), oneWith(codegenConfig.getName(), inputSpec));
        }

        return generatedFilesByGenerator;
    }

    /**
     * Test each with a given input spec
     *
     * @param generatorName the generator name to use
     * @param inputSpec     The input spec to use
     * @return List of generated files
     * @throws IOException if the test directory cannot be created
     */
    protected List<File> oneWith(String generatorName, String inputSpec) throws IOException {
        Objects.requireNonNull(generatorName, "Specify a generatorName to run that test");
        Objects.requireNonNull(inputSpec, "Specify an inputspec to run that test");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setInputSpec(inputSpec)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        return generator.opts(clientOptInput).generate();
    }
}
