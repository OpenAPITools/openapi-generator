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

package org.openapitools.codegen;

import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

public class AllGeneratorsTest {

    @Test
    public void testEachWithPetstore() throws IOException {
        for (final CodegenConfig codegenConfig : CodegenConfigLoader.getAll()) {
            File output = Files.createTempDirectory("test").toFile();
            output.deleteOnExit();

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName(codegenConfig.getName())
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

            final ClientOptInput clientOptInput = configurator.toClientOptInput();
            DefaultGenerator generator = new DefaultGenerator();
            List<File> files = generator.opts(clientOptInput).generate();

            // Main intention of this test is to check that nothing crashes. Besides, we check here that
            // at least 1 file is generated, besides the common ".openapi-generator-ignore", "FILES" and "VERSION" files.
            Assert.assertTrue(files.size() >= 4);
        }
    }

}
