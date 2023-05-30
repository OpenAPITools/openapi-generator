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

package org.openapitools.codegen;

import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MyImportTest {

    // @Test
    public void testGenerate() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("hideGenerationTimestamp","true");

        File output = new File("C:\\Martin\\Projekte\\openapi-generator\\samples\\server\\petstore\\java-msf4j");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java-msf4j")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        output.deleteOnExit();

        TestUtils.ensureContainsFile(files, output, "README.md");
    }

}
