/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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

package org.openapitools.codegen.java.helidon;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.junit.Assert;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class JavaHelidonCommonCodegenTest {

    private DefaultGenerator generator;
    private CodegenConfigurator configurator;
    private String outputDir;

    @BeforeMethod
    public void setup() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        outputDir = output.getAbsolutePath().replace('\\', '/');

        configurator = new CodegenConfigurator()
                .setGeneratorName("java-helidon-server")
                .setLibrary("mp")
                .setInputSpec("src/test/resources/3_0/helidon/petstore-for-testing.yaml")
                .setOutputDir(outputDir);

        generator = new DefaultGenerator();
    }

    @Test
    public void defaultVersionTest() {
        runVersionTest(null, null);
    }

    @Test
    public void customHelidonVersionOnlyTest() {
        runVersionTest("3.0.0", null);
    }

    @Test
    public void customParentVersionOnlyTest() {
        runVersionTest(null, "3.0.0");
    }

    @Test
    public void bothEqualsVersionTest() {
        runVersionTest("3.0.0", "3.0.0");
    }

    @Test
    public void bothNotEqualsVersionTest() {
        IllegalArgumentException e = Assert.assertThrows(IllegalArgumentException.class,() -> runVersionTest("1.0.0", "2.0.0"));
        Assert.assertEquals(
                "Both parentVersion and helidonVersion properties were set with different value.",
                e.getMessage());
    }

    private void runVersionTest(String helidonVersion, String parentVersion) {
        Map<String, Object> additionalProperties = new HashMap<>();
        String expected = "3.0.1";
        if (parentVersion != null) {
            additionalProperties.put(CodegenConstants.PARENT_VERSION, parentVersion);
            expected = parentVersion;
        }
        if (helidonVersion != null) {
            additionalProperties.put("helidonVersion", helidonVersion);
            expected = helidonVersion;
        }
        generator.opts(configurator.setAdditionalProperties(additionalProperties)
                .toClientOptInput());
        List<File> files = generator.generate();

        TestUtils.ensureContainsFile(files, Paths.get(outputDir).toFile(), "pom.xml");
        TestUtils.assertFileContains(Paths.get(outputDir + "/pom.xml"),
                String.format(Locale.ROOT, "<version>%s</version>", expected));
    }

}
