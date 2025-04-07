/*
 * Copyright 2022, 2024 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022, 2024 Oracle and/or its affiliates
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

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaHelidonCommonCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class JavaHelidonCommonCodegenTest {

    private DefaultGenerator mpServerGenerator;
    private DefaultGenerator mpClientGenerator;
    private CodegenConfigurator mpServerConfigurator;
    private CodegenConfigurator mpClientConfigurator;
    private String mpServerOutputDir;
    private String mpClientOutputDir;

    @BeforeMethod
    public void setup() throws IOException {
        File mpServerOutput = Files.createTempDirectory("testMpServer").toFile();
        mpServerOutput.deleteOnExit();
        mpServerOutputDir = mpServerOutput.getAbsolutePath().replace('\\', '/');

        File mpClientOutput = Files.createTempDirectory("testMpClient").toFile();
        mpClientOutputDir = mpClientOutput.getAbsolutePath().replace('\\', '/');

        mpServerConfigurator = new CodegenConfigurator()
                .setGeneratorName("java-helidon-server")
                .setLibrary("mp")
                .setInputSpec("src/test/resources/3_0/helidon/petstore-for-testing.yaml")
                .setOutputDir(mpServerOutputDir);

        mpClientConfigurator = new CodegenConfigurator()
                .setGeneratorName("java-helidon-client")
                .setLibrary("mp")
                .setInputSpec("src/test/resources/3_0/helidon/petstore-for-testing.yaml")
                .setOutputDir(mpClientOutputDir);


        mpServerGenerator = new DefaultGenerator();
        mpClientGenerator = new DefaultGenerator();
    }

    @Test
    public void defaultVersionTest() {
        runServerVersionTest(null, null);
    }

    @Test
    public void customHelidonVersionOnlyTest() {
        runServerVersionTest("3.0.0", null);
    }

    @Test
    void customHelidonMajorVersionOnlyTest() {
        runServerVersionTest("3", null);
    }

    @Test
    public void customParentVersionOnlyTest() {
        runServerVersionTest(null, "3.0.0");
    }

    @Test
    public void bothEqualsVersionTest() {
        runServerVersionTest("3.0.0", "3.0.0");
    }

    @Test
    public void bothNotEqualsVersionTest() {
        IllegalArgumentException e = Assert.expectThrows(IllegalArgumentException.class, () -> runServerVersionTest("1.0.0", "2.0.0"));
        Assert.assertEquals(
                "Both parentVersion and helidonVersion properties were set with different value.",
                e.getMessage());
    }

    @Test
    void customHelidonVersionOnlyClientTest() {
        runClientVersionTest("4", null);
    }

    private void runServerVersionTest(String helidonVersion, String parentVersion) {
        runVersionTest(helidonVersion, parentVersion, mpServerGenerator, mpServerConfigurator, mpServerOutputDir);
    }

    private void runClientVersionTest(String helidonVersion, String parentVersion) {
        runVersionTest(helidonVersion, parentVersion, mpClientGenerator, mpClientConfigurator, mpClientOutputDir);
    }

    private void runVersionTest(String helidonVersion,
                                String parentVersion,
                                DefaultGenerator generator,
                                CodegenConfigurator configurator,
                                String outputDir) {
        Map<String, Object> additionalProperties = new HashMap<>();
        String expected = JavaHelidonCommonCodegen.defaultHelidonVersion();
        if (parentVersion != null) {
            additionalProperties.put(CodegenConstants.PARENT_VERSION, parentVersion);
            expected = JavaHelidonCommonCodegen.chooseVersion(parentVersion);
        }
        if (helidonVersion != null) {
            additionalProperties.put("helidonVersion", helidonVersion);
            expected = JavaHelidonCommonCodegen.chooseVersion(helidonVersion);
        }
        generator.opts(configurator.setAdditionalProperties(additionalProperties)
                .toClientOptInput());
        List<File> files = generator.generate();

        TestUtils.ensureContainsFile(files, Paths.get(outputDir).toFile(), "pom.xml");
        TestUtils.assertFileContains(Paths.get(outputDir + "/pom.xml"),
                String.format(Locale.ROOT, "<version>%s</version>", expected));
    }

}
