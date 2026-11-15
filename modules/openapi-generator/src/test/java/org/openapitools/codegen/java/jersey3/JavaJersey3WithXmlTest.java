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

package org.openapitools.codegen.java.jersey3;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.testng.Assert.assertTrue;

/**
 * Verifies that the jersey3 library supports the withXml option
 * when {@code withXml=true} is set.
 */
public class JavaJersey3WithXmlTest {

    private static final String PETSTORE_SPEC = "src/test/resources/3_0/petstore.yaml";

    @Test
    public void jersey3WithXmlDoesNotThrow() throws Exception {
        Path output = Files.createTempDirectory("jersey3-withXml-positive");
        output.toFile().deleteOnExit();

        List<File> files = generateWithXml(output);
        assertTrue(!files.isEmpty(), "Generation should produce files");
    }

    @Test
    public void pomReferencesXmlArtifacts_Jackson2() throws Exception {
        Path output = Files.createTempDirectory("jersey3-jackson2-xml-pom");
        output.toFile().deleteOnExit();
        generateWithXml(output);

        String pom = Files.readString(output.resolve("pom.xml"));
        //xml jackson2 libs
        assertTrue(pom.contains("<groupId>com.fasterxml.jackson.dataformat</groupId>"),
                "pom.xml should contain com.fasterxml.jackson.dataformat group");
        assertTrue(pom.contains("<artifactId>jackson-dataformat-xml</artifactId>"),
                "pom.xml should contain jackson-dataformat-xml artifact");
    }

    private List<File> generateWithXml(Path outputDir) {
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.JERSEY3)
                .setInputSpec(PETSTORE_SPEC)
                .setOutputDir(outputDir.toAbsolutePath().toString());
        configurator.addAdditionalProperty(JavaClientCodegen.WITH_XML, true);
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        return generator.opts(configurator.toClientOptInput()).generate();
    }

}
