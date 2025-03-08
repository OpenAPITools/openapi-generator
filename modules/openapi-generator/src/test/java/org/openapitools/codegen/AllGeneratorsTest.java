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
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

public class AllGeneratorsTest {

    @DataProvider(name = "generators")
    Iterator<CodegenConfig> generators() {
        return CodegenConfigLoader.getAll().iterator();
    }

    @Test(dataProvider = "generators")
    public void testEachWithPetstore(CodegenConfig codegenConfig) {
        try {
            File output = Files.createTempDirectory("test").toFile();
            output.deleteOnExit();

            final CodegenConfigurator configurator = new CodegenConfigurator()
                    .setGeneratorName(codegenConfig.getName())
                    .setInputSpec("src/test/resources/3_0/petstore.yaml")
                    .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

            List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();

            // Main intention of this test is to check that nothing crashes. Besides, we check here that
            // at least 1 file is generated, besides the common ".openapi-generator-ignore", "FILES" and "VERSION" files.
            assertThat(files).hasSizeGreaterThanOrEqualTo(4);
        } catch (Exception e) {
            throw new RuntimeException(
                    String.format(
                            Locale.ROOT,
                            "Generator %s threw an exception (generating %s): %s",
                            codegenConfig.getName(), codegenConfig.getInputSpec(), e.getMessage()
                    ), e
            );
        }
    }

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/18810">#18810</a>
     */
    @Test(dataProvider = "generators")
    void noDuplicateCliOptions(CodegenConfig codegenConfig) {
        final List<String> cliOptionKeys = codegenConfig.cliOptions()
                .stream().map(CliOption::getOpt).collect(Collectors.toList());

        assertThat(cliOptionKeys).allSatisfy(
                opt -> assertThat(cliOptionKeys)
                        .as("Generator '%s' defines CliOption '%s' more than once!", codegenConfig.getName(), opt)
                        .containsOnlyOnce(opt)
        );
    }

    @Test(dataProvider = "generators")
    void noDuplicateSupportedLibraries(CodegenConfig codegenConfig) {
        final var supportedLibraries = codegenConfig.supportedLibraries().keySet();

        assertThat(supportedLibraries).allSatisfy(
                lib -> assertThat(supportedLibraries)
                        .as("Generator '%s' defines '%s' more than once in supportedLibraries!", codegenConfig.getName(), lib)
                        .containsOnlyOnce(lib)
        );
    }

    @Test(dataProvider = "generators")
    void noDuplicateSupportingFiles(CodegenConfig codegenConfig) {
        final List<String> supportingFiles = codegenConfig.supportingFiles()
                .stream().map(SupportingFile::toString).collect(Collectors.toList());

        assertThat(supportingFiles).allSatisfy(
                file -> assertThat(supportingFiles)
                        .as("Generator '%s' defines '%s' more than once in supportingFiles!", codegenConfig.getName(), file)
                        .containsOnlyOnce(file)
        );
    }

    @Test(dataProvider = "generators")
    void noDuplicateSupportedVendorExtensions(CodegenConfig codegenConfig) {
        final List<VendorExtension> supportedVendorExtensions = codegenConfig.getSupportedVendorExtensions();

        assertThat(supportedVendorExtensions).allSatisfy(
                extension -> assertThat(supportedVendorExtensions)
                        .as("Generator '%s' defines '%s' more than once in supportedVendorExtensions!", codegenConfig.getName(), extension)
                        .containsOnlyOnce(extension)
        );
    }
}
