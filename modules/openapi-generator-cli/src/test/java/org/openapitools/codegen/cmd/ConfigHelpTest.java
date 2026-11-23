/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.cmd;

import io.airlift.airline.Cli;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class ConfigHelpTest {

    @DataProvider
    public Object[][] generatorsWithTypeBraces() {
        return new Object[][]{
                {"julia-client", "Vector{UInt8}", "<li>Vector&#123;UInt8&#125;</li>"},
                {"julia-server", "Vector{UInt8}", "<li>Vector&#123;UInt8&#125;</li>"},
                {"go", "interface{}", "<li>interface&#123;&#125;</li>"}
        };
    }

    @Test(dataProvider = "generatorsWithTypeBraces")
    public void testMarkdownEscapesTypeBraces(String generatorName, String type, String markdownItem) throws IOException {
        String output = configHelp(generatorName, "markdown");

        assertTrue(output.contains(markdownItem), output);
    }

    @Test(dataProvider = "generatorsWithTypeBraces")
    public void testPlainTextPreservesTypeBraces(String generatorName, String type, String markdownItem) throws IOException {
        String output = configHelp(generatorName, "text");

        assertTrue(output.contains(type), output);
        assertFalse(output.contains("&#123;"), output);
        assertFalse(output.contains("&#125;"), output);
    }

    private String configHelp(String generatorName, String format) throws IOException {
        Path output = Files.createTempFile("ConfigHelpTest", ".out");
        try {
            Cli.<Runnable>builder("openapi-generator-cli")
                    .withCommands(ConfigHelp.class)
                    .build()
                    .parse("config-help", "-g", generatorName, "--full-details", "--format", format,
                            "-o", output.toString())
                    .run();

            return Files.readString(output, StandardCharsets.UTF_8);
        } finally {
            Files.deleteIfExists(output);
        }
    }
}
