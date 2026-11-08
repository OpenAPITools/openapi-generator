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

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class ListGeneratorsTest {

    @Test
    public void testDocsiteWrapsGeneratorLists() throws IOException, InterruptedException {
        String output = list("list", "--docsite", "--include", "all");

        assertTrue(output.contains("## CLIENT generators\n\n<div className=\"generators-list\">\n\n"
                + "* [ada](generators/ada.md)"), output);
        assertTrue(output.contains("\n\n</div>\n\n\n## SERVER generators"), output);
    }

    @Test
    public void testGithubIndexRemainsMarkdown() throws IOException, InterruptedException {
        String output = list("list", "--github-nested-index", "--include", "all");

        assertTrue(output.contains("## CLIENT generators\n* [ada](ada.md)"), output);
        assertFalse(output.contains("<div"), output);
        assertFalse(output.contains("](generators/"), output);
    }

    @DataProvider
    public Object[][] shortFormats() {
        return new Object[][]{
                {new String[]{"list", "--short", "--docsite"}},
                {new String[]{"list", "--short", "--github-nested-index"}},
                {new String[]{"list", "--short", "--docsite", "--github-nested-index"}}
        };
    }

    @Test(dataProvider = "shortFormats")
    public void testShortOutputIgnoresDocumentationFormat(String[] args) throws IOException, InterruptedException {
        assertEquals(list(args), list("list", "--short"));
    }

    @Test
    public void testDocsiteTakesPrecedenceOverGithubIndex() throws IOException, InterruptedException {
        assertEquals(list("list", "--docsite", "--github-nested-index"), list("list", "--docsite"));
    }

    private String list(String... args) throws IOException, InterruptedException {
        List<String> command = new ArrayList<>(Arrays.asList(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "-cp", System.getProperty("surefire.test.class.path", System.getProperty("java.class.path")),
                "org.openapitools.codegen.OpenAPIGenerator"));
        command.addAll(Arrays.asList(args));

        Path output = Files.createTempFile("ListGeneratorsTest", ".out");
        try {
            Process process = new ProcessBuilder(command)
                    .redirectOutput(output.toFile())
                    .redirectError(ProcessBuilder.Redirect.INHERIT)
                    .start();
            try {
                assertTrue(process.waitFor(30, TimeUnit.SECONDS), "Generator list command timed out");
                assertEquals(process.exitValue(), 0, "Generator list command failed; see stderr");
                return Files.readString(output, StandardCharsets.UTF_8).replace("\r\n", "\n");
            } finally {
                if (process.isAlive()) {
                    process.destroyForcibly().waitFor();
                }
            }
        } finally {
            Files.deleteIfExists(output);
        }
    }
}
