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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class ListGeneratorsTest {

    @Test
    public void testDocsiteWrapsGeneratorLists() {
        String output = list("list", "--docsite", "--include", "all");

        assertTrue(output.contains("## CLIENT generators\n\n<div className=\"generators-list\">\n\n"
                + "* [ada](generators/ada.md)"), output);
        assertTrue(output.contains("\n\n</div>\n\n\n## SERVER generators"), output);
    }

    @Test
    public void testGithubIndexRemainsMarkdown() {
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
    public void testShortOutputIgnoresDocumentationFormat(String[] args) {
        assertEquals(list(args), list("list", "--short"));
    }

    @Test
    public void testDocsiteTakesPrecedenceOverGithubIndex() {
        assertEquals(list("list", "--docsite", "--github-nested-index"), list("list", "--docsite"));
    }

    private String list(String... args) {
        PrintStream original = System.out;
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (PrintStream captured = new PrintStream(output, true, StandardCharsets.UTF_8)) {
            System.setOut(captured);
            Cli.<Runnable>builder("openapi-generator-cli")
                    .withCommand(ListGenerators.class)
                    .build()
                    .parse(args)
                    .run();
        } finally {
            System.setOut(original);
        }
        return new String(output.toByteArray(), StandardCharsets.UTF_8).replace("\r\n", "\n");
    }
}
