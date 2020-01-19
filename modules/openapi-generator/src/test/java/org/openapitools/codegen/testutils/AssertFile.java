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

package org.openapitools.codegen.testutils;

import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;
import org.testng.Assert;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;

/**
 * Assertion for recursively testing directories.
 *
 * @author andreas
 */
public class AssertFile {

    private AssertFile() {
        throw new RuntimeException("This class should not be instantiated");
    }

    /**
     * Asserts that two directories are recursively equal. If they are not, an {@link AssertionError} is thrown with the
     * given message.<br/>
     * There will be a textual comparison of all files under expected with all files under actual. File attributes will
     * not be considered.<br/>
     * Missing or additional files are considered an error.<br/>
     *
     * @param expected Path expected directory
     * @param actual   Path actual directory
     */
    public static void assertPathEqualsRecursively(final Path expected, final Path actual) {
        Assert.assertNotNull(expected);
        Assert.assertNotNull(actual);
        final Path absoluteExpected = expected.toAbsolutePath();
        final Path absoluteActual = actual.toAbsolutePath();
        try {
            Files.walkFileTree(expected, new FileVisitor<Path>() {

                @Override
                public FileVisitResult preVisitDirectory(Path expectedDir, BasicFileAttributes attrs) throws IOException {
                    Path relativeExpectedDir = absoluteExpected.relativize(expectedDir.toAbsolutePath());
                    Path actualDir = absoluteActual.resolve(relativeExpectedDir);

                    if (!Files.exists(actualDir)) {
                        fail(String.format(Locale.ROOT,"Directory '%s' is missing.", actualDir));
                    }

                    String[] expected = expectedDir.toFile().list();
                    String[] actual = actualDir.toFile().list();

                    if (expected != null) {
                        Arrays.sort(expected);
                    }
                    if (actual != null) {
                        Arrays.sort(actual);
                    }

                    assertEquals(expected,
                            actual,
                            String.format(Locale.ROOT, "Directory content of '%s' and '%s' differ.", expectedDir, actualDir));

                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path expectedFile, BasicFileAttributes attrs) throws IOException {
                    Path relativeExpectedFile = absoluteExpected.relativize(expectedFile.toAbsolutePath());
                    Path actualFile = absoluteActual.resolve(relativeExpectedFile);

                    if (!Files.exists(actualFile)) {
                        fail(String.format(Locale.ROOT, "File '%s' is missing.", actualFile));
                    }

                    assertFilesAreEqual(expectedFile, actualFile);

                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    fail(exc.getMessage());
                    return FileVisitResult.TERMINATE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    return FileVisitResult.CONTINUE;
                }

            });
        } catch (IOException e) {
            fail(e.getMessage(), e);
        }
    }


    public static void assertFilesAreEqual(final Path expected, final Path actual) {

        if (!Files.isRegularFile(expected)) {
            fail("expected: '%s' is not a readable file");
        }

        if (!Files.isRegularFile(actual)) {
            fail("actual: '%s' is not a readable file");
        }

        try {
            List<String> expectedLines = Files.readAllLines(expected, Charset.defaultCharset());
            List<String> actualLines = Files.readAllLines(actual, Charset.defaultCharset());
            Patch diff = DiffUtils.diff(expectedLines, actualLines);
            List<Delta> deltas = diff.getDeltas();
            if (!deltas.isEmpty()) {
                StringBuilder stringBuilder = new StringBuilder();
                stringBuilder.append("files diff:\n");
                stringBuilder.append("\tfile: '").append(expected.toAbsolutePath().toString()).append("' \n");
                stringBuilder.append("\tfile: '").append(actual.toAbsolutePath().toString()).append("' \n");
                stringBuilder.append("\tdiffs:\n");

                for (Delta delta : deltas) {
                    stringBuilder.append(delta.toString()).append("\n");
                }

                fail(stringBuilder.toString());
            }

        } catch (IOException e) {
            fail(e.getMessage(), e);
        }
    }
}

