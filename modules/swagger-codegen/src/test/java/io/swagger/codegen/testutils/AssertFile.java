package io.swagger.codegen.testutils;

import org.testng.Assert;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;

import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;

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
                        fail(String.format("Directory '%s' is missing.", actualDir));
                    }

                    assertEquals(expectedDir.toFile().list(),
                                 actualDir.toFile().list(),
                                 String.format("Directory content of '%s' and '%s' differ.", expectedDir, actualDir));

                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path expectedFile, BasicFileAttributes attrs) throws IOException {
                    Path relativeExpectedFile = absoluteExpected.relativize(expectedFile.toAbsolutePath());
                    Path actualFile = absoluteActual.resolve(relativeExpectedFile);

                    if (!Files.exists(actualFile)) {
                        fail(String.format("File '%s' is missing.", actualFile));
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

        if(!Files.isRegularFile(expected)) {
            fail("expected: '%s' is not a readable file");
        }

        if(!Files.isRegularFile(actual)) {
            fail("actual: '%s' is not a readable file");
        }

        try {
            List<String> expectedLines = Files.readAllLines(expected, Charset.defaultCharset());
            List<String> actualLines = Files.readAllLines(actual, Charset.defaultCharset());
            Patch diff = DiffUtils.diff(expectedLines, actualLines);
            List<Delta> deltas = diff.getDeltas();
            if(!deltas.isEmpty()) {
                StringBuilder stringBuilder = new StringBuilder();
                stringBuilder.append("files diff:\n");
                stringBuilder.append("\tfile: '" + expected.toAbsolutePath().toString() + "' \n");
                stringBuilder.append("\tfile: '" + actual.toAbsolutePath().toString() + "' \n");
                stringBuilder.append("\tdiffs:\n");

                for (Delta delta: deltas) {
                    stringBuilder.append(delta.toString() + "\n");
                }

                fail(stringBuilder.toString());
            }

        } catch (IOException e) {
            fail(e.getMessage(), e);
        }
    }
}

