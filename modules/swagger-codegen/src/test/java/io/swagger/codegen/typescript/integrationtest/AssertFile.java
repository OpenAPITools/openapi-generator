package io.swagger.codegen.typescript.integrationtest;

import org.testng.Assert;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;

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
     * There will be a binary comparison of all files under expected with all files under actual. File attributes will
     * not be considered.<br/>
     * Missing or additional files are considered an error.<br/>
     *
     * @param expected Path expected directory
     * @param actual   Path actual directory
     */
    public static final void assertPathEqualsRecursively(final Path expected, final Path actual) {
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
                        fail(String.format("Directory \'%s\' missing in target.", expectedDir.getFileName()));
                    }

                    assertEquals(expectedDir.toFile().list().length,
                                        actualDir.toFile().list().length,
                                        String.format("Directory size of \'%s\' differ. ", relativeExpectedDir));

                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path expectedFile, BasicFileAttributes attrs) throws IOException {
                    Path relativeExpectedFile = absoluteExpected.relativize(expectedFile.toAbsolutePath());
                    Path actualFile = absoluteActual.resolve(relativeExpectedFile);

                    if (!Files.exists(actualFile)) {
                        fail(String.format("File \'%s\' missing in target.", expectedFile.getFileName()));
                    }
                    assertEquals(Files.readAllLines(expectedFile, Charset.defaultCharset()),
                                        Files.readAllLines(actualFile, Charset.defaultCharset()),
                                        String.format("File content of \'%s\' differ. ", relativeExpectedFile));

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
            fail(e.getMessage());
        }
    }

}

