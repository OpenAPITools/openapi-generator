package io.swagger.codegen.ignore;

import org.apache.commons.io.FileUtils;
import org.testng.annotations.*;
import static org.testng.Assert.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class CodegenIgnoreProcessorTest {

    private Boolean allowed;
    private final String description;
    private String outputDir;
    private File target;
    private Path temp;

    private CodegenIgnoreProcessorTest(String filename, String ignoreDefinition, String description) throws IOException {
        this.description = description;

        temp = Files.createTempDirectory(null);
        this.outputDir = temp.toFile().getAbsolutePath();

        target = new File(this.outputDir, filename);

        boolean mkdirs = target.getParentFile().mkdirs();
        if(!mkdirs) {
            throw new IOException("Failed to create parent directories for CodegenIgnoreProcessorTest test file.");
        }

        Path created = Files.createFile(target.toPath());
        if(!created.toFile().exists()) {
            throw new IOException("Failed to write CodegenIgnoreProcessorTest test file.");
        }

        // System.out.print(String.format("Created codegen ignore processor test file: %s\n", created.toAbsolutePath()));
        File ignoreFile = new File(this.outputDir, ".swagger-codegen-ignore");
        try (FileOutputStream stream = new FileOutputStream(ignoreFile)) {
            stream.write(ignoreDefinition.getBytes());
        }
    }

    CodegenIgnoreProcessorTest allowed() {
        this.allowed = true;
        return this;
    }

    CodegenIgnoreProcessorTest ignored() {
        this.allowed = false;
        return this;
    }

    @AfterClass
    public void afterClass() throws IOException {
        FileUtils.deleteDirectory(temp.toFile());
    }

    @Test
    public void evaluate() {
        // Arrange
        CodegenIgnoreProcessor processor = new CodegenIgnoreProcessor(outputDir);
        Boolean actual = null;

        // Act
        actual = processor.allowsFile(target);

        // Assert
        assertEquals(actual, this.allowed, this.description);
    }

    @Factory
    public static Object[] factoryMethod() throws IOException {
        return new Object[] {
                // Matching filenames
                new CodegenIgnoreProcessorTest("build.sh", "build.sh", "A file when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("src/build.sh", "**/build.sh", "A file when matching nested files should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("Build.sh", "build.sh", "A file when non-matching should allow.").allowed(),
                new CodegenIgnoreProcessorTest("build.sh", "/build.sh", "A rooted file when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("nested/build.sh", "/build.sh", "A rooted file definition when non-matching should allow.").allowed(),
                new CodegenIgnoreProcessorTest("src/IO.Swagger.Test/Model/AnimalFarmTests.cs", "src/IO.Swagger.Test/Model/AnimalFarmTests.cs", "A file when matching exactly should ignore.").ignored(),

                // Matching spaces in filenames
                new CodegenIgnoreProcessorTest("src/properly escaped.txt", "**/properly escaped.txt", "A file when matching nested files with spaces in the name should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("src/improperly escaped.txt", "**/improperly\\ escaped.txt", "A file when matching nested files with spaces in the name (improperly escaped rule) should allow.").allowed(),

                // Match All
                new CodegenIgnoreProcessorTest("docs/somefile.md", "docs/**", "A recursive file (0 level) when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/somefile.md", "docs/**", "A recursive file (1 level) when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/somefile.md", "docs/**", "A recursive file (n level) when matching should ignore.").ignored(),

                // Match Any
                new CodegenIgnoreProcessorTest("docs/1/2/3/somefile.md", "docs/**/somefile.*", "A recursive file with match-any extension when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/somefile.java", "docs/**/*.java", "A recursive file with match-any file name when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/4/somefile.md", "docs/**/*", "A recursive file with match-any file name when matching should ignore.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/4/5/somefile.md", "docs/**/anyfile.*", "A recursive file with match-any extension when non-matching should allow.").allowed(),

                // Directory matches
                new CodegenIgnoreProcessorTest("docs/1/Users/a", "docs/**/Users/", "A directory rule when matching should be ignored.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/Users1/a", "docs/**/Users/", "A directory rule when non-matching should be allowed.").allowed(),

                // Negation of excluded recursive files
                new CodegenIgnoreProcessorTest("docs/UserApi.md", "docs/**\n!docs/UserApi.md", "A pattern negating a previous ignore FILE rule should be allowed.").allowed(),

                // Negation of excluded directories
                new CodegenIgnoreProcessorTest("docs/1/Users/UserApi.md", "docs/**/Users/\n!docs/1/Users/UserApi.md", "A pattern negating a previous ignore DIRECTORY rule should be ignored.").ignored(),

                // Other matches which may not be parsed for correctness, but are free because of PathMatcher
                new CodegenIgnoreProcessorTest("docs/1/2/3/Some99File.md", "**/*[0-9]*", "A file when matching against simple regex patterns when matching should be ignored.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/SomeFile.md", "**/*.{java,md}", "A file when matching against grouped subpatterns for extension when matching (md) should be ignored.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/SomeFile.java", "**/*.{java,md}", "A file when matching against grouped subpatterns for extension when matching (java) should be ignored.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/SomeFile.txt", "**/*.{java,md}", "A file when matching against grouped subpatterns for extension when non-matching should be allowed.").allowed(),

                new CodegenIgnoreProcessorTest("docs/1/2/3/foo.c", "**/*.?", "A file when matching against required single-character extension when matching should be ignored.").ignored(),
                new CodegenIgnoreProcessorTest("docs/1/2/3/foo.cc", "**/*.?", "A file when matching against required single-character extension when non-matching should be allowed.").allowed()

        };
    }
}