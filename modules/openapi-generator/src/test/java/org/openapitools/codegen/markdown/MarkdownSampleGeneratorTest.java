/*
package org.openapitools.codegen.markdown;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class MarkdownSampleGeneratorTest {
    private File outputTempDirectory;
    private List<File> generatedFiles;

    @BeforeClass
    public void beforeClassGenerateTestMarkup() throws Exception {
        // set line break to \n across all platforms
        System.setProperty("line.separator", "\n");

        this.outputTempDirectory = Files.createTempDirectory("test-markdown-sample-generator.").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("markdown")
                .setInputSpec("src/test/resources/3_0/markdown/issue_6096.yaml")
                .setOutputDir(outputTempDirectory.getAbsolutePath());

        DefaultGenerator generator = new DefaultGenerator();
        this.generatedFiles = generator.opts(configurator.toClientOptInput()).generate();
    }

    @Test
    public void testSampleMarkdownGeneration() throws IOException {
        Path expectedFiles = new File("src/test/resources/3_0/markdown/expected/").toPath();
        for (File generated : this.generatedFiles) {
            if (!generated.toString().endsWith(".md")) {
                continue;
            }
            Path expectedPath = this.outputTempDirectory.toPath().relativize(generated.toPath());
            File expected = expectedFiles.resolve(expectedPath).toFile();

            Assert.assertTrue(expected.exists(), "Could not find " + expected);

            Assert.assertEquals(FileUtils.readFileToString(generated, StandardCharsets.UTF_8).replace("\n", "").replace("\r", ""),
                    FileUtils.readFileToString(expected, StandardCharsets.UTF_8).replace("\n", "").replace("\r", ""));
        }
    }

}
*/
