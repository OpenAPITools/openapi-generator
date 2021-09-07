package org.openapitools.codegen.asciidoc;

import static org.testng.Assert.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/** checks if markdown is converted to asciidoc */
public class AsciidocMarkdownConvertTest {
    public String markupContent = null;
    public String markupFileName = null;

    @BeforeClass
    public void beforeClassGenerateTestMarkup() throws Exception {
        File outputTempDirectory = Files.createTempDirectory("test-asciidoc-markdown-generator.").toFile();

        CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/asciidoc/api-docs-markdown.yaml")
                .setOutputDir(outputTempDirectory.getAbsolutePath());

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
                this.markupFileName = file.getAbsoluteFile().toString();
                this.markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
            }
        }
    }

    @AfterClass
    public void afterClassCleanUpTestMarkup() throws Exception {
        if (this.markupFileName != null) {
            Files.deleteIfExists(Paths.get(this.markupFileName));
        }
    }

    /**
     * Source:
     * 
     * <pre>
     * this library can be used by:
     *
     * 1. someone who need it
     * 2. everybody else
     * </pre>
     */
    @Test
    public void testMarkDownInInfoDescription() {
        assertContent("this library can be used by:\n\n. someone who need it\n. everybody else", "info description");
    }

    /**
     * Source:
     * 
     * <pre>
     * *dummy* application info endpoint.
     * </pre>
     */
    @Test
    public void testMarkDownInPathSummary() {
        assertContent("_dummy_ application info endpoint.", "path summary");
    }

    /**
     * Source:
     * 
     * <pre>
     * some *notes* with special ><& char
     * </pre>
     */
    @Test
    public void testMarkDownInPathDescription() {
        assertContent("some _notes_ with special ><& chars", "path description");
    }

    /**
     * Source:
     * 
     * <pre>
     * the **default** response
     * </pre>
     */
    @Test
    public void testMarkDownInPathResponseDescription() {
        assertContent("the *default* response", "path response description");
    }

    /**
     * Source:
     * 
     * <pre>
     * oh, ``you`` should care about it
     * </pre>
     */
    @Test
    public void testMarkDownInComponentsSchemasDescription() {
        assertContent("oh, `you` should care about it", "component schema description");
    }

    /**
     * Source:
     * 
     * <pre>
     * should be *very* secure
     * </pre>
     */
    @Test
    public void testMarkDownInComponentSchemaPropertyDescription() {
        assertContent("should be _very_ secure", "component schema property description");
    }

    private void assertContent(String expected, String field) {
        assertTrue(markupContent.contains(expected), "expected " + field + " to be converted to be '" + expected + "'");
    }
}
