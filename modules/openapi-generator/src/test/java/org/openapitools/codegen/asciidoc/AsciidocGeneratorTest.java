package org.openapitools.codegen.asciidoc;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AsciidocDocumentationCodegen;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;

/** unit test asciidoc markup generation against ping.yaml openapi spec. */
public class AsciidocGeneratorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(AsciidocGeneratorTest.class);

    @Test
    public void testPingSpecTitle() throws Exception {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");

        AsciidocDocumentationCodegen codeGen = new AsciidocDocumentationCodegen();
        codeGen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(openAPI.getInfo().getTitle(), "ping test");
    }

    @Test
    public void testGenerateIndexAsciidocMarkupFileWithAsciidocGenerator() throws Exception {

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/ping.yaml").setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SNIPPET_DIR, "MY-SNIPPET-DIR")
                .addAdditionalProperty(AsciidocDocumentationCodegen.SPEC_DIR, "MY-SPEC-DIR");

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        List<File> generatedFiles = generator.opts(clientOptInput).generate();
        TestUtils.ensureContainsFile(generatedFiles, output, "index.adoc");
    }

    @Test
    public void testGenerateIndexAsciidocMarkupContent() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/ping.yaml");
        CodegenConfig codegenConfig = new AsciidocDocumentationCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());
        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean markupFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
                markupFileGenerated = true;
                String markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                // check on some basic asciidoc markup content
                Assert.assertTrue(markupContent.contains("= ping test"),
                        "expected = header in: " + markupContent.substring(0, 50));
                Assert.assertTrue(markupContent.contains(":toc: "),
                        "expected = :toc: " + markupContent.substring(0, 50));
            }
        }
        Assert.assertTrue(markupFileGenerated, "Default api file is not generated!");
    }

    @Test
    public void testAdditionalDirectoriesGeneratedIntoHeaderAttributes() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        LOGGER.info("test: generating sample markup " + output.getAbsolutePath());

        Map<String, Object> props = new TreeMap<String, Object>();
        props.put("specDir", "spec");

        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/ping.yaml").setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SPEC_DIR, "SPEC-DIR")
                .addAdditionalProperty(AsciidocDocumentationCodegen.SNIPPET_DIR, "MY/SNIPPET/DIR");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        boolean markupFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
                markupFileGenerated = true;
                String markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                Assert.assertTrue(markupContent.contains(":specDir: SPEC-DIR"),
                        "expected :specDir: in: " + markupContent.substring(0, 250));
                Assert.assertTrue(markupContent.contains(":snippetDir: MY/SNIPPET/DIR"),
                        "expected :snippetDir: in: " + markupContent.substring(0, 250));
            }
        }
        Assert.assertTrue(markupFileGenerated, "index.adoc is not generated!");

    }
    

    @Test
    public void testHeaderAttributesFlagRemovesAttributesFromMarkupHeaderSection() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        LOGGER.info("test: generating sample markup " + output.getAbsolutePath());

        Map<String, Object> props = new TreeMap<String, Object>();
        props.put("specDir", "spec");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty(AsciidocDocumentationCodegen.HEADER_ATTRIBUTES_FLAG, "false")    // option avoids generation of attributes
                .addAdditionalProperty(AsciidocDocumentationCodegen.SPEC_DIR, "SPEC-DIR")
                .addAdditionalProperty(AsciidocDocumentationCodegen.SNIPPET_DIR, "MY/SNIPPET/DIR");

        DefaultGenerator generator = new DefaultGenerator();
        boolean markupFileGenerated = false;
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
                markupFileGenerated = true;
                String markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                Assert.assertFalse(markupContent.contains(":specDir: SPEC-DIR"),
                        "not expected :specDir: in: " + markupContent.substring(0, 250));
                Assert.assertFalse(markupContent.contains(":snippetDir: MY/SNIPPET/DIR"),
                        "not expected :snippetDir: in: " + markupContent.substring(0, 250));
                Assert.assertFalse(markupContent.contains(":toc:"),
                        "not expected :toc: in: " + markupContent.substring(0, 250));               // typical attributes not found in markup.
            }
        }
        Assert.assertTrue(markupFileGenerated, "index.adoc is not generated!");
    }    

}
