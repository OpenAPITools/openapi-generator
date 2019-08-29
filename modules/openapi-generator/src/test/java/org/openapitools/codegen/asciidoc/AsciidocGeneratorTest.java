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
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.OpenAPI;

public class AsciidocGeneratorTest {
	
 
	@Test
    public void testSpecSchema() throws Exception {
        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/ping.yaml");

        AsciidocDocumentationCodegen codeGen = new AsciidocDocumentationCodegen();
        codeGen.preprocessOpenAPI(openAPI);

        Assert.assertEquals(openAPI.getInfo().getTitle(), "ping test");
    }
	
    @Test
    public void testGenerateMarkupFileWithAsciidocGenerator() throws Exception {

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("snippetDir", "MY-SNIPPET-DIR")
                .addAdditionalProperty("specDir", "MY-SPEC-DIR");

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        TestUtils.ensureContainsFile(generatedFiles, output, "index.adoc");
    }
    
    @Test
    public void testGenerateAsciidocMarkupContent() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_0/ping.yaml");
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
                Assert.assertTrue(markupContent.contains("= ping test"), "expected = header in: " + markupContent.substring(0, 50));
                Assert.assertTrue(markupContent.contains(":toc: "), "expected = :toc: " + markupContent.substring(0, 50));
            }
        }
        Assert.assertTrue(markupFileGenerated, "Default api file is not generated!");
    }

    
    @Test
    public void testAdditionalDirectoriesGeneratoreHeaderAttributes() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        Map<String, Object> props = new TreeMap<String, Object>();
        props.put("specDir", "spec");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("specDir", "SPEC-DIR")
        		.addAdditionalProperty("snippetDir", "MY/SNIPPET/DIR");

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        boolean markupFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
            	markupFileGenerated = true;
                String markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                Assert.assertTrue(markupContent.contains(":specDir: SPEC-DIR"), "expected :specDir: in: " + markupContent.substring(0, 250));
                Assert.assertTrue(markupContent.contains(":snippetDir: MY/SNIPPET/DIR"), "expected :snippetDir: in: " + markupContent.substring(0, 250));
            }
        }
        Assert.assertTrue(markupFileGenerated, "index.adoc is not generated!");

    }

}
