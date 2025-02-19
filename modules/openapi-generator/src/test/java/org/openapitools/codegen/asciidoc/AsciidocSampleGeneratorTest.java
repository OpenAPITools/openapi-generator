package org.openapitools.codegen.asciidoc;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.FileUtils;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AsciidocDocumentationCodegen;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/** several asciidoc content checks with sample openapi v3. */
public class AsciidocSampleGeneratorTest {

    public String markupContent = null;
    public String markupFileName = null;

    File specDir = new File("src/test/resources/3_0/asciidoc/specs/");
    File snippetDir = new File("src/test/resources/3_0/asciidoc/generated-snippets/");

    @BeforeClass
    public void beforeClassGenerateTestMarkup() throws Exception {

        File outputTempDirectory = Files.createTempDirectory("test-asciidoc-sample-generator.").toFile();

        Assert.assertTrue(specDir.exists(), "test cancel, not specDir found to use." + specDir.getPath());
        Assert.assertTrue(snippetDir.exists(), "test cancel, not snippedDir found to use." + snippetDir.getPath());

        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/asciidoc/api-docs.json")
                .setOutputDir(outputTempDirectory.getAbsolutePath())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SPEC_DIR, specDir.toString())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SNIPPET_DIR, snippetDir.toString());

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

    @Test
    public void testMarkupExistence() {
        Assert.assertNotNull(this.markupContent, "asciidoc content index.adoc not created.");
    }

    /**
     * ensure api-docs.json includes sample and spec files directory as attributes.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationFromJsonWithAttributes() {
        Assert.assertTrue(markupContent.contains(":specDir: " + specDir.toString()),
                "expected :specDir: in: " + markupContent.substring(0, 350));
        Assert.assertTrue(markupContent.contains(":snippetDir: " + snippetDir.toString()),
                "expected :snippetDir: in: " + markupContent.substring(0, 350));
    }

    /**
     * ensure api-docs.json includes sample and spec files into markup.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationFromJsonWithIncludes() {

        // include correct markup from separate directories, relative links
        Assert.assertTrue(markupContent.contains("include::{specDir}rest/project/GET/spec.adoc["),
                "expected project spec.adoc to be included in " + markupFileName);

        Assert.assertTrue(markupContent.contains("include::{specDir}rest/project/GET/implementation.adoc["),
                "expected project implementation.adoc to be included in " + markupFileName);

        Assert.assertTrue(markupContent.contains("include::{snippetDir}rest/project/GET/http-request.adoc["),
                "expected project http-request.adoc to be included in " + markupFileName);

        Assert.assertTrue(markupContent.contains("include::{snippetDir}rest/project/GET/http-response.adoc["),
                "expected project http-response.adoc to be included in " + markupFileName);

        Assert.assertTrue(markupContent.contains("link:rest/project/GET/GET.json["),
                "expected link: not found in file: " + markupFileName);
    }

    /**
     * markup doc header content.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationFromJsonWithContent() {
        Assert.assertTrue(markupContent.contains("= time@work rest api"),
                "missing main header for api spec from json: " + markupContent.substring(0, 100));

    }

    /**
     * fix: parameter name unchanged.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationParameterNameUnchanged() {
        Assert.assertTrue(markupContent.contains("from-iso-date-string"),
                "keep parameter name from-iso-date-string unchanged.");
    }

    /**
     * added apikey info in access section.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationAccessApiKey() {
        Assert.assertTrue(markupContent.contains("*APIKey*"),
                "access section mit apikey expected.");
        Assert.assertFalse(markupContent.contains("*OAuth*"),
                "access section no oauth expected.");
        Assert.assertFalse(markupContent.contains("*HTTP Basic*"),
                "access section no http basic expected.");
    }

    /**
     * no form params in this sample spec.
     */
    @Test
    public void testSampleAsciidocMarkupGenerationWithoutFormParameter() {
        Assert.assertFalse(markupContent.contains("= Form Parameter"),
                "no form parameters in this openapi spec expected.");
    }

}
