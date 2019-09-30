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
import org.testng.annotations.Test;

public class AsciidocSampleGeneratorTest {

    /** 
     * ensure api-docs.json includes sample and spec files into markup.
     * @throws Exception generic exception
     */
    @Test
    public void testSampleAsciidocMarkupGenerationFromJsonWithSpecsAndSamples() throws Exception {

        File outputTempDirectory = Files.createTempDirectory("test-asciidoc-sample-generator.").toFile();

        File specDir = new File("src/test/resources/3_0/asciidoc/specs/");
        File snippetDir = new File("src/test/resources/3_0/asciidoc/generated-snippets/");

        Assert.assertTrue(specDir.exists(), "test cancel, not specdDir found to use." + specDir.getPath());
        Assert.assertTrue(snippetDir.exists(), "test cancel, not snippedDir found to use." + snippetDir.getPath());

        final CodegenConfigurator configurator = new CodegenConfigurator().setGeneratorName("asciidoc")
                .setInputSpec("src/test/resources/3_0/asciidoc/api-docs.json")
                .setOutputDir(outputTempDirectory.getAbsolutePath())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SPEC_DIR, specDir.toString())
                .addAdditionalProperty(AsciidocDocumentationCodegen.SNIPPET_DIR, snippetDir.toString());

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();

        boolean markupFileGenerated = false;

        for (File file : files) {
            if (file.getName().equals("index.adoc")) {
                markupFileGenerated = true;
                String markupContent = FileUtils.readFileToString(file, StandardCharsets.UTF_8);

                // include correct values from cli.
                Assert.assertTrue(markupContent.contains(":specDir: " + specDir.toString()),
                        "expected :specDir: in: " + markupContent.substring(0, 350));
                Assert.assertTrue(markupContent.contains(":snippetDir: " + snippetDir.toString()),
                        "expected :snippetDir: in: " + markupContent.substring(0, 350));

                // include correct markup from separate directories, relative links
                Assert.assertTrue(markupContent.contains("include::rest/project/GET/spec.adoc[]"),
                        "expected project spec.adoc to be included in " + file.getAbsolutePath());

                Assert.assertTrue(markupContent.contains("include::rest/project/GET/implementation.adoc[]"),
                        "expected project implementation.adoc to be included in " + file.getAbsolutePath());

                Assert.assertTrue(markupContent.contains("include::rest/project/GET/http-request.adoc[]"),
                        "expected project http-request.adoc to be included in " + file.getAbsolutePath());

                Assert.assertTrue(markupContent.contains("include::rest/project/GET/http-response.adoc[]"),
                        "expected project http-response.adoc to be included in " + file.getAbsolutePath());

                Assert.assertTrue(markupContent.contains("link:rest/project/GET/GET.json["),
                        "expected link: not found in file: " + file.getAbsoluteFile());

                // extract correct value from json
                Assert.assertTrue(markupContent.contains("= time@work rest api"),
                        "missing main header for api spec from json: " + markupContent.substring(0, 100));
            }
            Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
        }

        Assert.assertTrue(markupFileGenerated, "index.adoc is not generated!");

        Files.deleteIfExists(Paths.get(outputTempDirectory.getAbsolutePath(), ".openapi-generator"));
        Files.deleteIfExists(Paths.get(outputTempDirectory.getAbsolutePath()));
    }

}
