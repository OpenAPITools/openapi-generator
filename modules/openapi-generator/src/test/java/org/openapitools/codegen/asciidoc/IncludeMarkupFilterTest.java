package org.openapitools.codegen.asciidoc;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.mockito.MockitoAnnotations;
import org.openapitools.codegen.languages.AsciidocDocumentationCodegen;
import org.openapitools.codegen.templating.mustache.LambdaTest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import org.testng.Assert;

public class IncludeMarkupFilterTest extends LambdaTest {

    @BeforeMethod
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testIncludeMarkupFilterDoesNotIncludeMissingFile() {

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("specinclude", generator.new IncludeMarkupLambda("DOES_NOT_EXIST"));

        final String result = execute("{{#specinclude}}not.an.existing.file.adoc{{/specinclude}}", ctx);
        Assert.assertTrue(result.contains("// markup not found, no include ::not.an.existing.file.adoc["),
                "unexpected filtered " + result);
    }

    @Test
    public void testIncludeMarkupFilterFoundFileOk() throws IOException {

        File tempFile = File.createTempFile("IncludeMarkupFilterTestDummyfile", "-adoc");
        tempFile.deleteOnExit();

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("snippetinclude",
                generator.new IncludeMarkupLambda(tempFile.getParent()));

        final String result = execute("{{#snippetinclude}}" + tempFile.getName() + "{{/snippetinclude}}", ctx);
        Assert.assertTrue(result.contains("include::"), "unexpected filtered: " + result);
        Assert.assertTrue(result.contains(tempFile.getName()), "unexpected filtered: " + result);
    }

}
