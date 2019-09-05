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

public class LinkMarkupFilterTest extends LambdaTest {

    @BeforeMethod
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testLinkMarkupFilterDoesNotLinkMissingFile() {

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("link", generator.new LinkMarkupLambda("DOES_NOT_EXIST"));

        final String result = execute("{{#link}}not.an.existing.file.adoc{{/link}}", ctx);
        Assert.assertTrue(result.contains("// file not found, no"), "unexpected filtered: " + result);
    }

    @Test
    public void testLinkMarkupFilterLinksFoundFileOk() throws IOException {

        File tempFile = File.createTempFile("LinkMarkupFilterTestDummyfile", ".adoc");
        tempFile.deleteOnExit();

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("linkIntoMarkup", generator.new LinkMarkupLambda(tempFile.getParent()));

        final String result = execute("{{#linkIntoMarkup}}my link text, " + tempFile.getName() + "{{/linkIntoMarkup}}",
                ctx);
        Assert.assertTrue(result.contains("link:"), "unexpected filtered: " + result);
        Assert.assertTrue(result.contains(tempFile.getName() + "[]"), "unexpected filtered: " + result);
    }

}
