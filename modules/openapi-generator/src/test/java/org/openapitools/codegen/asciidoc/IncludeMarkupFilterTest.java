package org.openapitools.codegen.asciidoc;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
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
        final Map<String, Object> ctx = context("specinclude", generator.new IncludeMarkupLambda("specDir","DOES_NOT_EXIST"));

        final String result = execute("{{#specinclude}}not.an.existing.file.adoc{{/specinclude}}", ctx);
        Assert.assertTrue(result.contains("// markup not found, no include::{specDir}not.an.existing.file.adoc[opts=optional]"),
                "unexpected filtered " + result);
    }

    @Test
    public void testIncludeMarkupFilterFoundFileOk() throws IOException {

        File tempFile = Files.createTempFile("IncludeMarkupFilterTestDummyfile", "-adoc").toFile();
        tempFile.deleteOnExit();

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("snippetinclude",
                generator.new IncludeMarkupLambda("specDir",tempFile.getParent()));

        final String result = execute("{{#snippetinclude}}" + tempFile.getName() + "{{/snippetinclude}}", ctx);
        Assert.assertTrue(result.contains("include::{specDir}"+tempFile.getName()+"[opts=optional]"), "unexpected filtered: " + result);
    }

    @Test
    public void testIncludeMarkupFilterEscapeCurlyBracketsInOrderToBeParsedByAsciidoc() throws IOException {
        String temporaryPath = Files.createTempDirectory(null).toFile().getAbsolutePath();
        String pathWithCurlyBrackets = temporaryPath + "/{parameter1}/{parameter2}";
        File folderWithCurlyBrackets = new File(pathWithCurlyBrackets);
        folderWithCurlyBrackets.mkdirs();

        File tempFile =  File.createTempFile("curly", "-adoc", folderWithCurlyBrackets);
        tempFile.deleteOnExit();

        final AsciidocDocumentationCodegen generator = new AsciidocDocumentationCodegen();
        final Map<String, Object> ctx = context("snippetinclude",
                generator.new IncludeMarkupLambda("specDir",temporaryPath));

        final String result = execute("{{#snippetinclude}}" + "/{parameter1}/{parameter2}/"+tempFile.getName() + "{{/snippetinclude}}", ctx);
        Assert.assertEquals(result,"\ninclude::{specDir}"+ "\\{parameter1\\}/\\{parameter2\\}/" + tempFile.getName()+"[opts=optional]\n");
    }

}
