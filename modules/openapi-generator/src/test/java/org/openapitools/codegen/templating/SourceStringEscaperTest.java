package org.openapitools.codegen.templating;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class SourceStringEscaperTest {
    @Test
    public void javaLiteralPreservesControlCharactersAndBackslashes() {
        assertEquals(SourceStringEscaper.javaStringLiteral("quote \" slash \\ line\nliteral\\n"),
                "\"quote \\\" slash \\\\ line\\nliteral\\\\n\"");
    }

    @Test
    public void kotlinLiteralEscapesInterpolationCharacters() {
        assertEquals(SourceStringEscaper.kotlinStringLiteral("$name ${value} \" \\ \t"),
                "\"\\$name \\${value} \\\" \\\\ \\t\"");
    }

    @Test
    public void kotlinLiteralUsesUnicodeEscapeForFormFeed() {
        assertEquals(SourceStringEscaper.kotlinStringLiteral("form\ffeed"),
                "\"form\\u000cfeed\"");
    }

    @Test
    public void documentationProtectsCommentDelimitersUnicodeEscapesAndLiteralHtml() {
        assertEquals(SourceStringEscaper.docText("first\n/* data */ \\u002a/ &amp; <tag>\nlast"),
                "first\n * &#47;* data *&#47; &#92;u002a/ &amp;amp; &lt;tag&gt;\n * last");
    }
}
