package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import org.testng.annotations.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class IndentedLambdaTest extends LambdaTest {

    String lineSeparator = "\n";

    @Test
    public void defaultIndentTest() {
        // Given
        Map<String, Object> ctx = context("indented", new IndentedLambda());

        // When & Then
        // IndentedLambda applies indentation from second line on of a template.
        test("first line" + lineSeparator + "    second line",
                "{{#indented}}first line" + lineSeparator + "second line{{/indented}}", ctx);
    }

    @Test
    public void indentedCountTest() {
        // Given
        Map<String, Object> ctx = context("indented", new IndentedLambda(8, " ", false, false));

        // When & Then
        // IndentedLambda applies indentation from second line on of a template.
        test("first line" + lineSeparator + "        second line",
                "{{#indented}}first line" + lineSeparator + "second line{{/indented}}", ctx);
    }

    @Test
    public void lineBreaksPreservedTest() {
        Map<String, Object> ctx = context("indented", new IndentedLambda(4, " ", true, true));

        String actual = execute(Mustache.compiler(), "{{#indented}}first line\nsecond line\n\nthird line\n\n\nfourth line\n\n{{/indented}}", ctx);
        String expected = "    first line\n    second line\n\n    third line\n\n\n    fourth line\n\n";

        assertEquals(expected, actual);
    }
}
