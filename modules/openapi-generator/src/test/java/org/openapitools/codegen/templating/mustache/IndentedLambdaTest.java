package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

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
        Map<String, Object> ctx = context("indented", new IndentedLambda(8, " ", false));

        // When & Then
        // IndentedLambda applies indentation from second line on of a template.
        test("first line" + lineSeparator + "        second line",
                "{{#indented}}first line" + lineSeparator + "second line{{/indented}}", ctx);
    }


}
