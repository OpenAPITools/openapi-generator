package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class EscapeJavaLambdaTest extends LambdaTest {

    @Test
    public void escapesDoubleQuote() {
        // Given
        Map<String, Object> ctx = context("escapeJava", new EscapeJavaLambda(), "value", "say \"hi\"");

        // When & Then
        test("say \\\"hi\\\"", "{{#escapeJava}}{{{value}}}{{/escapeJava}}", ctx);
    }

    @Test
    public void escapesBackslash() {
        // Given
        Map<String, Object> ctx = context("escapeJava", new EscapeJavaLambda(), "value", "a\\b");

        // When & Then
        test("a\\\\b", "{{#escapeJava}}{{{value}}}{{/escapeJava}}", ctx);
    }

    @Test
    public void leavesPlainTextUnchanged() {
        // Given
        Map<String, Object> ctx = context("escapeJava", new EscapeJavaLambda(), "value", "USER");

        // When & Then
        test("USER", "{{#escapeJava}}{{{value}}}{{/escapeJava}}", ctx);
    }

}
