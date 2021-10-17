package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class EscapeDoubleQuoteLambdaTest extends LambdaTest {

    private Map<String, Object> ctx = context("escapeDoubleQuote", new EscapeDoubleQuoteLambda());

    @Test
    public void testNoChanges() {
        test(
                "no changes",
                "{{#escapeDoubleQuote}}no changes{{/escapeDoubleQuote}}",
                ctx
        );
    }

    @Test
    public void testDoubleQuotes() {
        test(
                "{\\\"key\\\": \\\"value\\\"}",
                "{{#escapeDoubleQuote}}{\"key\": \"value\"}{{/escapeDoubleQuote}}",
                ctx
        );
    }

    @Test
    public void testAlreadyEscapedDoubleQuotes() {

        test(
                "{\\\"key\\\": \\\"value\\\"}",
                "{{#escapeDoubleQuote}}{\\\"key\\\": \\\"value\\\"}{{/escapeDoubleQuote}}",
                ctx
        );
    }
}