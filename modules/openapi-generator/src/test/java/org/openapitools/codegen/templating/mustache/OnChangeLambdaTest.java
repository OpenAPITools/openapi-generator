package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class OnChangeLambdaTest extends LambdaTest {

    private Map<String, Object> ctx;

    // OnChangeLambda holds internal state it is important that context is
    // reinitialize before each test.
    @BeforeMethod
    public void setup() {
        ctx = context("onchange", new OnChangeLambda());
    }

    @Test
    public void firstValueIsReturnedTest() {
        // Given

        // When & Then
        test("first", "{{#onchange}}first{{/onchange}}", ctx);
    }

    @Test
    public void repeatingValueReturnedOnFirstOccurrenceTest() {
        // Given

        // When & Then
        test("First", "{{#onchange}}First{{/onchange}}", ctx);
        test("", "{{#onchange}}First{{/onchange}}", ctx);

        test("Another", "{{#onchange}}Another{{/onchange}}", ctx);
        test("", "{{#onchange}}Another{{/onchange}}", ctx);
        test("", "{{#onchange}}Another{{/onchange}}", ctx);

        test("First", "{{#onchange}}First{{/onchange}}", ctx);
    }

}
