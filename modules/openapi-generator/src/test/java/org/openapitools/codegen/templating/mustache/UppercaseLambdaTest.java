package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class UppercaseLambdaTest extends LambdaTest {

    @Test
    public void uppercaseTest() {
        // Given
        Map<String, Object> ctx = context("uppercase", new UppercaseLambda());

        // When & Then
        test("INPUT TEXT", "{{#uppercase}}InPut Text{{/uppercase}}", ctx);
    }

}
