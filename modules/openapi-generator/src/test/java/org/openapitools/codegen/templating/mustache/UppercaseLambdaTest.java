package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

public class UppercaseLambdaTest extends LambdaTest {

    @Test
    public void uppercaseTest() {
        // Given
        Map<String, Object> ctx = context("uppercase", new UppercaseLambda());

        // When & Then
        test("INPUT TEXT", "{{#uppercase}}InPut Text{{/uppercase}}", ctx);
    }

}
