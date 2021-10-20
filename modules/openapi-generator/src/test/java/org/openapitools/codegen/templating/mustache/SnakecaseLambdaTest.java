package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

public class SnakecaseLambdaTest extends LambdaTest {

    @Test
    public void snakecaseTest() {
        // Given
        Map<String, Object> ctx = context("snakecase", new SnakecaseLambda());

        // When & Then
        test("access_code", "{{#snakecase}}accessCode{{/snakecase}}", ctx);
    }

}
