package org.openapitools.codegen.templating.mustache;

import org.testng.annotations.Test;

import java.util.Map;

public class KebabCaseLambdaTest extends LambdaTest {

    @Test
    public void CamelCaseToKebabcaseTest() {
        // Given
        Map<String, Object> ctx = context("kebabcase", new KebabCaseLambda());

        // When & Then
        test("access-code", "{{#kebabcase}}accessCode{{/kebabcase}}", ctx);
    }

    @Test
    public void KebabcaseTestWithSpecialCharacters() {
        // Given
        Map<String, Object> ctx = context("kebabcase", new KebabCaseLambda());

        // When & Then
        test("this-is-a-test", "{{#kebabcase}}This?Is#@$%^A#@$%^Test{{/kebabcase}}", ctx);
    }

    @Test
    public void SnakeCaseToKebabCase() {
        // Given
        Map<String, Object> ctx = context("kebabcase", new KebabCaseLambda());

        // When & Then
        test("this-is-another-test", "{{#kebabcase}}This_is_another_test{{/kebabcase}}", ctx);
    }


}
