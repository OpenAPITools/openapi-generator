package org.openapitools.codegen.templating.mustache;

import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Map;

public class UncamelizeLambdaTest extends LambdaTest {
    @BeforeMethod
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void camelCaseTest() {
        // Given
        Map<String, Object> ctx = context("uncamelize", new UncamelizeLambda());

        // When & Then
        test("Input Text", "{{#uncamelize}}inputText{{/uncamelize}}", ctx);
    }

    @Test
    public void pascalCaseTest() {
        // Given
        Map<String, Object> ctx = context("uncamelize", new UncamelizeLambda());

        // When & Then
        test("Input Text", "{{#uncamelize}}InputText{{/uncamelize}}", ctx);
    }


    @Test
    public void emptyStringTest() {
        // Given
        Map<String, Object> ctx = context("uncamelize", new UncamelizeLambda());

        // When & Then
        test("", "{{#uncamelize}}{{/uncamelize}}", ctx);
    }

    @Test
    public void nonCamelCaseStringTest() {
        // Given
        Map<String, Object> ctx = context("uncamelize", new UncamelizeLambda());

        // When & Then
        test("Input Text", "{{#uncamelize}}Input Text{{/uncamelize}}", ctx);
    }
}