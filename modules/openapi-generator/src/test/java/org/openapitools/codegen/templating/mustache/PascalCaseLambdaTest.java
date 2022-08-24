package org.openapitools.codegen.templating.mustache;

import static org.mockito.AdditionalAnswers.returnsFirstArg;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openapitools.codegen.CodegenConfig;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class PascalCaseLambdaTest extends LambdaTest {

    @Mock
    CodegenConfig generator;

    @BeforeMethod
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void pascalCaseTest() {
        // Given
        Map<String, Object> ctx = context("pascalcase", new CamelCaseLambda(false));

        // When & Then
        test("InputText", "{{#pascalcase}}Input-text{{/pascalcase}}", ctx);
        test("InputText", "{{#pascalcase}}Input_text{{/pascalcase}}", ctx);
        test("", "{{#pascalcase}}{{/pascalcase}}", ctx);

    }

    @Test
    public void pascalCaseSpaceTest() {
        // Given
        Map<String, Object> ctx = context("pascalcase", new CamelCaseLambda(false));

        // When & Then
        test("InputTextApi", "{{#pascalcase}}Input text  api{{/pascalcase}}", ctx);
    }

    @Test
    public void pascalCaseReservedWordTest() {
        // Given
        Map<String, Object> ctx = context("pascalcase", new CamelCaseLambda(false).generator(generator));

        when(generator.sanitizeName(anyString())).then(returnsFirstArg());
        when(generator.reservedWords()).thenReturn(new HashSet<String>(Arrays.asList("ReservedWord")));
        when(generator.escapeReservedWord("ReservedWord")).thenReturn("escapedReservedWord");

        // When & Then
        test("escapedReservedWord", "{{#pascalcase}}reserved-word{{/pascalcase}}", ctx);
    }

    @Test
    public void pascalCaseEscapeParamTest() {
        // Given
        Map<String, Object> ctx = context("pascalcase", new CamelCaseLambda(false)
                .generator(generator).escapeAsParamName(true));

        when(generator.sanitizeName(anyString())).then(returnsFirstArg());
        when(generator.reservedWords()).thenReturn(new HashSet<String>());
        when(generator.toParamName("InputText")).thenReturn("inputTextAsParam");

        // When & Then
        test("inputTextAsParam", "{{#pascalcase}}Input_text{{/pascalcase}}", ctx);
    }

}
