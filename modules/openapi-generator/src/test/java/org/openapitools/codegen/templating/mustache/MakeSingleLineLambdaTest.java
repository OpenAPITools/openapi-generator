package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

public class MakeSingleLineLambdaTest extends LambdaTest {

    @Test
    public void multipleLinesTest() {
        // Given
        Map<String, Object> ctx = context("makeSingleLine", new MakeSingleLineLambda());

        // When & Then
        test("First line then  Second line", "{{#makeSingleLine}}First line then\n Second line{{/makeSingleLine}}", ctx);
        test("First line then  Second line  Third line", "{{#makeSingleLine}}First line then\n Second line\n Third line{{/makeSingleLine}}", ctx);
        test("First line then  Second line", "{{#makeSingleLine}}First line then\r\n\n Second line{{/makeSingleLine}}", ctx);
        test("First line then  Second line  Third line", "{{#makeSingleLine}}First line then\n\r\n\n Second line\r\n\r\n Third line{{/makeSingleLine}}", ctx);
    }

    @Test
    public void singleLineTest() {
        // Given
        Map<String, Object> ctx = context("makeSingleLine", new MakeSingleLineLambda());

        // When & Then
        test("Single line", "{{#makeSingleLine}}Single line{{/makeSingleLine}}", ctx);
    }
	
    @Test
    public void questionMarkTest() {
        // Given
        Map<String, Object> ctx = context("makeSingleLine", new MakeSingleLineLambda());

        // When & Then
        test("Question is not converted into space?", "{{#makeSingleLine}}Question is not converted into space?{{/makeSingleLine}}", ctx);
    }

    @Test
    public void emptyLineTest() {
        // Given
        Map<String, Object> ctx = context("makeSingleLine", new MakeSingleLineLambda());

        // When & Then
        test("", "{{#makeSingleLine}}{{/makeSingleLine}}", ctx);
    }


}
