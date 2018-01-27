package io.swagger.codegen.mustache;

import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class LowercaseLambdaTest extends MustacheTestBase {

    @Test(description = "lowercases expected inputs")
    public void testExecute() throws Exception {
        // Arrange
        String template = "{{#lowercase}}{{value}}{{/lowercase}}";
        Object lowercaseCtx = context(
                "lowercase", new LowercaseLambda(),
                "value", "lowercase input"
        );
        Object uppercaseCtx = context(
                "lowercase", new LowercaseLambda(),
                "value", "UPPERCASE INPUT"
        );

        // Act
        String lowercaseResult = compile(template, lowercaseCtx);
        String uppercaseResult = compile(template, uppercaseCtx);


        // Assert
        assertEquals(lowercaseResult, "lowercase input");
        assertEquals(uppercaseResult, "uppercase input");
    }
}