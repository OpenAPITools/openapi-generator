package io.swagger.codegen.mustache;

import io.swagger.codegen.languages.CSharpClientCodegen;
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

    @Test(description = "lowercase escapes reserved words")
    public void testEscapingReservedWords() {
        // Arrange
        String template = "{{#lowercase}}{{value}}{{/lowercase}}";
        String expected = "_class";
        Object ctx = context(
                "lowercase", new LowercaseLambda().generator(new CSharpClientCodegen()),
                "value", "CLASS"
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }
}