package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.*;

public class UppercaseLambdaTest extends MustacheTestBase {

    @Test(description = "uppercases expected inputs")
    public void testExecute() throws Exception {
        // Arrange
        String template = "{{#uppercase}}{{value}}{{/uppercase}}";
        Object lowercaseCtx = context(
                "uppercase", new UppercaseLambda(),
                "value", "lowercase input"
        );
        Object uppercaseCtx = context(
                "uppercase", new UppercaseLambda(),
                "value", "UPPERCASE INPUT"
        );

        // Act
        String lowercaseResult = compile(template, lowercaseCtx);
        String uppercaseResult = compile(template, uppercaseCtx);


        // Assert
        assertEquals(lowercaseResult, "LOWERCASE INPUT");
        assertEquals(uppercaseResult, "UPPERCASE INPUT");
    }
}