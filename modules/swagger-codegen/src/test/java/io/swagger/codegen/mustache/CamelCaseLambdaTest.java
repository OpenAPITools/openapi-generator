package io.swagger.codegen.mustache;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.CSharpClientCodegen;
import io.swagger.codegen.languages.ScalaClientCodegen;
import org.testng.annotations.Factory;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class CamelCaseLambdaTest extends MustacheTestBase {
    private final String input;
    private final String expected;
    private Boolean escapeAsParamName = false;

    private CodegenConfig generator = null;

    public CamelCaseLambdaTest(String input, String expected) {
        this.input = input;
        this.expected = expected;
    }

    public CamelCaseLambdaTest generator(CodegenConfig generator) {
        this.generator = generator;
        return this;
    }

    public CamelCaseLambdaTest escapeAsParamName(Boolean setting) {
        this.escapeAsParamName = setting;
        return this;
    }

    @Test(description = "camelCase expected inputs")
    public void testExecute() throws Exception {
        // Arrange
        String template = "{{#camelcase}}{{value}}{{/camelcase}}";
        Object inputCtx = context(
                "camelcase", new CamelCaseLambda().generator(this.generator).escapeAsParamName(this.escapeAsParamName),
                "value", this.input
        );

        // Act
        String actual = compile(template, inputCtx);


        // Assert
        assertEquals(actual, this.expected);
    }

    @Factory
    public static Object[] factoryMethod() {
        return new Object[] {
                new CamelCaseLambdaTest("lowercase input", "lowercase input"),

                // NOTE: DefaultCodegen.camelize(string, true) only results in first character of first word being lowercased.
                // Keeping this behavior as it will match whatever is expected by existing codegen implementations.
                new CamelCaseLambdaTest("UPPERCASE INPUT", "uPPERCASE INPUT"),
                new CamelCaseLambdaTest("inputText", "inputText"),
                new CamelCaseLambdaTest("input_text", "inputText"),

                // TODO: This result for INPUT_TEXT may be unexpected, but is the result of DefaultCodegen.camelize.
                // CamelCaseLambda can be extended to accept a method reference after move to Java 8.
                new CamelCaseLambdaTest("INPUT_TEXT", "iNPUTTEXT"),
                new CamelCaseLambdaTest("input-text", "inputText"),
                new CamelCaseLambdaTest("input-text input-text input-text input-text input-text", "inputText inputText inputText inputText inputText"),
                // C# codegen at time of writing this test escapes using a character that would be removed by camelize function.
                new CamelCaseLambdaTest("class", "_class").generator(new CSharpClientCodegen()),
                new CamelCaseLambdaTest("123List", "_123List").generator(new CSharpClientCodegen()).escapeAsParamName(true),
                // Scala codegen is only one at time of writing this test that uses a Mustache.Escaper
                new CamelCaseLambdaTest("class", "`class`").generator(new ScalaClientCodegen())
        };
    }
}