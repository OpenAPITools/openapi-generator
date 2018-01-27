package io.swagger.codegen.mustache;

import org.apache.commons.lang3.StringUtils;
import org.testng.annotations.Test;

import java.util.Arrays;

import static org.testng.Assert.assertEquals;

public class IndentedLambdaTest extends MustacheTestBase {

    @Test(description = "indents by 4 spaces by default")
    public void testFourSpaceIndent() throws Exception {
        // Arrange
        String indent = StringUtils.repeat(" ", 4);
        String template = "    {{#indent_4}}{{value}}{{/indent_4}}";
        String input = StringUtils.join(
                Arrays.asList(
                        "line1",
                        "line2",
                        "",
                        "line3"
                ), System.lineSeparator());
        String expected = StringUtils.join(
                Arrays.asList(
                        indent + "line1",
                        indent + "line2",
                        indent + "",
                        indent + "line3"
                ), System.lineSeparator());
        Object ctx = context(
                "indent_4", new IndentedLambda(),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }

    @Test
    public void testCustomCountAndDelim() throws Exception {
        // Arrange
        int count = 12;
        String delim = ".";
        String indent = StringUtils.repeat(delim, count);
        String template = indent + "{{#indent_"+count+"}}{{value}}{{/indent_"+count+"}}";
        String input = StringUtils.join(
                Arrays.asList(
                        "line1",
                        "line2",
                        "",
                        "line3"
                ), System.lineSeparator());
        String expected = StringUtils.join(
                Arrays.asList(
                        indent + "line1",
                        indent + "line2",
                        indent + "",
                        indent + "line3"
                ), System.lineSeparator());
        Object ctx = context(
                "indent_" + count, new IndentedLambda(count, delim),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }

    @Test(description = "throws illegal arg for count < 0.",
            expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "prefixSpaceCount must be greater than 0"
    )
    public void testRequiresValidCount() throws Exception {
        // Arrange
        int count = -1;

        // Act
        IndentedLambda instance = new IndentedLambda(count, " ");
    }
}