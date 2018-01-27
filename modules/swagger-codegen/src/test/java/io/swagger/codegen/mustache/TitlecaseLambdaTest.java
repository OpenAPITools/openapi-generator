package io.swagger.codegen.mustache;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class TitlecaseLambdaTest extends MustacheTestBase {

    private String template = "{{#titlecase}}{{value}}{{/titlecase}}";

    @Test(description = "title cases single word")
    public void testTitlecase() throws Exception {
        // Arrange
        String input = "single";
        String expected = "Single";
        Object ctx = context(
                "titlecase", new TitlecaseLambda(),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }

    @Test(description = "title cases multiple words based on custom delimeter")
    public void testTitlecaseCustomDelim() throws Exception {
        // Arrange
        String input = "one|or|more|words";
        String expected = "One|Or|More|Words";
        Object ctx = context(
                "titlecase", new TitlecaseLambda("|"),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }

    @Test(description = "title cases first word when delim is null")
    public void testTitlecaseFirstWord() throws Exception {
        // Arrange
        String input = "one or more words";
        String expected = "One or more words";
        Object ctx = context(
                "titlecase", new TitlecaseLambda(null),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }

    @Test(description = "title cases multiple words")
    public void testTitlecaseMultipleWords() throws Exception {
        // Arrange
        String input = "one or more words";
        String expected = "One Or More Words";
        Object ctx = context(
                "titlecase", new TitlecaseLambda(),
                "value", input
        );

        // Act
        String actual = compile(template, ctx);

        // Assert
        assertEquals(actual, expected);
    }
}