package io.swagger.codegen.ignore.rules;

import org.testng.annotations.Test;

import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import static org.testng.Assert.*;

public class IgnoreLineParserTest {
    private IgnoreLineParser.Token verifyInputToSingleToken(final String input, IgnoreLineParser.Token token) throws ParserException {
        // Act
        List<Part> result = IgnoreLineParser.parse(input);

        // Assert
        assertNotNull(result);
        assertEquals(result.size(), 1);
        IgnoreLineParser.Token actual = result.get(0).getToken();
        assertEquals(actual, token);

        return actual;
    }

    @Test
    public void parseMatchAll() throws Exception {
        verifyInputToSingleToken("**", IgnoreLineParser.Token.MATCH_ALL);
    }

    @Test
    public void parseMatchAny() throws Exception {
        verifyInputToSingleToken("*", IgnoreLineParser.Token.MATCH_ANY);
    }

    @Test(expectedExceptions = ParserException.class,
          expectedExceptionsMessageRegExp = "Negation with no negated pattern\\.")
    public void parseNegate() throws Exception {
        verifyInputToSingleToken("!", IgnoreLineParser.Token.NEGATE);

        // Assert
        fail("Expected simple pattern '!' to throw a ParserException.");
    }

    @Test
    public void parseComment() throws Exception {
        // Arrange
        final String input = "# This is a comment";
        Part actual = null;

        // Act
        List<Part> result = IgnoreLineParser.parse(input);

        // Assert
        assertEquals(result.size(), 1);
        actual = result.get(0);
        assertEquals(actual.getToken(), IgnoreLineParser.Token.COMMENT);
        assertEquals(actual.getValue(),  input);
    }

    @Test
    public void parseEscapedExclamation() throws Exception {
        final String input = "\\!";
        verifyInputToSingleToken(input, IgnoreLineParser.Token.ESCAPED_EXCLAMATION);
    }

    @Test
    public void parseEscapedSpace() throws Exception {
        final String input = "\\ ";
        verifyInputToSingleToken(input, IgnoreLineParser.Token.ESCAPED_SPACE);
    }

    @Test
    public void parseDirectoryMarker() throws Exception {
        // Arrange
        final String input = "foo/";
        Part actual = null;

        // Act
        List<Part> result = IgnoreLineParser.parse(input);

        // Assert
        assertEquals(result.size(), 2);
        actual = result.get(0);
        assertEquals(actual.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(actual.getValue(), "foo");
        actual = result.get(1);
        assertEquals(actual.getToken(), IgnoreLineParser.Token.DIRECTORY_MARKER);
    }

    @Test
    public void parseRooted() throws Exception {
        // Arrange
        final String input = "/abcd";
        Part actual = null;

        // Act
        List<Part> result = IgnoreLineParser.parse(input);

        // Assert
        assertEquals(result.size(), 2);
        actual = result.get(0);
        assertEquals(actual.getToken(), IgnoreLineParser.Token.ROOTED_MARKER);
        actual = result.get(1);
        assertEquals(actual.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(actual.getValue(), "abcd");
    }

    @Test
    public void parseComplex() throws Exception {
        // Arrange
        final String input = "**/abcd/**/foo/bar/sample.txt";
        Part current = null;

        // Act
        Queue<Part> result = new LinkedList<>(IgnoreLineParser.parse(input));

        // Assert
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.MATCH_ALL);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.PATH_DELIM);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(current.getValue(), "abcd");
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.PATH_DELIM);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.MATCH_ALL);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.PATH_DELIM);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(current.getValue(), "foo");
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.PATH_DELIM);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(current.getValue(), "bar");
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.PATH_DELIM);
        current = result.remove();
        assertEquals(current.getToken(), IgnoreLineParser.Token.TEXT);
        assertEquals(current.getValue(), "sample.txt");
    }

    @Test(expectedExceptions = ParserException.class,
            expectedExceptionsMessageRegExp = "The pattern \\*\\*\\* is invalid\\.")
    public void parseTripleStarPattern() throws Exception {
        // Arrange
        final String input = "should/throw/***/anywhere";

        // Act
        List<Part> result = IgnoreLineParser.parse(input);

        // Assert
        fail("Expected pattern containing '***' to throw a ParserException.");
    }
}