package io.swagger.codegen.ignore.rules;

import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;

import static org.testng.Assert.*;

public class FileRuleTest {
    @Test
    public void testMatchComplex() throws Exception {
        // Arrange
        final String definition = "path/to/**/complex/*.txt";
        final String relativePath = "path/to/some/nested/complex/xyzzy.txt";

        final List<Part> syntax = Arrays.asList(
                new Part(IgnoreLineParser.Token.ROOTED_MARKER),
                new Part(IgnoreLineParser.Token.TEXT, "path"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.TEXT, "to"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.MATCH_ALL),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.TEXT, "complex"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.MATCH_ANY),
                new Part(IgnoreLineParser.Token.TEXT, ".txt")
        );

        Rule rule = new FileRule(syntax, definition);
        Boolean actual = null;

        // Act
        actual = rule.matches(relativePath);

        // Assert
        assertTrue(actual);
    }

    @Test
    public void testNonMatchComplex() throws Exception {
        // Arrange
        final String definition = "path/to/**/complex/*.txt";
        final String relativePath = "path/to/some/nested/invalid/xyzzy.txt";

        final List<Part> syntax = Arrays.asList(
                new Part(IgnoreLineParser.Token.ROOTED_MARKER),
                new Part(IgnoreLineParser.Token.TEXT, "path"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.TEXT, "to"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.MATCH_ALL),
                new Part(IgnoreLineParser.Token.TEXT, "complex"),
                new Part(IgnoreLineParser.Token.PATH_DELIM),
                new Part(IgnoreLineParser.Token.MATCH_ANY),
                new Part(IgnoreLineParser.Token.TEXT, ".txt")
        );

        Rule rule = new FileRule(syntax, definition);
        Boolean actual = null;

        // Act
        actual = rule.matches(relativePath);

        // Assert
        assertFalse(actual);
    }

    @Test
    public void testGlobbingRecursive() throws Exception {
        // Arrange
        final String definition = "*.txt";
        final String relativePath = "path/to/some/nested/location/xyzzy.txt";

        // Act
        final List<Part> syntax = Arrays.asList(
                new Part(IgnoreLineParser.Token.MATCH_ALL),
                new Part(IgnoreLineParser.Token.DIRECTORY_MARKER),
                new Part(IgnoreLineParser.Token.MATCH_ANY),
                new Part(IgnoreLineParser.Token.TEXT, ".txt")
        );

        Rule rule = new FileRule(syntax, definition);
        Boolean actual = rule.matches(relativePath);

        // Assert
        assertTrue(actual);
    }

    @Test
    public void testGlobbingNotRecursive() throws Exception {
        // Arrange
        final String definition = "*.txt";
        final String relativePath = "path/to/some/nested/location/xyzzy.txt";

        // Act
        final List<Part> syntax = Arrays.asList(
                new Part(IgnoreLineParser.Token.MATCH_ANY),
                new Part(IgnoreLineParser.Token.TEXT, ".txt")
        );

        Rule rule = new FileRule(syntax, definition);
        Boolean actual = rule.matches(relativePath);

        // Assert
        assertFalse(actual);
    }
}