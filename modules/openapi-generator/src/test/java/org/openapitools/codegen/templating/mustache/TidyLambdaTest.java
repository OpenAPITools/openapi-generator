package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

public class TidyLambdaTest extends LambdaTest {

    @Test
    public void tidyApostrophesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("'", "{{#tidy}}`{{/tidy}}", ctx);
        test("'", "{{#tidy}}``{{/tidy}}", ctx);
        test("'", "{{#tidy}}``````{{/tidy}}", ctx);
        test("' '", "{{#tidy}}` `{{/tidy}}", ctx);
        test("' '", "{{#tidy}}``` `{{/tidy}}", ctx);
        test("' '", "{{#tidy}}`` ``{{/tidy}}", ctx);
        test("' '", "{{#tidy}}``` ```{{/tidy}}", ctx);
        test("' '", "{{#tidy}}`````` ``````````{{/tidy}}", ctx);
        test("He says: 'This is a test'", "{{#tidy}}He says: `This is a test`{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("'", "{{#tidyForDocs}}`{{/tidyForDocs}}", ctxForDocs);
        test("'", "{{#tidyForDocs}}``{{/tidyForDocs}}", ctxForDocs);
        test("'", "{{#tidyForDocs}}``````{{/tidyForDocs}}", ctxForDocs);
        test("' '", "{{#tidyForDocs}}` `{{/tidyForDocs}}", ctxForDocs);
        test("``` '", "{{#tidyForDocs}}``` `{{/tidyForDocs}}", ctxForDocs);
        test("' '", "{{#tidyForDocs}}`` ``{{/tidyForDocs}}", ctxForDocs);
        test("``` ```", "{{#tidyForDocs}}``` ```{{/tidyForDocs}}", ctxForDocs);
        test("' '", "{{#tidyForDocs}}`````` ``````````{{/tidyForDocs}}", ctxForDocs);
        test("He says: 'This is a test'", "{{#tidyForDocs}}He says: `This is a test`{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyNonStandardQuotesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("\"", "{{#tidy}}“{{/tidy}}", ctx);
        test("\"", "{{#tidy}}”{{/tidy}}", ctx);
        test("\"TEST\"", "{{#tidy}}“TEST”{{/tidy}}", ctx);
        test("\"TEST\"", "{{#tidy}}“““““TEST”””””{{/tidy}}", ctx);
        test("\"TEST\"", "{{#tidy}}“””“TEST”“””“{{/tidy}}", ctx);
        test("He says: \"This is a test\"", "{{#tidy}}He says: “This is a test”{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("\"", "{{#tidyForDocs}}“{{/tidyForDocs}}", ctxForDocs);
        test("\"", "{{#tidyForDocs}}”{{/tidyForDocs}}", ctxForDocs);
        test("\"TEST\"", "{{#tidyForDocs}}“TEST”{{/tidyForDocs}}", ctxForDocs);
        test("\"TEST\"", "{{#tidyForDocs}}“““““TEST”””””{{/tidyForDocs}}", ctxForDocs);
        test("\"TEST\"", "{{#tidyForDocs}}“””“TEST”“””“{{/tidyForDocs}}", ctxForDocs);
        test("He says: \"This is a test\"", "{{#tidyForDocs}}He says: “This is a test”{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyBreakTagsTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\nTEST", "{{#tidy}}TEST<br>TEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST<br><br>TEST{{/tidy}}", ctx);
        test("This is the first phrase.\r\nThen another phrase comes up after a break.\r\nHello test!", "{{#tidy}}This is the first phrase.<br>Then another phrase comes up after a break.<br><br>Hello test!{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\nTEST", "{{#tidyForDocs}}TEST<br>TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST<br><br>TEST{{/tidyForDocs}}", ctxForDocs);
        test("This is the first phrase.\r\nThen another phrase comes up after a break.\r\n\r\nHello test!", "{{#tidyForDocs}}This is the first phrase.<br>Then another phrase comes up after a break.<br><br>Hello test!{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyBracketsTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("<", "{{#tidy}}<{{/tidy}}", ctx);
        test(">", "{{#tidy}}>{{/tidy}}", ctx);
        test("<>", "{{#tidy}}<>{{/tidy}}", ctx);
        test("<TEST>", "{{#tidy}}<TEST>{{/tidy}}", ctx);
        test("Brackets test: <my example of tag>", "{{#tidy}}Brackets test: <my example of tag>{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("&lt;", "{{#tidyForDocs}}<{{/tidyForDocs}}", ctxForDocs);
        test("&gt;", "{{#tidyForDocs}}>{{/tidyForDocs}}", ctxForDocs);
        test("&lt;&gt;", "{{#tidyForDocs}}<>{{/tidyForDocs}}", ctxForDocs);
        test("&lt;TEST&gt;", "{{#tidyForDocs}}<TEST>{{/tidyForDocs}}", ctxForDocs);
        test("Brackets test: &lt;my example of tag&gt;", "{{#tidyForDocs}}Brackets test: <my example of tag>{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyMultipleSpacesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test(" TEST", "{{#tidy}}  TEST{{/tidy}}", ctx);
        test(" TEST", "{{#tidy}}   TEST{{/tidy}}", ctx);
        test(" TEST", "{{#tidy}}            TEST{{/tidy}}", ctx);
        test("An example with many spaces", "{{#tidy}}An    example   with      many      spaces{{/tidy}}", ctx);
        test("TEST\r\n * **\r\n**ANOTHER TEST**","{{#tidy}}TEST\r\n\r\n * **\r\n\r\n**ANOTHER TEST**{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test(" TEST", "{{#tidyForDocs}}  TEST{{/tidyForDocs}}", ctxForDocs);
        test(" TEST", "{{#tidyForDocs}}   TEST{{/tidyForDocs}}", ctxForDocs);
        test(" TEST", "{{#tidyForDocs}}            TEST{{/tidyForDocs}}", ctxForDocs);
        test("An example with many spaces", "{{#tidyForDocs}}An    example   with      many      spaces{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n * **\r\n\r\n**ANOTHER TEST**","{{#tidyForDocs}}TEST\r\n\r\n * **\r\n\r\n**ANOTHER TEST**{{/tidyForDocs}}", ctxForDocs);

    }

    @Test
    public void tidyMultipleLinesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\nTEST{{/tidy}}", ctx);
        test("TEST\nTEST", "{{#tidy}}TEST\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\r\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\r\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("An example\r\n with maximum\r\n 2 consecutives\r\n line-breaks", "{{#tidy}}An example\r\n with maximum\r\n\r\n 2 consecutives\r\n line-breaks{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks", "{{#tidy}}An example\r\n\r\n\r\n\r\n\r\n\r\n with\r\n\r\n\r\n\r\n\r\n more than 2 consecutives\r\n line-breaks{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks", "{{#tidy}}An example\n\r\n\n with\n\n\n\n\n more than 2 consecutives\r\n\n\n line-breaks{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\nTEST", "{{#tidyForDocs}}TEST\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\nTEST", "{{#tidyForDocs}}TEST\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\nTEST", "{{#tidyForDocs}}TEST\r\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\n\nTEST", "{{#tidyForDocs}}TEST\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\n\r\nTEST", "{{#tidyForDocs}}TEST\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\r\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n\r\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n with maximum\r\n\r\n 2 consecutives\r\n line-breaks", "{{#tidyForDocs}}An example\r\n with maximum\r\n\r\n 2 consecutives\r\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n line-breaks", "{{#tidyForDocs}}An example\r\n\r\n\r\n\r\n\r\n\r\n with\r\n\r\n\r\n\r\n\r\n more than 2 consecutives\r\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n\r\n line-breaks", "{{#tidyForDocs}}An example\n\r\n\n with\n\n\n\n\n more than 2 consecutives\r\n\n\n line-breaks{{/tidyForDocs}}", ctxForDocs);

    }

    @Test
    public void tidyStartsWithNewLinesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("", "{{#tidy}}\r\n{{/tidy}}", ctx);
        test("", "{{#tidy}}\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\r\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\nTEST{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}\n\n\n\n\n\n\n\n\n\n\nTEST{{/tidy}}", ctx);
        test("An example starting with new lines", "{{#tidy}}\nAn example starting with new lines{{/tidy}}", ctx);
        test("An example starting with new lines", "{{#tidy}}\r\nAn example starting with new lines{{/tidy}}", ctx);
        test("An example starting with new lines", "{{#tidy}}\r\n\r\nAn example starting with new lines{{/tidy}}", ctx);
        test("An example starting with new lines", "{{#tidy}}\n\n\nAn example starting with new lines{{/tidy}}", ctx);
        test("An example starting with new lines", "{{#tidy}}\r\n\r\n\n\r\nAn example starting with new lines{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("", "{{#tidyForDocs}}\r\n{{/tidyForDocs}}", ctxForDocs);
        test("", "{{#tidyForDocs}}\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}\n\n\n\n\n\n\n\n\n\n\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("An example starting with new lines", "{{#tidyForDocs}}\nAn example starting with new lines{{/tidyForDocs}}", ctxForDocs);
        test("An example starting with new lines", "{{#tidyForDocs}}\r\nAn example starting with new lines{{/tidyForDocs}}", ctxForDocs);
        test("An example starting with new lines", "{{#tidyForDocs}}\r\n\r\nAn example starting with new lines{{/tidyForDocs}}", ctxForDocs);
        test("An example starting with new lines", "{{#tidyForDocs}}\n\n\nAn example starting with new lines{{/tidyForDocs}}", ctxForDocs);
        test("An example starting with new lines", "{{#tidyForDocs}}\r\n\r\n\n\r\nAn example starting with new lines{{/tidyForDocs}}", ctxForDocs);

    }

    @Test
    public void tidyEndWithNewLinesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST", "{{#tidy}}TEST\r\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\r\n\r\n\r\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\n\r\n\r\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\n{{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST\n\n\n\n\n\n\n\n\n\n\n{{/tidy}}", ctx);
        test("An example ending with new lines", "{{#tidy}}An example ending with new lines\n{{/tidy}}", ctx);
        test("An example ending with new lines", "{{#tidy}}An example ending with new lines\r\n{{/tidy}}", ctx);
        test("An example ending with new lines", "{{#tidy}}An example ending with new lines\r\n\r\n{{/tidy}}", ctx);
        test("An example ending with new lines", "{{#tidy}}An example ending with new lines\n\n\n{{/tidy}}", ctx);
        test("An example ending with new lines", "{{#tidy}}An example ending with new lines\r\n\r\n\n\r\n{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST", "{{#tidyForDocs}}TEST\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\r\n\r\n\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\n\r\n\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\r\n\r\n\n\n\r\n\r\n\n\n\n\r\n\r\n\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST\n\n\n\n\n\n\n\n\n\n\n{{/tidyForDocs}}", ctxForDocs);
        test("An example ending with new lines", "{{#tidyForDocs}}An example ending with new lines\n{{/tidyForDocs}}", ctxForDocs);
        test("An example ending with new lines", "{{#tidyForDocs}}An example ending with new lines\r\n{{/tidyForDocs}}", ctxForDocs);
        test("An example ending with new lines", "{{#tidyForDocs}}An example ending with new lines\r\n\r\n{{/tidyForDocs}}", ctxForDocs);
        test("An example ending with new lines", "{{#tidyForDocs}}An example ending with new lines\n\n\n{{/tidyForDocs}}", ctxForDocs);
        test("An example ending with new lines", "{{#tidyForDocs}}An example ending with new lines\r\n\r\n\n\r\n{{/tidyForDocs}}", ctxForDocs);

    }

    @Test
    public void tidyHorizontalLineTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST ***TEST{{/tidy}}", ctx);
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST ******TEST{{/tidy}}", ctx);
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST ---TEST{{/tidy}}", ctx);
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST -------TEST{{/tidy}}", ctx);
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST ___TEST{{/tidy}}", ctx);
        test("TEST\r\n ***\r\nTEST", "{{#tidy}}TEST _______TEST{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST ***TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST *******TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST ---TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST -------TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST ___TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\nTEST", "{{#tidyForDocs}}TEST _______TEST{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyDocListsTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\n  * TEST\r\n  * TEST", "{{#tidy}}TEST\r\n  * TEST\r\n  * TEST{{/tidy}}", ctx);
        test("TEST\r\n  - TEST\r\n    + TEST", "{{#tidy}}TEST\r\n  - TEST\r\n    + TEST{{/tidy}}", ctx);
        test("TEST\r\n       1. TEST\r\n           2. TEST", "{{#tidy}}TEST\r\n       1. TEST\r\n           2. TEST{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\n  * TEST\r\n  * TEST", "{{#tidyForDocs}}TEST\r\n  * TEST\r\n  * TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n  - TEST\r\n    + TEST", "{{#tidyForDocs}}TEST\r\n  - TEST\r\n    + TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n       1. TEST\r\n           2. TEST", "{{#tidyForDocs}}TEST\r\n       1. TEST\r\n           2. TEST{{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyEndWithSpacesTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST", "{{#tidy}}TEST {{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST  {{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST    {{/tidy}}", ctx);
        test("TEST", "{{#tidy}}TEST      {{/tidy}}", ctx);
        test("TEST\r\n TEST\r\nTEST", "{{#tidy}}TEST \r\n TEST \r\nTEST  {{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST", "{{#tidyForDocs}}TEST {{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST  {{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST    {{/tidyForDocs}}", ctxForDocs);
        test("TEST", "{{#tidyForDocs}}TEST      {{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n TEST\r\nTEST", "{{#tidyForDocs}}TEST \r\n TEST \r\nTEST  {{/tidyForDocs}}", ctxForDocs);
    }

    @Test
    public void tidyMixedTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>TEST\n{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>\nTEST<br>{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}\n\r\nTEST\n<br>\r\nTEST\r\n{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>\r\nTEST   {{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>TEST \n {{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>\nTEST<br> {{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}\n\r\nTEST\n<br>\r\nTEST \r\n {{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}\n\r\nTEST\n<br>\r\nTEST  \r\n{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n<br>TEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n<br>\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST<br>\r\n<br>\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n<br><br>\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\r\n<br><br>\r\nTEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>TEST{{/tidy}}", ctx);
        test("TEST\r\nTEST", "{{#tidy}}TEST\n<br>\nTEST{{/tidy}}", ctx);
        test("", "{{#tidy}}  {{/tidy}}", ctx);
        test("", "{{#tidy}}   {{/tidy}}", ctx);
        test("", "{{#tidy}}            {{/tidy}}", ctx);
        test("An example with many spaces", "{{#tidy}}An    example   with      many      spaces    {{/tidy}}", ctx);
        test("An example\r\n with maximum\r\n 2 consecutives\r\n line-breaks", "{{#tidy}}An example\r\n with maximum \r\n\r\n 2 consecutives \r\n line-breaks{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks", "{{#tidy}}An example\r\n\r\n\r\n\r\n\r\n\r\n with \r\n\r\n\r\n\r\n\r\n more than 2 consecutives \r\n line-breaks{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks", "{{#tidy}}An example\n\r\n\n with \n\n\n\n\n more than 2 consecutives \r\n\n\n line-breaks{{/tidy}}", ctx);
        test("This is the first phrase.\r\nThen another phrase comes up after a break.\r\nHello test!", "{{#tidy}}This is the first phrase. <br>Then another phrase comes up after a break. <br><br>Hello test!{{/tidy}}", ctx);
        test("An example with many spaces", "{{#tidy}}An    example   with      many      spaces   {{/tidy}}", ctx);
        test("An example\r\n with maximum\r\n 2 consecutives\r\n line-breaks\r\n and break tag", "{{#tidy}}An example\r\n with maximum \r\n\r\n 2 consecutives \r\n line-breaks\n<br>\n and break tag{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks\r\n and break tag", "{{#tidy}}An example\r\n\r\n\r\n\r\n\r\n\r\n with \r\n\r\n\r\n\r\n\r\n more than 2 consecutives \r\n line-breaks\n<br>\n and break tag{{/tidy}}", ctx);
        test("An example\r\n with\r\n more than 2 consecutives\r\n line-breaks", "{{#tidy}}An example\n\r\n\n with \n\n\n\n\n more than 2 consecutives \r\n\n\n line-breaks{{/tidy}}", ctx);
        test("TEST\r\n ***\r\n **ANOTHER TEST**","{{#tidy}}TEST\r\n\r\n  ***\r\n\r\n  **ANOTHER TEST**{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>TEST\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>\nTEST<br>{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}\n\r\nTEST\n<br>\r\nTEST\r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>\r\nTEST   {{/tidyForDocs}}", ctxForDocs);
        test("TEST\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>TEST \n {{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>\nTEST<br> {{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}\n\r\nTEST\n<br>\r\nTEST \r\n {{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}\n\r\nTEST\n<br>\r\nTEST  \r\n{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n<br>TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n<br>\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST<br>\r\n<br>\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n<br><br>\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\r\n<br><br>\r\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\nTEST", "{{#tidyForDocs}}TEST\n<br>\nTEST{{/tidyForDocs}}", ctxForDocs);
        test("", "{{#tidyForDocs}}  {{/tidyForDocs}}", ctxForDocs);
        test("", "{{#tidyForDocs}}   {{/tidyForDocs}}", ctxForDocs);
        test("", "{{#tidyForDocs}}            {{/tidyForDocs}}", ctxForDocs);
        test("An example with many spaces", "{{#tidyForDocs}}An    example   with      many      spaces    {{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n with maximum\r\n\r\n 2 consecutives\r\n line-breaks", "{{#tidyForDocs}}An example\r\n with maximum \r\n\r\n 2 consecutives \r\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n line-breaks", "{{#tidyForDocs}}An example\r\n\r\n\r\n\r\n\r\n\r\n with \r\n\r\n\r\n\r\n\r\n more than 2 consecutives \r\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n\r\n line-breaks", "{{#tidyForDocs}}An example\n\r\n\n with \n\n\n\n\n more than 2 consecutives \r\n\n\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("This is the first phrase.\r\nThen another phrase comes up after a break.\r\n\r\nHello test!", "{{#tidyForDocs}}This is the first phrase. <br>Then another phrase comes up after a break. <br><br>Hello test!{{/tidyForDocs}}", ctxForDocs);
        test("An example with many spaces", "{{#tidyForDocs}}An    example   with      many      spaces   {{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n with maximum\r\n\r\n 2 consecutives\r\n line-breaks\r\n\r\n and break tag", "{{#tidyForDocs}}An example\r\n with maximum \r\n\r\n 2 consecutives \r\n line-breaks\n<br>\n and break tag{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n line-breaks\r\n\r\n and break tag", "{{#tidyForDocs}}An example\r\n\r\n\r\n\r\n\r\n\r\n with \r\n\r\n\r\n\r\n\r\n more than 2 consecutives \r\n line-breaks\n<br>\n and break tag{{/tidyForDocs}}", ctxForDocs);
        test("An example\r\n\r\n with\r\n\r\n more than 2 consecutives\r\n\r\n line-breaks", "{{#tidyForDocs}}An example\n\r\n\n with \n\n\n\n\n more than 2 consecutives \r\n\n\n line-breaks{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n ***\r\n\r\n **ANOTHER TEST**","{{#tidyForDocs}}TEST\r\n\r\n  ***\r\n\r\n  **ANOTHER TEST**{{/tidyForDocs}}", ctxForDocs);

    }

    @Test
    public void tidyUnchangedTextTest() {
        // Given
        Map<String, Object> ctx = context("tidy", new TidyLambda());

        // When & Then
        test("TEST\r\n***\r\nANOTHER TEST","{{#tidy}}TEST\r\n\r\n***\r\n\r\nANOTHER TEST{{/tidy}}", ctx);
        test("TEST\r\n***\r\n**ANOTHER TEST**","{{#tidy}}TEST\r\n\r\n***\r\n\r\n**ANOTHER TEST**{{/tidy}}", ctx);

        // Given for docs
        Map<String, Object> ctxForDocs = context("tidyForDocs", new TidyLambda(true));

        // When & Then
        test("TEST\r\n\r\n***\r\n\r\nANOTHER TEST","{{#tidyForDocs}}TEST\r\n\r\n***\r\n\r\nANOTHER TEST{{/tidyForDocs}}", ctxForDocs);
        test("TEST\r\n\r\n***\r\n\r\n**ANOTHER TEST**","{{#tidyForDocs}}TEST\r\n\r\n***\r\n\r\n**ANOTHER TEST**{{/tidyForDocs}}", ctxForDocs);

    }
}
