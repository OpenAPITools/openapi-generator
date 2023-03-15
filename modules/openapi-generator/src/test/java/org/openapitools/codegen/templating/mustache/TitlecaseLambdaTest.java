package org.openapitools.codegen.templating.mustache;

import java.util.Map;

import org.testng.annotations.Test;

public class TitlecaseLambdaTest extends LambdaTest {

    @Test
    public void titlecaseTest() {
        // Given
        Map<String, Object> ctx = context("titlecase", new TitlecaseLambda());

        // When & Then
        test("Once Upon A Time", "{{#titlecase}}once upon a time{{/titlecase}}", ctx);
    }

    @Test
    public void titlecaseWithDelimiterTest() {
        // Given
        Map<String, Object> ctx = context("titlecase", new TitlecaseLambda("-"));

        // When & Then
        test("Once-Upon-A-Time", "{{#titlecase}}once-upon-a-time{{/titlecase}}", ctx);
    }


}
