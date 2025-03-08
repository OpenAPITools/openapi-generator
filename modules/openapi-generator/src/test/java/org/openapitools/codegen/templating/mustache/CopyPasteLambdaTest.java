package org.openapitools.codegen.templating.mustache;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.templating.mustache.CopyLambda.CopyContent;
import org.openapitools.codegen.templating.mustache.CopyLambda.WhiteSpaceStrategy;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Map;

public class CopyPasteLambdaTest extends LambdaTest {

    @Mock
    CodegenConfig generator;

    @BeforeMethod
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void camelCaseTest() {
        final CopyContent copyContent = new CopyContent();
        Map<String, Object> ctx = context("copy", new CopyLambda(copyContent, WhiteSpaceStrategy.None, WhiteSpaceStrategy.None));
        ctx.put("paste", new PasteLambda(copyContent, false));
        test("foo", "{{#copy}}foo{{/copy}}{{#paste}}{{/paste}}", ctx);

        // the first line break does not count
        // it results in the tag being the only element on the line, so the line does not get printed
        test("\nfoo\n", "{{#copy}}\n\nfoo\n{{/copy}}{{#paste}}{{/paste}}", ctx);
    }

    @Test
    public void camelCaseTestAppend() {
        final CopyContent copyContent = new CopyContent();
        Map<String, Object> ctx = context("copy", new CopyLambda(copyContent, WhiteSpaceStrategy.AppendLineBreakIfMissing, WhiteSpaceStrategy.AppendLineBreakIfMissing));
        ctx.put("paste", new PasteLambda(copyContent, false));
        test("\nfoo\n", "{{#copy}}foo{{/copy}}{{#paste}}{{/paste}}", ctx);
        test("\nfoo\n", "{{#copy}}\n\nfoo\n{{/copy}}{{#paste}}{{/paste}}", ctx);
    }

    @Test
    public void camelCaseTestStripLineBreakIfPresent() {
        final CopyContent copyContent = new CopyContent();
        Map<String, Object> ctx = context("copy", new CopyLambda(copyContent, WhiteSpaceStrategy.StripLineBreakIfPresent, WhiteSpaceStrategy.StripLineBreakIfPresent));
        ctx.put("paste", new PasteLambda(copyContent, false));
        test("foo", "{{#copy}}foo{{/copy}}{{#paste}}{{/paste}}", ctx);
        test("foo", "{{#copy}}\nfoo\n{{/copy}}{{#paste}}{{/paste}}", ctx);
        test("    \nfoo\n    ", "{{#copy}}\n\n    \nfoo\n    {{/copy}}{{#paste}}{{/paste}}", ctx);
        test("    foo    ", "{{#copy}}\n\n    foo    \n{{/copy}}{{#paste}}{{/paste}}", ctx);
    }

    @Test
    public void camelCaseTestStrip() {
        final CopyContent copyContent = new CopyContent();
        Map<String, Object> ctx = context("copy", new CopyLambda(copyContent, WhiteSpaceStrategy.Strip, WhiteSpaceStrategy.Strip));
        ctx.put("paste", new PasteLambda(copyContent, false));
        test("foo", "{{#copy}}foo{{/copy}}{{#paste}}{{/paste}}", ctx);
        test("foo", "{{#copy}}\n\nfoo\n{{/copy}}{{#paste}}{{/paste}}", ctx);
        test("foo", "{{#copy}}    \nfoo\n    {{/copy}}{{#paste}}{{/paste}}", ctx);
        test("foo", "{{#copy}}\n\n    foo    \n{{/copy}}{{#paste}}{{/paste}}", ctx);
    }
}
