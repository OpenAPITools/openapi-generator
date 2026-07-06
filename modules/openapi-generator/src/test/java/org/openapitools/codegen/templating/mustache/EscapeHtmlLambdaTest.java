package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Template;
import org.apache.commons.text.StringEscapeUtils;
import org.testng.annotations.Test;

import java.io.StringWriter;
import java.io.Writer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.testng.Assert.assertEquals;

public class EscapeHtmlLambdaTest {
    @Test
    public void escapesHtmlCharactersAsFragmentStreams() throws Exception {
        StringWriter output = new StringWriter();
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("<tag attr=\"value\">Tom & ");
            assertEquals(output.toString(), "&lt;tag attr=&quot;value&quot;&gt;Tom &amp; ");
            invocation.<Writer>getArgument(0).write("Jerry</tag>");
            return null;
        }).when(fragment).execute(any(Writer.class));

        new EscapeHtmlLambda().execute(fragment, output);

        assertEquals(output.toString(), "&lt;tag attr=&quot;value&quot;&gt;Tom &amp; Jerry&lt;/tag&gt;");
    }

    @Test
    public void handlesSurrogatePairsSplitAcrossWrites() throws Exception {
        String value = "<tag>\uD83D\uDE00 & copy</tag>";
        StringWriter output = new StringWriter();
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write(value.substring(0, 6));
            invocation.<Writer>getArgument(0).write(value.substring(6));
            return null;
        }).when(fragment).execute(any(Writer.class));

        new EscapeHtmlLambda().execute(fragment, output);

        assertEquals(output.toString(), StringEscapeUtils.escapeHtml4(value));
    }
}
