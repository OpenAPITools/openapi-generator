package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Template;
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
        Template.Fragment fragment = mock(Template.Fragment.class);
        doAnswer(invocation -> {
            invocation.<Writer>getArgument(0).write("<tag attr=\"value\">Tom & ");
            invocation.<Writer>getArgument(0).write("Jerry</tag>");
            return null;
        }).when(fragment).execute(any(Writer.class));

        StringWriter output = new StringWriter();
        new EscapeHtmlLambda().execute(fragment, output);

        assertEquals(output.toString(), "&lt;tag attr=&quot;value&quot;&gt;Tom &amp; Jerry&lt;/tag&gt;");
    }
}
