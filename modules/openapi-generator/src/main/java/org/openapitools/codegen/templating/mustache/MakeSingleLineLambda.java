package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts any text into a single line, i.e. removes any line-breaks into a space.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("make-single-line", new MakeSingleLineLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>{@code
 *     {{#make-single-line}}{{{text}}}{{/make-single-line}}
 * }</pre>
 */
public class MakeSingleLineLambda implements Mustache.Lambda 
{
    private static final String MULTIPLE_NEWLINE_REGEX = "((\\r)?\\n)+";

    public MakeSingleLineLambda() 
    {
    }

    @Override
    public void execute(Template.Fragment i_fragment, Writer i_writer) throws IOException 
    {
        String text = 
            i_fragment.execute()
                .replaceAll(MULTIPLE_NEWLINE_REGEX, " ");
        i_writer.write(text);
    }
}
