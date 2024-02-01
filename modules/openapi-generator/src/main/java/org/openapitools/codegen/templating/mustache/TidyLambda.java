package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Creates a (multi/single-)line text into a tidy comment converting special tags
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("tidy", new TidyLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>{@code
 *     {{#tidy}}{{{text}}}{{/tidy}}
 * }</pre>
 */
public class TidyLambda implements Mustache.Lambda 
{
    private static final String SINGLE_SPACE = " ";
    private static final String MULTIPLE_SPACE_REGEX = "[ ]+";
    private static final String HTML_BREAK_TAG = "<br>";
    private static final String HTML_TAG_REGEX = "<[^>]*>";
    private static final String MULTIPLE_NEWLINE_REGEX = "[\\r?\\n]{2,}";

    @Override
    public void execute(Template.Fragment i_fragment, Writer i_writer) throws IOException 
    {
        String text = 
            i_fragment.execute()
                // Replace and normalize all non-standard apostrophes
                .replaceAll("`+", "'")
                // Replace and normalize all non-standard quotes
                .replaceAll("[“”]+", "\"")
                // Replace all HTML break tags by new lines
                .replaceAll(HTML_BREAK_TAG, "\r\n")
                // Escape all opening/closing brackets
                .replaceAll("<", "\\<")
                .replaceAll(">", "\\>")
                // Normalize spaces
                .replaceAll(MULTIPLE_SPACE_REGEX, SINGLE_SPACE)
                // Normalize new-lines (maximum 2 consecutive line-breaks)
                .replaceAll(MULTIPLE_NEWLINE_REGEX, "\r\n\r\n")
                // Remove new-lines at the end
                .replaceAll("[\\r?\\n]$", "");
        i_writer.write(text);
    }
}
