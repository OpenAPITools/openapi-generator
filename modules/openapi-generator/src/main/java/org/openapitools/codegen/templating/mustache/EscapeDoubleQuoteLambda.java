package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;
import java.util.regex.Matcher;

/**
 * replace double quotes by escaped double quotes
 *
 * Register:
 * <pre>
 * additionalProperties.put("escapeDoubleQuote", new EscapeDoubleQuoteLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#escapeDoubleQuote}}{{name}}{{/escapeDoubleQuote}}
 * </pre>
 */
public class EscapeDoubleQuoteLambda implements Mustache.Lambda{
    private static final String REGEX_DOUBLE_QUOTES_NOT_ALREADY_ESCAPED = "(?<!\\\\)\"";

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        writer.write(fragment.execute()
                .replaceAll(REGEX_DOUBLE_QUOTES_NOT_ALREADY_ESCAPED, Matcher.quoteReplacement("\\\"")));
    }
}
