package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text in a fragment to title case.
 *
 * Register:
 * <pre>
 * additionalProperties.put("titlecase", new TitlecaseLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#titlecase}}{{classname}}{{/titlecase}}
 * </pre>
 */
public class TitlecaseLambda implements Mustache.Lambda  {
    private String delimiter;

    /**
     * Constructs a new instance of {@link TitlecaseLambda}, which will convert all text
     * in a space delimited string to title-case.
     */
    public TitlecaseLambda() {
        this(" ");
    }

    /**
     * Constructs a new instance of {@link TitlecaseLambda}, splitting on the specified
     * delimiter and converting each word to title-case.
     *
     * NOTE: passing {@code null} results in a title-casing the first word only.
     *
     * @param delimiter Provided to allow an override for the default space delimiter.
     */
    public TitlecaseLambda(String delimiter) {
        this.delimiter = delimiter;
    }

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase() + input.substring(1);
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        if (delimiter == null) {
            writer.write(titleCase(text));
            return;
        }

        // Split accepts regex. \Q and \E wrap the delimiter to create a literal regex,
        // so things like "." and "|" aren't treated as their regex equivalents.
        String[] parts = text.split("\\Q" + delimiter + "\\E");
        for (int i = 0; i < parts.length; i++) {
            String part = parts[i];
            writer.write(titleCase(part));
            if (i != parts.length - 1) {
                writer.write(delimiter);
            }
        }
    }
}
