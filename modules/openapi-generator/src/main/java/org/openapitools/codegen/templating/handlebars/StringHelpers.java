package org.openapitools.codegen.templating.handlebars;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.TagType;

import java.io.IOException;
import java.util.Locale;

public enum StringHelpers implements Helper<Object> {

    /**
     * Indicates the string starts with the defined value.
     * For example:
     *
     * <pre>
     * {{startsWith a b ["insensitive"]}}
     * </pre>
     *
     * <pre>
     * {{startsWith a text='b' [insensitive=true]}}
     * </pre>
     *
     * Render 'yes' or 'no':
     * <pre>
     *   {{#startsWith a b}}
     *     yes
     *   {{else}}
     *     no
     *   {{/startsWith}}
     * </pre>
     *
     * Render 'true' or 'false':
     * <pre>
     *   {{startsWith a b}}
     * </pre>
     *
     * Render 'y' or 'n':
     * <pre>
     *   {{startsWith a b yes='y' no='n'}}
     * </pre>
     *
     * If value is "handlebars.java", the output will be "Handlebars.java".
     */
    startsWith {
        @Override
        public Object apply(Object value, Options options) throws IOException {
            String match = options.param(0, options.hash("text", ""));
            if (match.length() < 1) {
                return false;
            }

            boolean caseInsensitive = options.hash("insensitive", false);
            boolean result = caseInsensitive ? value.toString().toLowerCase(Locale.ROOT).startsWith(match.toLowerCase(Locale.ROOT)) : value.toString().startsWith(match);

            if (options.tagType == TagType.SECTION) {
                return result ? options.fn() : options.inverse();
            }

            return result
                    ? options.hash("yes", true)
                    : options.hash("no", false);
        }
    }
}
