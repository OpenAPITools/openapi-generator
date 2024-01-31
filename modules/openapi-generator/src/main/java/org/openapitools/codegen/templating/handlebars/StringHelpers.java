package org.openapitools.codegen.templating.handlebars;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.TagType;

import java.io.IOException;
import java.util.Locale;

public enum StringHelpers implements Helper<Object> {

    /**
     * Indicates whether the string starts with the given value.
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
    },

    /**
     * Indicates whether the string ends with the given value.
     * For example:
     *
     * <pre>
     * {{endsWith a b ["insensitive"]}}
     * </pre>
     *
     * <pre>
     * {{endsWith a text='b' [insensitive=true]}}
     * </pre>
     *
     * Render 'yes' or 'no':
     * <pre>
     *   {{#endsWith a b}}
     *     yes
     *   {{else}}
     *     no
     *   {{/endsWith}}
     * </pre>
     *
     * Render 'true' or 'false':
     * <pre>
     *   {{endsWith a b}}
     * </pre>
     *
     * Render 'y' or 'n':
     * <pre>
     *   {{endsWith a b yes='y' no='n'}}
     * </pre>
     */
    endsWith {
        @Override
        public Object apply(Object value, Options options) throws IOException {
            String match = options.param(0, options.hash("text", ""));
            if (match.length() < 1) {
                return false;
            }

            boolean caseInsensitive = options.hash("insensitive", false);
            boolean result = caseInsensitive
                    ? value.toString().toLowerCase(Locale.ROOT).endsWith(match.toLowerCase(Locale.ROOT))
                    : value.toString().endsWith(match);

            if (options.tagType == TagType.SECTION) {
                return result ? options.fn() : options.inverse();
            }

            return result
                    ? options.hash("yes", true)
                    : options.hash("no", false);
        }
    }
}
