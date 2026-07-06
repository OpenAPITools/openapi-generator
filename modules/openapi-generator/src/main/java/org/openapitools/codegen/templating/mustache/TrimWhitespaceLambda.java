/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;

/**
 * Replaces duplicate whitespace characters in a fragment with single space.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#lambdaTrimWhitespace}}{{name}}{{/lambdaTrimWhitespace}}
 * </pre>
 */
public class TrimWhitespaceLambda implements Mustache.Lambda {
    private static final char SINGLE_SPACE = ' ';

    /**
     * Preserve the default Java regex \s character class: [ \t\n\x0B\f\r].
     */
    private static boolean isRegexWhitespace(char ch) {
        return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\u000B' || ch == '\f' || ch == '\r';
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        fragment.execute(new TrimWhitespaceWriter(writer));
    }

    private static class TrimWhitespaceWriter extends ForwardingWriter {
        private boolean inWhitespace = false;

        private TrimWhitespaceWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int end = off + len;
            int writeStart = off;
            for (int i = off; i < end; i++) {
                if (isRegexWhitespace(cbuf[i])) {
                    if (writeStart < i) {
                        writer.write(cbuf, writeStart, i - writeStart);
                    }
                    if (!inWhitespace) {
                        writer.write(SINGLE_SPACE);
                    }
                    inWhitespace = true;
                    writeStart = i + 1;
                } else {
                    inWhitespace = false;
                }
            }
            if (writeStart < end) {
                writer.write(cbuf, writeStart, end - writeStart);
            }
        }

    }

}
