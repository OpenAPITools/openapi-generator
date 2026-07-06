/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
import org.apache.commons.text.StringEscapeUtils;

import java.io.IOException;
import java.io.Writer;
import java.nio.CharBuffer;

/**
 * Escapes HTML-sensitive characters in a fragment for HTML text content.
 * <p>
 * This lambda is intended for element body text such as {@code <pre><code>...</code></pre>}.
 * Do not use it for HTML attribute values; attribute contexts need escaping rules which match
 * the attribute quoting style.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("lambdaEscapeHtml", new EscapeHtmlLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#lambdaEscapeHtml}}{{{name}}}{{/lambdaEscapeHtml}}
 * </pre>
 */
public class EscapeHtmlLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        HtmlEscapingWriter escapingWriter = new HtmlEscapingWriter(writer);
        fragment.execute(escapingWriter);
        escapingWriter.finish();
    }

    private static class HtmlEscapingWriter extends ForwardingWriter {
        private char pendingHighSurrogate;

        private HtmlEscapingWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int start = off;
            int end = off + len;

            if (pendingHighSurrogate != 0 && start < end) {
                StringEscapeUtils.ESCAPE_HTML4.translate(new String(new char[]{pendingHighSurrogate, cbuf[start]}), writer);
                pendingHighSurrogate = 0;
                start++;
            }

            if (start < end && Character.isHighSurrogate(cbuf[end - 1])) {
                pendingHighSurrogate = cbuf[end - 1];
                end--;
            }

            if (start < end) {
                StringEscapeUtils.ESCAPE_HTML4.translate(CharBuffer.wrap(cbuf, start, end - start), writer);
            }
        }

        private void finish() throws IOException {
            if (pendingHighSurrogate != 0) {
                StringEscapeUtils.ESCAPE_HTML4.translate(String.valueOf(pendingHighSurrogate), writer);
                pendingHighSurrogate = 0;
            }
        }
    }
}
