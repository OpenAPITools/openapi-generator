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

/**
 * Escapes HTML-sensitive characters in a fragment.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("lambdaEscapeHtml", new EscapeHtmlLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#lambdaEscapeHtml}}{{name}}{{/lambdaEscapeHtml}}
 * </pre>
 */
public class EscapeHtmlLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        HtmlEscapingWriter escapingWriter = new HtmlEscapingWriter(writer);
        fragment.execute(escapingWriter);
        escapingWriter.flushBuffer();
    }

    private static class HtmlEscapingWriter extends ForwardingWriter {
        private final StringBuilder buffer = new StringBuilder();

        private HtmlEscapingWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) {
            buffer.append(cbuf, off, len);
        }

        private void flushBuffer() throws IOException {
            writer.write(StringEscapeUtils.escapeHtml4(buffer.toString()));
        }
    }
}
