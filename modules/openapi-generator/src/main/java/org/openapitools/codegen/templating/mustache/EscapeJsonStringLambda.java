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

import java.io.IOException;
import java.io.Writer;

/**
 * Escapes fragment output for use inside JSON string values.
 */
public class EscapeJsonStringLambda implements Mustache.Lambda {
    private static final char[] HEX = "0123456789abcdef".toCharArray();

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        fragment.execute(new EscapeJsonStringWriter(writer));
    }

    private static class EscapeJsonStringWriter extends ForwardingWriter {
        private EscapeJsonStringWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int end = off + len;
            int writeStart = off;
            for (int i = off; i < end; i++) {
                String replacement = replacementFor(cbuf[i]);
                if (replacement != null) {
                    if (writeStart < i) {
                        writer.write(cbuf, writeStart, i - writeStart);
                    }
                    writer.write(replacement);
                    writeStart = i + 1;
                }
            }
            if (writeStart < end) {
                writer.write(cbuf, writeStart, end - writeStart);
            }
        }

        private String replacementFor(char ch) {
            switch (ch) {
                case '\\':
                    return "\\\\";
                case '"':
                    return "\\\"";
                case '\n':
                    return "\\n";
                case '\r':
                    return "\\r";
                case '\t':
                    return "\\t";
                case '\b':
                    return "\\b";
                case '\f':
                    return "\\f";
                default:
                    if (ch < 0x20) {
                        return unicodeEscape(ch);
                    }
                    return null;
            }
        }

        private String unicodeEscape(char ch) {
            return new String(new char[]{
                    '\\',
                    'u',
                    HEX[(ch >> 12) & 0xf],
                    HEX[(ch >> 8) & 0xf],
                    HEX[(ch >> 4) & 0xf],
                    HEX[ch & 0xf]
            });
        }
    }
}
