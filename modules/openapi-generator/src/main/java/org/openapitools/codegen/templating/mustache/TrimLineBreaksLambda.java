/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
 * Replaces duplicate line break characters in a fragment with single line break.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("trimLineBreaks", new TrimLineBreaksLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#trimLineBreaks}}{{name}}{{/trimLineBreaks}}
 * </pre>
 */
public class TrimLineBreaksLambda implements Mustache.Lambda {
    private static final int MAX_CONSECUTIVE_LINE_BREAKS = 2;

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        fragment.execute(new TrimLineBreaksWriter(writer));
    }

    private static class TrimLineBreaksWriter extends ForwardingWriter {
        private int lineBreaks = 0;

        private TrimLineBreaksWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int end = off + len;
            int writeStart = off;
            for (int i = off; i < end; i++) {
                if (cbuf[i] == '\n') {
                    if (writeStart < i) {
                        writer.write(cbuf, writeStart, i - writeStart);
                    }
                    if (lineBreaks < MAX_CONSECUTIVE_LINE_BREAKS) {
                        writer.write(cbuf, i, 1);
                    }
                    lineBreaks++;
                    writeStart = i + 1;
                } else if (lineBreaks > 0) {
                    lineBreaks = 0;
                }
            }
            if (writeStart < end) {
                writer.write(cbuf, writeStart, end - writeStart);
            }
        }

    }
}
