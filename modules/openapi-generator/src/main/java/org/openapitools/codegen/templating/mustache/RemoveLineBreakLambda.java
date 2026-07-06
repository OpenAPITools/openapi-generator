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
 * Removes line break characters from a fragment.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("lambdaRemoveLineBreak", new RemoveLineBreakLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#lambdaRemoveLineBreak}}{{name}}{{/lambdaRemoveLineBreak}}
 * </pre>
 */
public class RemoveLineBreakLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        fragment.execute(new RemoveLineBreakWriter(writer));
    }

    private static class RemoveLineBreakWriter extends ForwardingWriter {
        private RemoveLineBreakWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int end = off + len;
            int writeStart = off;
            for (int i = off; i < end; i++) {
                if (cbuf[i] == '\r' || cbuf[i] == '\n') {
                    if (writeStart < i) {
                        writer.write(cbuf, writeStart, i - writeStart);
                    }
                    writeStart = i + 1;
                }
            }
            if (writeStart < end) {
                writer.write(cbuf, writeStart, end - writeStart);
            }
        }

    }
}
