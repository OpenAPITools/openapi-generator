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
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Split text by the delimiter and then write only the unique entries
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("unique", new UniqueLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#unique}}{{name}}{{/unique}}
 * </pre>
 */
public class UniqueLambda implements Mustache.Lambda {
    private static final String REGEX_META_CHARS = "\\.[]{}()*+-?^$|";

    private final String delimiter;
    private final boolean withNewLine;

    public UniqueLambda(String delimiter, boolean withNewLine) {
        this.delimiter = delimiter;
        this.withNewLine = withNewLine;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {

        if (isPlainDelimiter()) {
            UniqueWriter uniqueWriter = new UniqueWriter(writer);
            fragment.execute(uniqueWriter);
            uniqueWriter.finish();
        } else {
            writer.write(Arrays.stream(fragment.execute().split(this.delimiter, -1))
                    .distinct()
                    .collect(Collectors.joining(delimiter)));
        }

        if (withNewLine) {
            writer.write("\n");
        }
    }

    private boolean isPlainDelimiter() {
        return !delimiter.isEmpty() && delimiter.chars().noneMatch(ch -> REGEX_META_CHARS.indexOf(ch) >= 0);
    }

    private class UniqueWriter extends ForwardingWriter {
        private final Set<String> values = new LinkedHashSet<>();
        private final StringBuilder buffer = new StringBuilder();
        private boolean first = true;
        private boolean finished = false;

        private UniqueWriter(Writer writer) {
            super(writer);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            buffer.append(cbuf, off, len);
            finished = false;
            writeCompleteValues();
        }

        @Override
        public void close() throws IOException {
            finish();
        }

        private void writeCompleteValues() throws IOException {
            int valueStart = 0;
            int delimiterIndex = buffer.indexOf(delimiter, valueStart);
            while (delimiterIndex >= 0) {
                writeUniqueValue(buffer.substring(valueStart, delimiterIndex));
                valueStart = delimiterIndex + delimiter.length();
                delimiterIndex = buffer.indexOf(delimiter, valueStart);
            }
            if (valueStart > 0) {
                buffer.delete(0, valueStart);
            }
        }

        private void finish() throws IOException {
            if (finished) {
                return;
            }
            writeUniqueValue(buffer.toString());
            buffer.setLength(0);
            finished = true;
        }

        private void writeUniqueValue(String value) throws IOException {
            if (values.add(value)) {
                if (!first) {
                    writer.write(delimiter);
                }
                writer.write(value);
                first = false;
            }
        }
    }
}
