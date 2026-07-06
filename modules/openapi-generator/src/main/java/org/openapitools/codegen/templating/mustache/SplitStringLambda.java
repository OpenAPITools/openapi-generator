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
import com.samskivert.mustache.Template.Fragment;

import java.io.IOException;
import java.io.Writer;

/**
 * Splits long fragments into smaller strings and uses a StringBuilder to merge
 * them back.
 * <p>
 * Register:
 *
 * <pre>
 * additionalProperties.put("lambdaSplitString", new SplitStringLambda());
 * </pre>
 * <p>
 * Use:
 *
 * <pre>
 * {{#lambdaSplitString}}{{summary}}{{/lambdaSplitString}}
 * </pre>
 */
public class SplitStringLambda implements Mustache.Lambda {
    private static final int DEFAULT_MAX_LENGTH = 65535;

    private static final String SPLIT_INIT = "new StringBuilder()";

    private static final String SPLIT_PART_PREFIX = ".append(\"";

    private static final String SPLIT_PART_SUFFIX = "\")";

    private static final String SPLIT_SUFFIX = ".toString()";

    private final int maxLength;

    public SplitStringLambda() {
        this(DEFAULT_MAX_LENGTH);
    }

    public SplitStringLambda(int maxLength) {
        this.maxLength = maxLength;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        SplitStringWriter splitStringWriter = new SplitStringWriter(writer, maxLength);
        fragment.execute(splitStringWriter);
        splitStringWriter.finish();
    }

    private static class SplitStringWriter extends ForwardingWriter {
        private final int maxLength;
        private final StringBuilder bufferedInput = new StringBuilder();
        private final StringBuilder currentPart = new StringBuilder();
        private boolean split = false;

        private SplitStringWriter(Writer writer, int maxLength) {
            super(writer);
            this.maxLength = maxLength;
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            int end = off + len;
            if (!split && bufferedInput.length() + len <= maxLength) {
                bufferedInput.append(cbuf, off, len);
                return;
            }

            if (!split) {
                split = true;
                writer.write(SPLIT_INIT);
                writeSplit(bufferedInput);
                bufferedInput.setLength(0);
            }

            for (int i = off; i < end; i++) {
                appendSplit(cbuf[i]);
            }
        }

        private void writeSplit(CharSequence value) throws IOException {
            for (int i = 0; i < value.length(); i++) {
                appendSplit(value.charAt(i));
            }
        }

        private void appendSplit(char c) throws IOException {
            currentPart.append(c);
            if (currentPart.length() > maxLength) {
                int splitLength = maxLength - 1;
                if (splitLength == 0) {
                    return;
                }
                writePart(currentPart.subSequence(0, splitLength));
                currentPart.delete(0, splitLength);
            } else if (currentPart.length() == maxLength && currentPart.charAt(currentPart.length() - 1) != '\\') {
                writePart(currentPart);
                currentPart.setLength(0);
            }
        }

        private void finish() throws IOException {
            if (split) {
                if (currentPart.length() > 0) {
                    writePart(currentPart);
                    currentPart.setLength(0);
                }
                writer.write(SPLIT_SUFFIX);
            } else {
                writer.write('"');
                writer.write(bufferedInput.toString());
                writer.write('"');
            }
        }

        private void writePart(CharSequence part) throws IOException {
            writer.write(SPLIT_PART_PREFIX);
            writer.append(part);
            writer.write(SPLIT_PART_SUFFIX);
        }

    }

}
