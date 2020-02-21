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

import java.io.IOException;
import java.io.Writer;
import java.util.Locale;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

/**
 * Splits long fragments into smaller strings and uses a StringBuilder to merge
 * them back.
 *
 * Register:
 *
 * <pre>
 * additionalProperties.put("lambdaSplitString", new SplitStringLambda());
 * </pre>
 *
 * Use:
 *
 * <pre>
 * {{#lambdaSplitString}}{{summary}}{{/lambdaSplitString}}
 * </pre>
 */
public class SplitStringLambda implements Mustache.Lambda {
    private static final int DEFAULT_MAX_LENGTH = 65535;

    private static final String SPLIT_INIT = "new StringBuilder(%d)";

    private static final String SPLIT_PART = ".append(\"%s\")";

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
        String input = fragment.execute();
        int inputLength = input.length();

        StringBuilder builder = new StringBuilder();
        if (inputLength > maxLength) {

            // Initialize a StringBuilder
            builder.append(String.format(Locale.ROOT, SPLIT_INIT, inputLength));

            int currentPosition = 0;
            int currentStringLength = 0;
            char currentLastChar = '\\';

            // Split input into parts of at most maxLength and not ending with an escape character
            // Append each part to the StringBuilder
            while (currentPosition + maxLength < input.length()) {
                currentStringLength = maxLength;
                currentLastChar = input.charAt(currentPosition + currentStringLength - 1);
                if (currentLastChar == '\\') {
                    --currentStringLength;
                }

                builder.append(String.format(Locale.ROOT, SPLIT_PART, input.substring(currentPosition, currentPosition + currentStringLength)));
                currentPosition += currentStringLength;
            }

            // Append last part if necessary
            if (currentPosition < input.length()) {
                builder.append(String.format(Locale.ROOT, SPLIT_PART, input.substring(currentPosition)));
            }

            // Close the builder and merge everything back to a string
            builder.append(SPLIT_SUFFIX);
        } else {
            builder.append(String.format(Locale.ROOT, "\"%s\"", input));
        }

        writer.write(builder.toString());
    }

}
