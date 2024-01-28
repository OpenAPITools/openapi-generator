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
import java.util.List;
import java.util.stream.Collectors;

/**
 * Split text by the delimiter and then write only the unique entries
 *
 * Register:
 * <pre>
 * additionalProperties.put("unique", new UniqueLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#unique}}{{name}}{{/unique}}
 * </pre>
 */
public class UniqueLambda implements Mustache.Lambda {
    private final String delimiter;
    private final boolean withNewLine;

    public UniqueLambda(String delimiter, boolean withNewLine)
    {
        this.delimiter = delimiter;
        this.withNewLine = withNewLine;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {

        String[] parts = fragment.execute().split(this.delimiter);

        List<String> uniqueLines = Arrays.stream(parts).distinct().collect(Collectors.toList());

        writer.write(String.join(delimiter, uniqueLines));

        if (withNewLine) {
            writer.write("\n");
        }
    }
}
