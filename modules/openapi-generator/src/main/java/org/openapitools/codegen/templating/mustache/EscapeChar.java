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
 * Escapes the desired character if not escaped already, e.g. {@code $ => \$}.
 *
 * Register:
 * {@code additionalProperties.put("escapeDollar", new EscapeChar("(?<!\\\\)\\$", "\\\\\\$")); }
 *
 * Use:
 * {@code {{#lambda.escapeDollar}}{{name}}{{/lambda.escapeDollar}} }
 */
public class EscapeChar implements Mustache.Lambda {
    private final String matchPattern;
    private final String replacement;

    /**
     * Constructs a new instance of {@link EscapeChar}, with the desired character to escape
     * 
     * @param matchPattern the character to escape
     * @param replacement the escaped character
     */
    public EscapeChar(String matchPattern, String replacement) {
        this.matchPattern = matchPattern;
        this.replacement = replacement;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        text = text.replaceAll(matchPattern, replacement);
        writer.write(text);
    }
}
