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

import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

/**
 * Replaces duplicate line break characters in a fragment with single line break.
 *
 * Register:
 * <pre>
 * additionalProperties.put("trimLineBreaks", new TrimLineBreaksLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#trimLineBreaks}}{{name}}{{/trimLineBreaks}}
 * </pre>
 */
public class TrimLineBreaksLambda implements Mustache.Lambda {
    private static final String SINGLE_LINE_BREAK = "\n\n";

    private static final String LINE_BREAK_REGEX = "\n\n+";

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        writer.write(fragment.execute().replaceAll(LINE_BREAK_REGEX, SINGLE_LINE_BREAK));
    }
}
