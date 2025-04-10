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
 * Replaces all regex captures with the provided string.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("regex", new ReplaceAllLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#regex}}{{summary}}{{/regex}}
 * </pre>
 */
public class ReplaceAllLambda implements Mustache.Lambda {
    private String regex;
    private String replacement;

    public ReplaceAllLambda(String regex, String replacement) {
        this.regex = regex;
        this.replacement = replacement;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        writer.write(fragment.execute()
                .replaceAll(regex, replacement));
    }
}
