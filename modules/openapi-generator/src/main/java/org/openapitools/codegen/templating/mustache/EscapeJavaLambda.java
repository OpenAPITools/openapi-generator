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
import org.apache.commons.text.StringEscapeUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Escapes the text so it can be safely embedded in a Java string or character
 * literal (escapes quotes, backslashes and control characters such as newlines).
 * <p>
 * The value is a no-op for normal identifier-like text, so it does not change
 * output for values that were already valid Java literal content.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("escapeJava", new EscapeJavaLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#escapeJava}}{{{summary}}}{{/escapeJava}}
 * </pre>
 */
public class EscapeJavaLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        writer.write(StringEscapeUtils.escapeJava(fragment.execute()));
    }
}
