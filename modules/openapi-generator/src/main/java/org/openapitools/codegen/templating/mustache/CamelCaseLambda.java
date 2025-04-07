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
import org.openapitools.codegen.utils.CamelizeOption;

import java.io.IOException;
import java.io.Writer;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Converts text in a fragment to camelCase.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("camelcase", new CamelCaseLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#camelcase}}{{name}}{{/camelcase}}
 * </pre>
 */
public class CamelCaseLambda implements Mustache.Lambda {
    public CamelCaseLambda() {
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        text = camelize(text, CamelizeOption.LOWERCASE_FIRST_CHAR);
        writer.write(text);
    }
}