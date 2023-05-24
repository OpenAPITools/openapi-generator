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
import java.util.Locale;

import static org.openapitools.codegen.utils.StringUtils.underscore;

/**
 * Converts text in a fragment to snake case.
 *
 * Register:
 * <pre>
 * additionalProperties.put("snakecase", new SnakecaseLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#snakecase}}{{summary}}{{/snakecase}}
 * </pre>
 */
public class SnakecaseLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        writer.write(underscore(fragment.execute()));
    }
}
