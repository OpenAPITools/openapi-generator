
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
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text in a fragment from camelCase or PascalCase to a space separated string
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("uncamelize", new UncamelizeLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#uncamelize}}{{name}}{{/uncamelize}}
 * </pre>
 */
public class UncamelizeLambda implements Mustache.Lambda {

    public UncamelizeLambda() {
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String input = fragment.execute();
        String text = StringUtils.capitalize(StringUtils.join(StringUtils.splitByCharacterTypeCamelCase(input.trim()), StringUtils.SPACE));
        writer.write(text.trim().replaceAll(" +", " "));
    }
}

