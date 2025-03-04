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

/**
 * Converts text in a fragment to snake case.
 * <p>
 * Register:
 * <pre>
 * additionalProperties.put("kebabcase", new KebabCaseLambda());
 * </pre>
 * <p>
 * Use:
 * <pre>
 * {{#kebabcase}}{{summary}}{{/kebabcase}}
 * </pre>
 */
public class KebabCaseLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();

        String SPECIAL = "[^A-Za-z0-9_]";
        String firstPattern = "([A-Z]+)([A-Z][a-z])";
        String secondPattern = "([a-z\\d])([A-Z])";
        String replacementPattern = "$1 $2";
        // Replace package separator with slash.
        text = text.replaceAll("\\.", "/");
        text = text.replaceAll(SPECIAL, "");
        // Replace capital letter with _ plus lowercase letter.
        text = text.replaceAll(firstPattern, replacementPattern);
        text = text.replaceAll(secondPattern, replacementPattern);
        text = text.replace('_', '-');
        text = text.replace(' ', '-');
        text = text.toLowerCase(Locale.ROOT);

        writer.write(text);
    }
}