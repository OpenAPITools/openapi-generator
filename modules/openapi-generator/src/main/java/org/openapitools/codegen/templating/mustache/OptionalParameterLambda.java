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
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.AbstractCSharpCodegen;

import java.io.IOException;
import java.io.Writer;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Appends trailing ? to a text fragement if not already present
 *
 * Register:
 * <pre>
 * additionalProperties.put("optional", new OptionalParameterLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#lambda.optional}}{{name}}{{/lambda.optional}}
 * </pre>
 */
public class OptionalParameterLambda implements Mustache.Lambda {
    private CodegenConfig generator = null;
    private Boolean escapeParam = false;

    public OptionalParameterLambda() {}

    public OptionalParameterLambda generator(final CodegenConfig generator) {
        this.generator = generator;
        return this;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();

        if (this.generator instanceof AbstractCSharpCodegen){
            AbstractCSharpCodegen csharpGenerator = (AbstractCSharpCodegen) this.generator;
            if (csharpGenerator.getNullableReferencesTypes()){
                text = text.endsWith("?")
                    ? text
                    : text + "?";
            }
        }

        writer.write(text);
    }
}
