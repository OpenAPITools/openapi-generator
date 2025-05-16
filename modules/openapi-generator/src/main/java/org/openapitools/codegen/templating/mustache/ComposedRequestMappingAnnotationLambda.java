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
 * Replaces @RequestMapping annotation with the appropriate composed annotation when using <strong>Spring generator</strong>. e.g:
 * <p><code>@RequestMapping(method = RequestMethod.GET</code>
 * will be replaced with:<code>@GetMapping(</code> as it is defined on the file JavaSpring/api.mustache under
 * the {{#useApiComposedAnnotation}} section</p>
 *
 * Register:
 * <pre>
 * additionalProperties.put("composedannotation", new ComposedRequestMappingAnnotationLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#lambda.composedannotation}}{{httpMethod}}{{/lambda.composedannotation}}
 * </pre>
 */
public class ComposedRequestMappingAnnotationLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
            String text = fragment.execute();
            if(text == null || text.isEmpty()) return;
            writer.write(String.format("@%sMapping(",text.substring(0,1).toUpperCase()+text.substring(1).toLowerCase()));
    }
}
