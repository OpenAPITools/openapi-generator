/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.templating;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Map;


public class MustacheEngineAdapter implements TemplatingEngineAdapter {

    /**
     * Provides an identifier used to load the adapter. This could be a name, uuid, or any other string.
     *
     * @return A string identifier.
     */
    @Override
    public String getIdentifier() {
        return "mustache";
    }

    public String[] extensions = new String[]{"mustache"};
    Mustache.Compiler compiler = Mustache.compiler();

    @Override
    public String compileTemplate(TemplatingGenerator generator, Map<String, Object> bundle,
                                  String templateFile) throws IOException {
        Template tmpl = compiler
                .withLoader(name -> findTemplate(generator, name))
                .defaultValue("")
                .compile(generator.getFullTemplateContents(templateFile));

        return tmpl.execute(bundle);
    }

    public Reader findTemplate(TemplatingGenerator generator, String name) {
        for (String extension : extensions) {
            try {
                return new StringReader(generator.getFullTemplateContents(name + "." + extension));
            } catch (Exception ignored) {
            }
        }
        throw new RuntimeException("couldnt find a subtemplate " + name);
    }

    public Mustache.Compiler getCompiler() {
        return compiler;
    }

    public void setCompiler(Mustache.Compiler compiler) {
        this.compiler = compiler;
    }

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }
}
