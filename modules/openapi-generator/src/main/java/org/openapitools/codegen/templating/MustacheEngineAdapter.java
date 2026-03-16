/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.templating;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingExecutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


public class MustacheEngineAdapter implements TemplatingEngineAdapter {

    private final Logger LOGGER = LoggerFactory.getLogger(TemplatingEngineAdapter.class);

    /**
     * Provides an identifier used to load the adapter. This could be a name, uuid, or any other string.
     *
     * @return A string identifier.
     */
    @Override
    public String getIdentifier() {
        return "mustache";
    }

    private final String[] extensions = {"mustache"};
    @Getter @Setter
    Mustache.Compiler compiler = Mustache.compiler();

    /**
     * Cache of template file name -> compiled Template object.
     * Templates are stateless after compilation and safe to reuse across invocations
     * <em>for the same executor</em>. The cache is invalidated whenever the executor changes.
     */
    private final Map<String, Template> compiledTemplateCache = new ConcurrentHashMap<>();

    /**
     * Tracks the executor whose template sources were used to populate {@link #compiledTemplateCache}.
     * A different executor may resolve the same template name to different content (e.g. user-defined
     * templates vs. built-ins, or different test fixtures), so the cache must be cleared on change.
     */
    private volatile TemplatingExecutor cachedExecutor;

    /**
     * Compiles a template into a string
     *
     * @param executor     From where we can fetch the templates content (e.g. an instance of DefaultGenerator)
     * @param bundle       The map of values to pass to the template
     * @param templateFile The name of the template (e.g. model.mustache )
     * @return the processed template result
     * @throws IOException an error occurred in the template processing
     */
    @Override
    public String compileTemplate(TemplatingExecutor executor, Map<String, Object> bundle, String templateFile) throws IOException {
        // If the executor changed, a different set of template sources may be in use; invalidate
        // stale compiled templates so we don't return results built from the old executor.
        if (cachedExecutor != executor) {
            compiledTemplateCache.clear();
            cachedExecutor = executor;
        }

        // Manual get → compile → put so the correct (current) executor is always captured in
        // the partial loader, and so that any compile-time exception propagates naturally.
        Template tmpl = compiledTemplateCache.get(templateFile);
        if (tmpl == null) {
            tmpl = compiler
                    .withLoader(name -> findTemplate(executor, name))
                    .defaultValue("")
                    .compile(executor.getFullTemplateContents(templateFile));
            compiledTemplateCache.put(templateFile, tmpl);
        }
        StringWriter out = new StringWriter();

        // the value of bundle[MUSTACHE_PARENT_CONTEXT] is used a parent content in mustache.
        // See description in https://mustache.github.io/mustache.5.html#Variables
        // See DefaultCodegen.processOpts() and DefaultCodegen.useCodegenAsMustacheParentContext
        Object parent = bundle.get(CodegenConstants.MUSTACHE_PARENT_CONTEXT);
        if (parent == null) {
            LOGGER.warn("{} not found. super.processOpts needs to be called in processOpts()", CodegenConstants.MUSTACHE_PARENT_CONTEXT);
            // avoid NPE
            parent = new Object();
        }
        tmpl.execute(bundle, parent, out);
        return out.toString();
    }

    @SuppressWarnings("java:S108") // catch-all is expected, and is later thrown
    public Reader findTemplate(TemplatingExecutor generator, String name) {
        for (String extension : extensions) {
            final String templateName = name + "." + extension;
            try {
                return new StringReader(generator.getFullTemplateContents(templateName));
            } catch (Exception exception) {
                LOGGER.error("Failed to read full template {}, {}", templateName, exception.getMessage());
            }
        }

        throw new TemplateNotFoundException(name);
    }

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }
}
