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

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Jackson2Helper;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.context.JavaBeanValueResolver;
import com.github.jknack.handlebars.context.MapValueResolver;
import com.github.jknack.handlebars.context.MethodValueResolver;
import com.github.jknack.handlebars.helper.ConditionalHelpers;
import com.github.jknack.handlebars.helper.StringHelpers;
import com.github.jknack.handlebars.io.AbstractTemplateLoader;
import com.github.jknack.handlebars.io.StringTemplateSource;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.github.jknack.handlebars.io.TemplateSource;
import lombok.Setter;
import org.openapitools.codegen.api.AbstractTemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingExecutor;
import org.openapitools.codegen.templating.handlebars.AccessAwareFieldValueResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class HandlebarsEngineAdapter extends AbstractTemplatingEngineAdapter {
    final Logger LOGGER = LoggerFactory.getLogger(HandlebarsEngineAdapter.class);
    private final String[] extensions = {"handlebars", "hbs"};

    // We use this as a simple lookup for valid file name extensions. This adapter will inspect .mustache (built-in) and infer the relevant handlebars filename
    private final String[] canCompileFromExtensions = {".handlebars", ".hbs", ".mustache"};
    private boolean infiniteLoops = false;
    @Setter
    private boolean prettyPrint = false;

    /**
     * Per-executor cache of fully-configured {@link Handlebars} engine instances.
     * Each executor gets its own engine because the engine's {@link TemplateLoader} closes over the
     * executor; sharing an engine across executors would silently resolve templates from the wrong source.
     * {@link ConcurrentHashMap#computeIfAbsent} ensures the engine is built at most once per executor.
     */
    private final ConcurrentHashMap<TemplatingExecutor, Handlebars> engineCache = new ConcurrentHashMap<>();

    /**
     * Per-executor cache of compiled {@link Template} objects.
     * Keying on the executor instance eliminates the non-atomic check-clear-update invalidation
     * that the previous single-cache approach required; no state ever needs to be cleared.
     */
    private final ConcurrentHashMap<TemplatingExecutor, ConcurrentHashMap<String, Template>> templateCaches =
            new ConcurrentHashMap<>();

    /**
     * Provides an identifier used to load the adapter. This could be a name, uuid, or any other string.
     *
     * @return A string identifier.
     */
    @Override
    public String getIdentifier() {
        return "handlebars";
    }

    @Override
    public String compileTemplate(TemplatingExecutor executor,
                                  Map<String, Object> bundle, String templateFile) throws IOException {
        Context context = Context
                .newBuilder(bundle)
                .resolver(
                        MapValueResolver.INSTANCE,
                        JavaBeanValueResolver.INSTANCE,
                        MethodValueResolver.INSTANCE,
                        AccessAwareFieldValueResolver.INSTANCE)
                .build();

        // Each executor gets its own Handlebars engine (the loader closes over the executor) and its
        // own compiled-template cache. computeIfAbsent is atomic, so concurrent calls with the same
        // executor share one engine/cache rather than racing to create duplicates.
        Handlebars handlebars = engineCache.computeIfAbsent(executor, this::buildHandlebars);
        ConcurrentHashMap<String, Template> cache =
                templateCaches.computeIfAbsent(executor, k -> new ConcurrentHashMap<>());

        // Manual get → compile → put so IOException propagates naturally.
        Template tmpl = cache.get(templateFile);
        if (tmpl == null) {
            tmpl = handlebars.compile(templateFile);
            cache.put(templateFile, tmpl);
        }
        return tmpl.apply(context);
    }

    /** Constructs and fully configures a {@link Handlebars} engine for the given executor. */
    private Handlebars buildHandlebars(TemplatingExecutor executor) {
        TemplateLoader loader = new AbstractTemplateLoader() {
            @Override
            public TemplateSource sourceAt(String location) {
                return findTemplate(executor, location);
            }
        };
        Handlebars handlebars = new Handlebars(loader);
        handlebars.registerHelperMissing((obj, options) -> {
            LOGGER.warn("Unregistered helper name '{}', processing template:\n{}", options.helperName, options.fn.text());
            return "";
        });
        handlebars.registerHelper("json", Jackson2Helper.INSTANCE);
        StringHelpers.register(handlebars);
        handlebars.registerHelpers(ConditionalHelpers.class);
        handlebars.registerHelpers(org.openapitools.codegen.templating.handlebars.StringHelpers.class);
        handlebars.setInfiniteLoops(infiniteLoops);
        handlebars.setPrettyPrint(prettyPrint);
        return handlebars;
    }

    @SuppressWarnings("java:S108")
    public TemplateSource findTemplate(TemplatingExecutor generator, String templateFile) {
        String[] possibilities = getModifiedFileLocation(templateFile);
        for (String file : possibilities) {
            try {
                return new StringTemplateSource(file, generator.getFullTemplateContents(file));
            } catch (Exception ignored) {
            }
        }

        // allow lookup of files without extension modification (such as .openapi-generator-ignore, README.md, etc)
        try {
            return new StringTemplateSource(templateFile, generator.getFullTemplateContents(templateFile));
        } catch (Exception ignored) {
        }

        throw new TemplateNotFoundException(String.join(", ", possibilities));
    }

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }

    /**
     * Determine if the adapter handles compilation of the file
     *
     * @param filename The template filename
     * @return True if the file should be compiled by this adapter, else false.
     */
    @Override
    public boolean handlesFile(String filename) {
        // disallow any extension-only files like ".hbs" or ".mustache", and only consider a file compilable if it's handlebars or mustache (from which we later infer the handlebars filename)
        return Arrays.stream(canCompileFromExtensions).anyMatch(suffix -> !suffix.equalsIgnoreCase(filename) && filename.endsWith(suffix));
    }

    /**
     * Enable/disable infiniteLoops setting for the Handlebars engine. Enabling this allows for recursive partial inclusion.
     *
     * @param infiniteLoops Whether to enable (true) or disable (false)
     * @return this object
     */
    public HandlebarsEngineAdapter infiniteLoops(boolean infiniteLoops) {
        this.infiniteLoops = infiniteLoops;
        return this;
    }

}

