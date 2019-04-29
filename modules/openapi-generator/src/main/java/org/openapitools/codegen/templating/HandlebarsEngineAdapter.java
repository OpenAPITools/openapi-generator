package org.openapitools.codegen.templating;

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Jackson2Helper;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.context.FieldValueResolver;
import com.github.jknack.handlebars.context.JavaBeanValueResolver;
import com.github.jknack.handlebars.context.MapValueResolver;
import com.github.jknack.handlebars.helper.ConditionalHelpers;
import com.github.jknack.handlebars.helper.StringHelpers;
import com.github.jknack.handlebars.io.AbstractTemplateLoader;
import com.github.jknack.handlebars.io.StringTemplateSource;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.github.jknack.handlebars.io.TemplateSource;
import org.openapitools.codegen.api.AbstractTemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;


public class HandlebarsEngineAdapter extends AbstractTemplatingEngineAdapter {
    static Logger LOGGER = LoggerFactory.getLogger(HandlebarsEngineAdapter.class);
    private final String[] extensions = new String[]{"handlebars", "hbs"};

    public String compileTemplate(TemplatingGenerator generator,
                                  Map<String, Object> bundle, String templateFile) throws IOException {
        TemplateLoader loader = new AbstractTemplateLoader() {
            @Override
            public TemplateSource sourceAt(String location) {
                return findTemplate(generator, location);
            }
        };

        Context context = Context
                .newBuilder(bundle)
                .resolver(
                        MapValueResolver.INSTANCE,
                        JavaBeanValueResolver.INSTANCE,
                        FieldValueResolver.INSTANCE)
                .build();

        Handlebars handlebars = new Handlebars(loader);
        handlebars.registerHelperMissing((obj, options) -> {
            LOGGER.warn(String.format(Locale.ROOT, "Unregistered helper name '%s', processing template:\n%s", options.helperName, options.fn.text()));
            return "";
        });
        handlebars.registerHelper("json", Jackson2Helper.INSTANCE);
        StringHelpers.register(handlebars);
        handlebars.registerHelpers(ConditionalHelpers.class);
        handlebars.registerHelpers(org.openapitools.codegen.templating.handlebars.StringHelpers.class);
        Template tmpl = handlebars.compile(templateFile);
        return tmpl.apply(context);
    }

    public TemplateSource findTemplate(TemplatingGenerator generator, String templateFile) {
        for (String file : getModifiedFileLocation(templateFile)) {
            try {
                return new StringTemplateSource(file, generator.getFullTemplateContents(file));
            } catch (Exception ignored) {
            }
        }
        throw new RuntimeException("couldnt find a subtemplate " + templateFile);
    }

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }
}

