package org.openapitools.codegen.templating;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.io.AbstractTemplateLoader;
import com.github.jknack.handlebars.io.StringTemplateSource;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.github.jknack.handlebars.io.TemplateSource;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;

import java.io.IOException;
import java.util.Map;


public class HandlebarsEngineAdapter implements TemplatingEngineAdapter {

    public String[] extensions = new String[]{"handlebars", "hbs"};

    public String compileTemplate(TemplatingGenerator generator,
                                  Map<String, Object> bundle, String templateFile) throws IOException {
        TemplateLoader loader = new AbstractTemplateLoader() {
            @Override
            public TemplateSource sourceAt(String location) {
                return findTemplate(generator, location);
            }
        };

        Handlebars handlebars = new Handlebars(loader);
        handlebars.registerHelperMissing((context, options) -> "");
        Template tmpl = handlebars.compile(templateFile);
        return tmpl.apply(bundle);
    }

    public TemplateSource findTemplate(TemplatingGenerator generator, String name) {
        for (String extension : extensions) {
            try {
                String location = name + "." + extension;
                return new StringTemplateSource(location, generator.getFullTemplateContents(location));
            } catch (Exception ignored) {
            }
        }
        throw new RuntimeException("couldnt find a subtemplate " + name);
    }

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }
}

