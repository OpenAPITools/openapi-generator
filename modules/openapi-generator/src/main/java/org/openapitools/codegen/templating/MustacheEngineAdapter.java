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
