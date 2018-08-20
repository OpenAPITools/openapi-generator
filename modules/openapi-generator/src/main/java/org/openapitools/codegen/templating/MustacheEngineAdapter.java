package org.openapitools.codegen.templating;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Map;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;


public class MustacheEngineAdapter implements TemplatingEngineAdapter {

    Mustache.Compiler compiler = Mustache.compiler();

    public Reader findTemplate(TemplatingGenerator generator, String name){
        for (String extension: extensions){
            try{
                return new StringReader(generator.getFullTemplateContents(name + "." + extension));
            } catch(Exception ignored) {
            }
        }
        throw new RuntimeException("couldnt find a subtemplate " + name);
    }

    @Override
    public String doProcessTemplateToFile(TemplatingGenerator generator, Map<String, Object> bundle,
                                          String templateFile) throws IOException {

        Template tmpl = compiler
                .withLoader(name -> findTemplate(generator, name))
                .defaultValue("")
                .compile(generator.getFullTemplateContents(templateFile));

        return tmpl.execute(bundle);
    }

    String[] extensions = new String[]{"mustache"};

    @Override
    public String[] getFileExtensions() {
        return extensions;
    }

    public Mustache.Compiler getCompiler() {
        return compiler;
    }

    public void setCompiler(Mustache.Compiler compiler) {
        this.compiler = compiler;
    }
}
