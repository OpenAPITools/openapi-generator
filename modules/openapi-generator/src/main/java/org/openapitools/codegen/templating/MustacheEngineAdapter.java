package org.openapitools.codegen.templating;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;


public class MustacheEngineAdapter implements TemplatingEngineAdapter {

    Mustache.Compiler compiler = Mustache.compiler();

    @Override
    public String doProcessTemplateToFile(TemplatingGenerator generator, Map<String, Object> bundle,
                                          String templateFile) throws IOException {

        Template tmpl = compiler
                .withLoader(name -> new StringReader(generator.getFullTemplateContents(name + "." + getFileExtension())))
                .defaultValue("")
                .compile(generator.getFullTemplateContents(templateFile));

        return tmpl.execute(bundle);
    }

    @Override
    public String getFileExtension() {
        return "mustache";
    }

    public Mustache.Compiler getCompiler() {
        return compiler;
    }

    public void setCompiler(Mustache.Compiler compiler) {
        this.compiler = compiler;
    }
}
