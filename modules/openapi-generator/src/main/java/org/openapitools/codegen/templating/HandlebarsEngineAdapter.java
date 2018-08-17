package org.openapitools.codegen.templating;

import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.github.jknack.handlebars.io.AbstractTemplateLoader;
import com.github.jknack.handlebars.io.StringTemplateSource;
import com.github.jknack.handlebars.io.TemplateLoader;
import com.github.jknack.handlebars.io.TemplateSource;
import java.io.IOException;
import java.util.Map;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingGenerator;


public class HandlebarsEngineAdapter implements TemplatingEngineAdapter {

  public String doProcessTemplateToFile(TemplatingGenerator generator,
      Map<String, Object> bundle, String templateFile) throws IOException {
    TemplateLoader loader = new AbstractTemplateLoader() {
      @Override
      public TemplateSource sourceAt(String location) {
        String templateContent = generator.getFullTemplate(location);
        return new StringTemplateSource(location, templateContent);
      }
    };

    Handlebars handlebars = new Handlebars(loader);
    handlebars.registerHelperMissing((context, options) -> "");
    Template tmpl = handlebars.compile(templateFile);
    return tmpl.apply(bundle);
  }

  @Override
  public String getFileExtension() {
    return "handlebars";
  }
}

