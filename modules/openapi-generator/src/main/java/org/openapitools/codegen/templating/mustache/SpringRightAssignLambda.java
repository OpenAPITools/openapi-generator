package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;
import java.io.IOException;
import java.io.Writer;
import org.openapitools.codegen.CodegenProperty;

public class SpringRightAssignLambda implements Mustache.Lambda {

  private final boolean openApiNullable;
  private final boolean useOptional;

  public SpringRightAssignLambda(boolean openApiNullable, boolean useOptional) {
    this.openApiNullable = openApiNullable;
    this.useOptional = useOptional;
  }

  /**
    {{#openApiNullable}}
   {{#isNullable}}JsonNullable.of({{/isNullable}}
   {{#useOptional}}
    {{^required}}{{^isNullable}}{{^isContainer}}Optional.ofNullable({{/isContainer}}{{/isNullable}}{{/required}}
   {{/useOptional}}

   {{name}}{{#isNullable}}){{/isNullable}}{{#useOptional}}
   {{^required}}{{^isNullable}}{{^isContainer}}){{/isContainer}}
   {{/isNullable}}{{/required}}{{/useOptional}

   */
  @Override
  public void execute(Fragment fragment, Writer writer) throws IOException {
    CodegenProperty property = (CodegenProperty)fragment.context();
    if (openApiNullable) {
      if (property.isNullable) {
        writer.write("JsonNullable.of(");
        writer.write(property.name);
        writer.write(')');
      }


      boolean ofNullable = (!property.required && !property.isNullable && !property.isContainer);
      if (ofNullable) {
        writer.write("Optional.ofNullable(");
        writer.write(property.name);
        writer.write(')');
      } else {
        writer.write(property.name);
      }
      if (property.isNullable) {
        writer.write(')');
      }

      if (property.isNullable) {
        writer.write(")");
      }
    } else {
      writer.write(property.getName());
    }
//
//    String text = fragment.execute();
//    if (text == null || text.length() == 0) {
//      return;
//    }
//    writer.write(text);
  }
}
