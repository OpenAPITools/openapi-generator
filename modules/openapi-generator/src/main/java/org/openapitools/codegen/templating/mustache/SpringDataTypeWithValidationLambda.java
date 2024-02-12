package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;
import java.io.IOException;
import java.io.Writer;
import org.openapitools.codegen.CodegenProperty;

public class SpringDataTypeWithValidationLambda implements Mustache.Lambda {

  private final boolean openApiNullable;
  private final boolean useOptional;

  public SpringDataTypeWithValidationLambda(boolean openApiNullable, boolean useOptional) {
    this.openApiNullable = openApiNullable;
    this.useOptional = useOptional;
  }


  @Override
  public void execute(Fragment fragment, Writer writer) throws IOException {
    CodegenProperty property = (CodegenProperty) fragment.context();
    if (openApiNullable) {
      if (property.isNullable) {
        writer.write("JsonNullable<");
        writer.write(property.datatypeWithEnum);
        writer.write('>');
      } else{
        boolean ofNullable = (!property.required && !property.isNullable && !property.isContainer);
        if (ofNullable) {
          writer.write("Optional<");
          writer.write(property.datatypeWithEnum);
          writer.write('>');
        } else {
          writer.write(property.datatypeWithEnum);
        }
      }

    } else {
      writer.write(property.datatypeWithEnum);
    }
  }
//

  }
