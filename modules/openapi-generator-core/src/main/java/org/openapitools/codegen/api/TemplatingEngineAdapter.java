package org.openapitools.codegen.api;

import java.io.IOException;
import java.util.Map;


public interface TemplatingEngineAdapter{

  /**
   * Processes a template into a string
   * @param generator From where we can fetch the templates content (e.g. an instance of DefaultGenerator)
   * @param bundle The map of values to pass to the template
   * @param templateFile The name of the template (e.g. model.mustache )
   * @return the processed template result
   * @throws IOException an error ocurred in the template processing
   */
  String doProcessTemplateToFile(TemplatingGenerator generator, Map<String, Object> bundle,
      String templateFile) throws IOException;

  /*
  Used to determine whether a given supporting file is a template
   */
  String[] getFileExtensions();
}
