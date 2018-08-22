package org.openapitools.codegen.api;

import java.io.IOException;
import java.util.Map;

/**
 * Each templating engine is called by an Adapter, selected at runtime
 */
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

  /**
   * During generation, if a supporting file has a file extension that is
   * inside that array, then it is considered a templated supporting file
   * and we use the templating engine adapter to generate it
   * @return string array of the valid file extensions for this templating engine
   */
  String[] getFileExtensions();
}
