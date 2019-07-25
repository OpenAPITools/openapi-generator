/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.api;

import java.io.IOException;
import java.util.Map;

/**
 * Each templating engine is called by an Adapter, selected at runtime
 */
public interface TemplatingEngineAdapter{

  /**
   * Provides an identifier used to load the adapter. This could be a name, uuid, or any other string.
   *
   * @return A string identifier.
   */
  String getIdentifier();

  /**
   * Compiles a template into a string
   *
   * @param generator From where we can fetch the templates content (e.g. an instance of DefaultGenerator)
   * @param bundle The map of values to pass to the template
   * @param templateFile The name of the template (e.g. model.mustache )
   * @return the processed template result
   * @throws IOException an error ocurred in the template processing
   */
  String compileTemplate(TemplatingGenerator generator, Map<String, Object> bundle,
                         String templateFile) throws IOException;

  /**
   * During generation, if a supporting file has a file extension that is
   * inside that array, then it is considered a templated supporting file
   * and we use the templating engine adapter to generate it
   * @return string array of the valid file extensions for this templating engine
   */
  String[] getFileExtensions();
}
