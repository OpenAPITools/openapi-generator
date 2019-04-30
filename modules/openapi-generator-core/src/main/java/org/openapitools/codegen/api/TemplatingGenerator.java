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

/**
 * interface to the full template content
 *   implementers might take into account the -t cli option,
 *   look in the resources for a language specific template, etc
 */
public interface TemplatingGenerator {

  /**
   * returns the template content by name
   * @param name the template name (e.g. model.mustache)
   * @return the contents of that template
   */
  String getFullTemplateContents(String name);

}
