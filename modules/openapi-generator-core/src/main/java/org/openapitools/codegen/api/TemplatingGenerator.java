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
