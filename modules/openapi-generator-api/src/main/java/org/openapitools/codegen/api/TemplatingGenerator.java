package org.openapitools.codegen.api;

/*
interface for getting the templates
 */
public interface TemplatingGenerator {

  // returns the template content by name
  String getFullTemplate(String name);

}
