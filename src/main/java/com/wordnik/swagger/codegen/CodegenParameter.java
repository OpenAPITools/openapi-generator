package com.wordnik.swagger.codegen;

public class CodegenParameter {
  public Boolean hasMore = null, isContainer = null, secondaryParam = null;
  public String baseName, paramName, dataType, collectionFormat, description, baseType;
  public Boolean isQueryParam, isPathParam, isHeaderParam, isCookieParam, isBodyParam;
  /**
   * Determines whether this parameter is mandatory. If the parameter is in "path",
   * this property is required and its value MUST be true. Otherwise, the property
   * MAY be included and its default value is false.
   */
  public boolean required;
}