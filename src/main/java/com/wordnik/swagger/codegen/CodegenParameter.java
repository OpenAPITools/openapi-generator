package com.wordnik.swagger.codegen;

public class CodegenParameter {
  public Boolean hasMore = null, isContainer = null, secondaryParam = null, required = null;
  public String baseName, paramName, dataType, collectionFormat, description, baseType;
  public Boolean isQueryParam, isPathParam, isHeaderParam, isCookieParam, isBodyParam;
}