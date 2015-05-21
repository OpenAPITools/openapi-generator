package com.wordnik.swagger.codegen;

public class CodegenParameter {
  public Boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
    isCookieParam, isBodyParam, isFile, notFile, hasMore, isContainer, secondaryParam,
    isPrimitiveType, notPrimitiveType;
  public String baseName, paramName, dataType, collectionFormat, description, baseType;
  public String jsonSchema;

  /**
   * Determines whether this parameter is mandatory. If the parameter is in "path",
   * this property is required and its value MUST be true. Otherwise, the property
   * MAY be included and its default value is false.
   */
  public Boolean required;

  public CodegenParameter copy() {
    CodegenParameter output = new CodegenParameter();
    output.isFile = this.isFile;
    output.notFile = this.notFile;
    output.hasMore = this.hasMore;
    output.isContainer = this.isContainer;
    output.secondaryParam = this.secondaryParam;
    output.baseName = this.baseName;
    output.paramName = this.paramName;
    output.dataType = this.dataType;
    output.collectionFormat = this.collectionFormat;
    output.description = this.description;
    output.baseType = this.baseType;
    output.isFormParam = this.isFormParam;
    output.isQueryParam = this.isQueryParam;
    output.isPathParam = this.isPathParam;
    output.isHeaderParam = this.isHeaderParam;
    output.isCookieParam = this.isCookieParam;
    output.isBodyParam = this.isBodyParam;
    output.required = this.required;
    output.jsonSchema = this.jsonSchema;
    output.isPrimitiveType = this.isPrimitiveType;
    output.notPrimitiveType = this.notPrimitiveType;

    return output;
  }
}