package com.wordnik.swagger.codegen;

import java.util.*;

public class CodegenResponse {
  public String code, message;
  public Boolean hasMore;
  public List<Map<String, String>> examples;
  public final List<CodegenProperty> headers = new ArrayList<CodegenProperty>();
  public String dataType, baseType, containerType;
  public Boolean simpleType;
  public Boolean primitiveType;
  public Boolean isMapContainer;
  public Boolean isListContainer;
  public Object schema;
  public String jsonSchema;
}