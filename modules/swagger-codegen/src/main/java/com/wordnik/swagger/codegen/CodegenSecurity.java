package com.wordnik.swagger.codegen;

public class CodegenSecurity {
  public String name;
  public String type;
  public Boolean hasMore, isBasic, isOAuth, isApiKey;
  // ApiKey specific
  public String keyParamName;
  public Boolean isKeyInQuery, isKeyInHeader;
}
