package com.wordnik.swagger.codegen;

public class CodegenSecurity {
  String name;
  String type;
  Boolean hasMore, isBasic, isOAuth, isApiKey;
  // ApiKey specific
  String keyParamName;
  Boolean isKeyInQuery, isKeyInHeader;

}
