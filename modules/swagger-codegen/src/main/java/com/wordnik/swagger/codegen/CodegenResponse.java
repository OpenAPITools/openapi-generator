package com.wordnik.swagger.codegen;

import java.util.*;

public class CodegenResponse {
  public String code, message;
  public Boolean hasMore;
  public List<Map<String, String>> examples;
  Object schema;
}