package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;


public class CodegenOperation {
  public Boolean hasParams, returnTypeIsPrimitive, returnSimpleType;
  public String path, operationId, returnType, httpMethod, returnBaseType, 
    returnContainer, summary, notes;

  public List<Map<String, String>> consumes, produces;
  public List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();

  public Set<String> imports = new HashSet<String>();

  // legacy support
  public String nickname;
}
