package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;

import java.util.*;

public class CodegenOperation {
  public Boolean hasConsumes, hasProduces, hasParams, returnTypeIsPrimitive,
    returnSimpleType, subresourceOperation, isMapContainer, isListContainer,
    hasMore = Boolean.TRUE;
  public String path, operationId, returnType, httpMethod, returnBaseType,
    returnContainer, summary, notes, baseName, defaultResponse;

  public List<Map<String, String>> consumes, produces;
  public CodegenParameter bodyParam;
  public List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
  public List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
  public List<CodegenSecurity> authMethods;
  public List<String> tags;
  public List<CodegenResponse> responses = new ArrayList<CodegenResponse>();
  public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
  public Set<String> imports = new HashSet<String>();
  public List<Map<String, String>> examples;
  public ExternalDocs externalDocs;

  // legacy support
  public String nickname;
}
