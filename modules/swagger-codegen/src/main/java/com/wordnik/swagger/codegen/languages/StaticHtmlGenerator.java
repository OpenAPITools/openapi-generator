package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.Operation;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.Json;

import java.util.*;
import java.io.File;

public class StaticHtmlGenerator extends DefaultCodegen implements CodegenConfig {
  private static final String ALL_OPERATIONS = "";
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";
  protected String sourceFolder = "src/main/scala";

  public CodegenType getTag() {
    return CodegenType.DOCUMENTATION;
  }

  public String getName() {
    return "html";
  }

  public String getHelp() {
    return "Generates a static HTML file.";
  }

  public StaticHtmlGenerator() {
    super();
    outputFolder = "docs";
    templateDir = "htmlDocs";

    defaultIncludes = new HashSet<String>();

    additionalProperties.put("appName", "Swagger Sample");
    additionalProperties.put("appDescription", "A sample swagger server");
    additionalProperties.put("infoUrl", "https://helloreverb.com");
    additionalProperties.put("infoEmail", "hello@helloreverb.com");
    additionalProperties.put("licenseInfo", "All rights reserved");
    additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);
  
    supportingFiles.add(new SupportingFile("index.mustache", "", "index.html")); 
    reservedWords = new HashSet<String>();

    languageSpecificPrimitives = new HashSet<String>();
    importMapping = new HashMap<String, String> ();
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
    for(CodegenOperation op: operationList) {
      op.httpMethod = op.httpMethod.toLowerCase();
    }
    return objs;
  }

  @Override
  public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
    List<CodegenOperation> opList = operations.get(ALL_OPERATIONS);
    if(opList == null) {
      opList = new ArrayList<CodegenOperation>();
      operations.put(ALL_OPERATIONS, opList);
    }
    for (CodegenOperation addedOperation: opList){
      if (addedOperation.operationId.equals(co.operationId) && addedOperation.path.equals(co.path) && addedOperation.httpMethod.equals(co.httpMethod)) {
        addedOperation.tags.addAll(co.tags);
        return;
      }
    }
    opList.add(co);
  }
}