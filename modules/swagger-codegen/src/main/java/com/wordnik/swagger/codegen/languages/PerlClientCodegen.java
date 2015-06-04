package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class PerlClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "SwaggerClient";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "perl";
  }

  public String getHelp() {
    return "Generates a Perl client library.";
  }

  public PerlClientCodegen() {
    super();
    modelPackage = File.separatorChar + "Object";
    outputFolder = "generated-code" + File.separatorChar + "perl";
    modelTemplateFiles.put("object.mustache", ".pm");
    apiTemplateFiles.put("api.mustache", ".pm");
    templateDir = "perl";

    typeMapping.clear();
    languageSpecificPrimitives.clear();

    reservedWords = new HashSet<String> (
      Arrays.asList(
  "else",  "lock",  "qw",
  "__END__", "elsif", "lt", "qx",
  "__FILE__", "eq", "m", "s",
  "__LINE__", "exp", "ne", "sub",
  "__PACKAGE__", "for", "no", "tr",
  "and", "foreach", "or", "unless",
  "cmp", "ge", "package", "until",
  "continue", "gt", "q", "while",
  "CORE", "if", "qq", "xor",
  "do", "le", "qr", "y"
      )  
    );

    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    languageSpecificPrimitives.add("int");
    languageSpecificPrimitives.add("double");
    languageSpecificPrimitives.add("string");
    languageSpecificPrimitives.add("boolean");
    languageSpecificPrimitives.add("DateTime");
    languageSpecificPrimitives.add("ARRAY");
    languageSpecificPrimitives.add("HASH");
    languageSpecificPrimitives.add("object");

    typeMapping.put("integer", "int");
    typeMapping.put("long", "int");
    typeMapping.put("float", "double");
    typeMapping.put("double", "double");
    typeMapping.put("boolean", "boolean");
    typeMapping.put("string", "string");
    typeMapping.put("date", "DateTime");
    typeMapping.put("dateTime", "DateTime");
    typeMapping.put("password", "string");
    typeMapping.put("array", "ARRAY");
    typeMapping.put("map", "HASH");
    typeMapping.put("object", "object");

    supportingFiles.add(new SupportingFile("ApiClient.mustache", ("lib/WWW/" + invokerPackage).replace('/', File.separatorChar), "ApiClient.pm"));
    supportingFiles.add(new SupportingFile("Configuration.mustache", ("lib/WWW/" + invokerPackage).replace('/', File.separatorChar), "Configuration.pm"));
    supportingFiles.add(new SupportingFile("BaseObject.mustache", ("lib/WWW/" + invokerPackage).replace('/', File.separatorChar), "Object/BaseObject.pm"));
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }
  
  @Override
  public String apiFileFolder() {
    return (outputFolder + "/lib/WWW/" + invokerPackage + apiPackage()).replace('/', File.separatorChar);
  }

  public String modelFileFolder() {
    return (outputFolder + "/lib/WWW/" + invokerPackage + modelPackage()).replace('/', File.separatorChar);
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
      return getSwaggerType(p) + "[string," + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type)) {
        return type;
      }
    }
    else
      type = swaggerType;
    if(type == null)
      return null;
    return type;
  }

  public String toDefaultValue(Property p) {
    return "null";
  }


  @Override
  public String toVarName(String name) {
    // parameter name starting with number won't compile
    // need to escape it by appending _ at the beginning
    if (name.matches("^[0-9]")) {
      name = "_" + name;
    }
    
    // return the name in underscore style
    // PhoneNumber => phone_number
    return underscore(name);
  }

  @Override
  public String toParamName(String name) {
    // should be the same as variable name
    return toVarName(name);
  }

  @Override
  public String toModelName(String name) {
    // model name cannot use reserved keyword
    if(reservedWords.contains(name))
      escapeReservedWord(name); // e.g. return => _return

    // camelize the model name
    // phone_number => PhoneNumber
    return camelize(name);
  }

  @Override
  public String toModelFilename(String name) {
    // should be the same as the model name
    return toModelName(name);
  }

  @Override
  public String toApiFilename(String name) {
    // replace - with _ e.g. created-at => created_at
    name = name.replaceAll("-", "_");

    // e.g. phone_number_api.rb => PhoneNumberApi.rb
    return camelize(name) + "Api";
  }

  @Override
  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultApi";
    // e.g. phone_number_api => PhoneNumberApi 
    return camelize(name) + "Api";
  }

  @Override
  public String toOperationId(String operationId) {
    // method name cannot use reserved keyword, e.g. return
    if(reservedWords.contains(operationId))
      throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");

    return underscore(operationId);
  }


}
