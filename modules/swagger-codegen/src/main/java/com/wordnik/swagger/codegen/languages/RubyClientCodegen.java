package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class RubyClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "ruby";
  }

  public String getHelp() {
    return "Generates a Ruby client library.";
  }

  public RubyClientCodegen() {
    super();
    modelPackage = "models";
    apiPackage = "lib";
    outputFolder = "generated-code/ruby";
    modelTemplateFiles.put("model.mustache", ".rb");
    apiTemplateFiles.put("api.mustache", ".rb");
    templateDir = "ruby";

    typeMapping.clear();
    languageSpecificPrimitives.clear();

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "__FILE__", "and", "def", "end", "in", "or", "self", "unless", "__LINE__", 
        "begin", "defined?", "ensure", "module", "redo", "super", "until", "BEGIN",
        "break", "do", "false", "next", "rescue", "then", "when", "END", "case", 
        "else", "for", "nil", "retry", "true", "while", "alias", "class", "elsif",
        "if", "not", "return", "undef", "yield")
    );

    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    languageSpecificPrimitives.add("int");
    languageSpecificPrimitives.add("array");
    languageSpecificPrimitives.add("map");
    languageSpecificPrimitives.add("string");
    languageSpecificPrimitives.add("DateTime");

    typeMapping.put("long", "int");
    typeMapping.put("integer", "int");
    typeMapping.put("Array", "array");
    typeMapping.put("String", "string");
    typeMapping.put("List", "array");
    typeMapping.put("map", "map");

    supportingFiles.add(new SupportingFile("swagger.mustache", "", "lib/swagger.rb"));
    supportingFiles.add(new SupportingFile("monkey.mustache", "", "lib/monkey.rb"));
    supportingFiles.add(new SupportingFile("swagger/request.mustache", "", "lib/swagger/request.rb"));
    supportingFiles.add(new SupportingFile("swagger/response.mustache", "", "lib/swagger/response.rb"));
    supportingFiles.add(new SupportingFile("swagger/version.mustache", "", "lib/swagger/version.rb"));
    supportingFiles.add(new SupportingFile("swagger/configuration.mustache", "", "lib/swagger/configuration.rb"));
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  public String modelFileFolder() {
    return outputFolder + "/" + modelPackage().replace('.', File.separatorChar);
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
    // replace - with _ e.g. created-at => created_at
    name = name.replaceAll("-", "_");

    // if it's all uppper case, convert to lower case
    if (name.matches("^[A-Z_]*$"))
      name = name.toLowerCase();

    // camelize (lower first character) the variable name
    // petId => pet_id
    name = underscore(name);

    // for reserved word or word starting with number, append _
    if(reservedWords.contains(name) || name.matches("^\\d.*"))
      name = escapeReservedWord(name);

    return name;
  }

  @Override
  public String toParamName(String name) {
    // should be the same as variable name
    return toVarName(name);
  }
 
  @Override
  public String toModelName(String name) {
    // model name cannot use reserved keyword, e.g. return
    if(reservedWords.contains(name))
      throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
 
    // camelize the model name
    // phone_number => PhoneNumber
    return camelize(name);
  }

  @Override
  public String toModelFilename(String name) {
    // model name cannot use reserved keyword, e.g. return
    if(reservedWords.contains(name))
      throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
 
    // underscore the model file name
    // PhoneNumber.rb => phone_number.rb
    return underscore(name);
  } 

  @Override
  public String toApiFilename(String name) {
    // replace - with _ e.g. created-at => created_at
    name = name.replaceAll("-", "_");

    // e.g. PhoneNumberApi.rb => phone_number_api.rb
    return underscore(name) + "_api";
  }

  @Override
  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultApi";
    // e.g. phone_number_api => PhoneNumberApi 
    return camelize(name) + "Api";
  }

}
