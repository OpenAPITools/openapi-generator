package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.io.File;
import java.util.*;

public class Python3ClientCodegen extends DefaultCodegen implements CodegenConfig {
  String module = "client";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "python3";
  }

  public String getHelp() {
    return "Generates a Python3 client library.";
  }

  public Python3ClientCodegen() {
    super();
    outputFolder = "generated-code/python3";
    modelTemplateFiles.put("model.mustache", ".py");
    apiTemplateFiles.put("api.mustache", ".py");
    templateDir = "python3";

    apiPackage = module;
    modelPackage = module + ".models";

    languageSpecificPrimitives.clear();
    languageSpecificPrimitives.add("int");
    languageSpecificPrimitives.add("float");
    //languageSpecificPrimitives.add("long");
    languageSpecificPrimitives.add("list");
    languageSpecificPrimitives.add("bool");
    languageSpecificPrimitives.add("str");
    languageSpecificPrimitives.add("datetime");

    typeMapping.clear();
    typeMapping.put("integer", "int");
    typeMapping.put("float", "float");
    typeMapping.put("long", "int");
    typeMapping.put("double", "float");
    typeMapping.put("array", "list");
    typeMapping.put("map", "map");
    typeMapping.put("boolean", "bool");
    typeMapping.put("string", "str");
    typeMapping.put("date", "datetime");

    // from https://docs.python.org/release/2.5.4/ref/keywords.html
    reservedWords = new HashSet<String> (
      Arrays.asList(
        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
        "assert", "else", "if", "pass", "yield", "break", "except", "import",
        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
        "return", "def", "for", "lambda", "try"));

    //supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("swagger.mustache", module, "swagger.py"));
    supportingFiles.add(new SupportingFile("__init__.mustache", module, "__init__.py"));
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

      return getSwaggerType(p) + "(String, " + getTypeDeclaration(inner) + ")";
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
    return type;
  }

  public String toDefaultValue(Property p) {
	// TODO: Support Python def value
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

  @Override
  public String toApiVarName(String name) {
      if(name.length() == 0)
          return "default_api";
    return underscore(name) + "_api";
  }

  @Override
  public String toOperationId(String operationId) {
    // method name cannot use reserved keyword, e.g. return
    if(reservedWords.contains(operationId))
      throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");

    return underscore(operationId);
  }

}
