package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class PhpClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "php";
  }

  public String getHelp() {
    return "Generates a PHP client library.";
  }

  public PhpClientCodegen() {
    super();

    invokerPackage = camelize("SwaggerClient"); 

    String packagePath = invokerPackage + "-php";

    modelPackage = packagePath + "/lib/models";
    apiPackage = packagePath + "/lib";
    outputFolder = "generated-code/php";
    modelTemplateFiles.put("model.mustache", ".php");
    apiTemplateFiles.put("api.mustache", ".php");
    templateDir = "php";

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
    );

    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    // ref: http://php.net/manual/en/language.types.intro.php
    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "boolean",
        "int",
        "integer",
        "double",
        "float",
        "string",
        "object",
        "DateTime",
        "mixed",
        "number")
    );

    instantiationTypes.put("array", "array");
    instantiationTypes.put("map", "map");

    // ref: https://github.com/swagger-api/swagger-spec/blob/master/versions/2.0.md#data-types
    typeMapping = new HashMap<String, String>();
    typeMapping.put("integer", "int");
    typeMapping.put("long", "int");
    typeMapping.put("float", "float");
    typeMapping.put("double", "double");
    typeMapping.put("string", "string");
    typeMapping.put("byte", "int");
    typeMapping.put("boolean", "boolean");
    typeMapping.put("date", "DateTime");
    typeMapping.put("datetime", "DateTime");
    typeMapping.put("file", "string");
    typeMapping.put("map", "map");
    typeMapping.put("array", "array");
    typeMapping.put("list", "array");

    supportingFiles.add(new SupportingFile("composer.mustache", packagePath, "composer.json"));
    supportingFiles.add(new SupportingFile("APIClient.mustache", packagePath + "/lib", "APIClient.php"));
    supportingFiles.add(new SupportingFile("APIClientException.mustache", packagePath + "/lib", "APIClientException.php"));
    supportingFiles.add(new SupportingFile("require.mustache", packagePath, invokerPackage + ".php"));
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
      else if (instantiationTypes.containsKey(type)) {
	return type;
      }
    }
    else
      type = swaggerType;
    if(type == null)
      return null;
    return toModelName(type);
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

}
