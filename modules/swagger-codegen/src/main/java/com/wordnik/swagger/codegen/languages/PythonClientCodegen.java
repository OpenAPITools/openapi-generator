package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.io.File;
import java.util.*;

public class PythonClientCodegen extends DefaultCodegen implements CodegenConfig {
  String module = "client";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "python";
  }

  public String getHelp() {
    return "Generates a Python client library.";
  }

  public PythonClientCodegen() {
    super();
    outputFolder = "generated-code/python";
    modelTemplateFiles.put("model.mustache", ".py");
    apiTemplateFiles.put("api.mustache", ".py");
    templateDir = "python";
    
    apiPackage = module;
    modelPackage = module + ".models";
    
    languageSpecificPrimitives.clear();
    languageSpecificPrimitives.add("int");
    languageSpecificPrimitives.add("float");
    languageSpecificPrimitives.add("long");
    languageSpecificPrimitives.add("list");
    languageSpecificPrimitives.add("bool");
    languageSpecificPrimitives.add("str");
    languageSpecificPrimitives.add("datetime");

    typeMapping.clear();
    typeMapping.put("integer", "int");
    typeMapping.put("float", "float");
    typeMapping.put("long", "long");
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

    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("swagger.mustache", module, "swagger.py"));
    supportingFiles.add(new SupportingFile("__init__.mustache", module, "__init__.py"));
    supportingFiles.add(new SupportingFile("__init__.mustache", modelPackage.replace('.', File.separatorChar), "__init__.py"));
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
}
