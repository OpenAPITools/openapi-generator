package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

public class JavaClientCodegen extends DefaultCodegen implements CodegenConfig {
  public JavaClientCodegen() {
    super();
    outputFolder = "generated-code/java";
    modelTemplateFiles.put("model.mustache", ".java");
    apiTemplateFiles.put("api.mustache", ".java");
    templateDir = "Java";
    apiPackage = "com.wordnik.api";
    modelPackage = "com.wordnik.model";

    additionalProperties.put("invokerPackage", "com.wordnik.common");

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Integer",
        "Long",
        "Float")
      );
  }

  @Override
  public String toModelImport(String name) {
    return null;
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
    }
    else if (p instanceof MapProperty) {
      throw new RuntimeException("not supported yet");
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type))
        return toModelName(type);
    }
    else
      type = swaggerType;
    return toModelName(type);
  }
}