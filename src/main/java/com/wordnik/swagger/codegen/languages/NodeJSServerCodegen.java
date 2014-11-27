package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class NodeJSServerCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";

  public String getName() {
    return "nodejs";
  }

  public String getHelp() {
    return "Generates a node.js server application compatible with the 1.2 swagger specification.";
  }

  public NodeJSServerCodegen() {
    super();
    outputFolder = "generated-code/nodejs";
    apiTemplateFiles.put("api.mustache", ".js");
    templateDir = "nodejs";
    apiPackage = "app.apis";
    modelPackage = "app";

    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
    supportingFiles.add(new SupportingFile("models.mustache", modelPackage, "models.js"));
    supportingFiles.add(new SupportingFile("main.mustache", "", "main.js"));
    supportingFiles.add(new SupportingFile("README.mustache", "", "README.js"));

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
    typeMapping.put("array", "array");
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + File.separator + apiPackage().replaceAll("\\.", File.separator);
  }

  public String modelFileFolder() {
    return outputFolder + File.separator + modelPackage().replaceAll("\\.", File.separator);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      return typeMapping.get(swaggerType);
    }
    else
      type = swaggerType;
    return toModelName(type);
  }
}