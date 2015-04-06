package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.*;
import com.wordnik.swagger.models.Swagger;

import org.apache.commons.io.FileUtils;

import java.io.File;

public class SwaggerYamlGenerator extends DefaultCodegen implements CodegenConfig {
  public CodegenType getTag() {
    return CodegenType.DOCUMENTATION;
  }

  public String getName() {
    return "swagger-yaml";
  }

  public String getHelp() {
    return "Creates a static swagger.yaml file.";
  }

  public SwaggerYamlGenerator() {
    super();
    templateDir = "swagger";
    outputFolder = "generated-code/swagger";

    supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
  }

  @Override
  public void processSwagger(Swagger swagger) {
    try{
      String swaggerString = Yaml.mapper().writeValueAsString(swagger);
      String outputFile = outputFolder + File.separator + "swagger.yaml";
      FileUtils.writeStringToFile(new File(outputFile), swaggerString);
      System.out.println("wrote file to " + outputFile);
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
}