package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.*;
import com.wordnik.swagger.models.Swagger;

import org.apache.commons.io.FileUtils;

import java.io.File;

public class SwaggerGenerator extends DefaultCodegen implements CodegenConfig {
  public CodegenType getTag() {
    return CodegenType.DOCUMENTATION;
  }

  public String getName() {
    return "swagger";
  }

  public String getHelp() {
    return "Creates a static swagger.json file.";
  }

  public SwaggerGenerator() {
    super();
    templateDir = "swagger";
    outputFolder = "generated-code/swagger";

    supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
  }

  @Override
  public void processSwagger(Swagger swagger) {
    String swaggerString = Json.pretty(swagger);

    try{
      String outputFile = outputFolder + File.separator + "swagger.json";
      FileUtils.writeStringToFile(new File(outputFile), swaggerString);
      System.out.println("wrote file to " + outputFile);
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
}