package com.wordnik.swagger.codegen;

public class JavaCodegen extends DefaultCodegen implements CodegenConfig {
  public JavaCodegen() {
    super();
    outputFolder = "generated-code/java";
    modelTemplateFiles.put("model.mustache", ".java");
    apiTemplateFiles.put("api.mustache", ".java");
    templateDir = "Java";
    modelPackage = "com.wordnik.model";
  }


}