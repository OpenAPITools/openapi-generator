package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.util.Json;

import java.io.File;

public class Codegen extends DefaultGenerator {
  public static void main(String[] args) {
    JavaCodegen config = new JavaCodegen();
    config.setTemplateDir("src/main/resources/Java");

    try{
      Swagger swagger = (Swagger) Json.mapper()
        .readValue(new File("swagger.json"), Swagger.class);

      new Codegen()
        .config(new JavaCodegen())
        .generate(swagger);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }
}
