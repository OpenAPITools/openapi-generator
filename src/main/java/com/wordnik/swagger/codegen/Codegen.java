package com.wordnik.swagger.codegen;

import  com.wordnik.swagger.codegen.languages.*;
import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.util.Json;

import java.io.File;

public class Codegen extends DefaultGenerator {
  public static void main(String[] args) {
    CodegenConfig config = null;
    if(args != null && args.length > 0) {
      String lang = args[0];
      if("java".equals(lang)) {
        JavaClientCodegen javaConfig = new JavaClientCodegen();
        javaConfig.setTemplateDir("src/main/resources/Java");
        config = javaConfig;
      }
      else if ("objc".equals(lang)) {
        ObjcClientCodegen objcConfig = new ObjcClientCodegen();
        objcConfig.setTemplateDir("src/main/resources/objc");
        config = objcConfig;
      }
    }

    try{
      Swagger swagger = (Swagger) Json.mapper()
        .readValue(new File("swagger.json"), Swagger.class);

      new Codegen()
        .config(config)
        .generate(swagger);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }
}
