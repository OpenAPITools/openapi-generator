package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.languages.*;
import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.util.*;

import org.apache.commons.cli.*;

import java.io.File;

public class Codegen extends DefaultGenerator {
  public static void main(String[] args) {
    Options options = new Options();
    options.addOption("l", "lang", true, "client language to generate");
    options.addOption("o", "output", true, "where to write the generated files");
    options.addOption("i", "input-spec", true, "location of the swagger spec, as URL or file");
    options.addOption("t", "template-dir", true, "folder containing the template files");

    ClientOptInput codegenInput = new ClientOptInput();
    ClientOpts clientArgs = new ClientOpts();
    Swagger swagger = null;

    CommandLine cmd = null;
    try {
      CommandLineParser parser = new BasicParser();
      cmd = parser.parse(options, args);
      if (cmd.hasOption("l"))
        codegenInput.setConfig(getConfig(cmd.getOptionValue("l")));
      if (cmd.hasOption("o"))
        codegenInput.getConfig().setOutputDir(cmd.getOptionValue("o"));
      if (cmd.hasOption("i"))
        swagger = new SwaggerLoader().read(cmd.getOptionValue("i"));
      if (cmd.hasOption("t"))
        clientArgs.getProperties().put("templateDir",
          String.valueOf(cmd.getOptionValue("t")));
      if (cmd.hasOption("h")) {
        usage(options);
        return;
      }
    }
    catch (Exception e) {
      usage(options);
      return;
    }
    try{
      codegenInput
        .opts(clientArgs)
        .swagger(swagger);
      new Codegen().opts(codegenInput).generate();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  static void usage(Options options) {
    HelpFormatter formatter = new HelpFormatter();
    formatter.printHelp( "Codegen", options );
  }

  static CodegenConfig getConfig(String name) {
    if("objc".equals(name))
      return new ObjcClientCodegen();
    else if("android".equals(name))
      return new AndroidClientCodegen();
    else if("java".equals(name)) 
      return new JavaClientCodegen();
    else if("jaxrs".equals(name))
      return new JaxRSServerCodegen();
    else
      throw new RuntimeException("unsupported client type");
  }
}
