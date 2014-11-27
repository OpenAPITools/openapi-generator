package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.languages.*;
import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.parser.SwaggerParser;
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

    ClientOptInput clientOptInput = new ClientOptInput();
    ClientOpts clientOpts = new ClientOpts();
    Swagger swagger = null;

    CommandLine cmd = null;
    try {
      CommandLineParser parser = new BasicParser();
      cmd = parser.parse(options, args);
      if (cmd.hasOption("l"))
        clientOptInput.setConfig(getConfig(cmd.getOptionValue("l")));

      if (cmd.hasOption("o"))
        clientOptInput.getConfig().setOutputDir(cmd.getOptionValue("o"));
      if (cmd.hasOption("i")) {
        swagger = new SwaggerParser().read(cmd.getOptionValue("i"));
      }
      if (cmd.hasOption("t")) {
        clientOpts.getProperties().put("templateDir",
          String.valueOf(cmd.getOptionValue("t")));
      }
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
      clientOptInput
        .opts(clientOpts)
        .swagger(swagger);
      new Codegen().opts(clientOptInput).generate();
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
    else if("nodejs".equals(name))
      return new NodeJSServerCodegen();
    else if("scalatra".equals(name))
      return new ScalatraServerCodegen();
    else if("html".equals(name))
      return new StaticHtmlGenerator();
    else if("swagger".equals(name))
      return new SwaggerGenerator();
    else if("tizen".equals(name))
      return new TizenClientCodegen();
    else if(name.indexOf(".") > 0) {
      // see if it's a class
      try {
        System.out.println("loading class " + name);
        Class customClass = Class.forName(name);
        System.out.println("loaded");
        return (CodegenConfig)customClass.newInstance();
      }
      catch (Exception e) {
        throw new RuntimeException("can't load class " + name);
      }
    }
    else
      throw new RuntimeException("unsupported client type");
  }
}
