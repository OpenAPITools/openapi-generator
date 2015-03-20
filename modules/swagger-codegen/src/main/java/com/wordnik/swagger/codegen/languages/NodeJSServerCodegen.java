package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class NodeJSServerCodegen extends DefaultCodegen implements CodegenConfig {
  protected String apiVersion = "1.0.0";
  protected int serverPort = 8080;
  protected String projectName = "swagger-server";

  public String apiPackage() {
    return "controllers";
  }

  /**
   * Configures the type of generator.
   * 
   * @return  the CodegenType for this generator
   * @see     com.wordnik.swagger.codegen.CodegenType
   */
  public CodegenType getTag() {
    return CodegenType.SERVER;
  }

  /**
   * Configures a friendly name for the generator.  This will be used by the generator
   * to select the library with the -l flag.
   * 
   * @return the friendly name for the generator
   */
  public String getName() {
    return "nodejs";
  }

  /**
   * Returns human-friendly help for the generator.  Provide the consumer with help
   * tips, parameters here
   * 
   * @return A string value for the help message
   */
  public String getHelp() {
    return "Generates a nodejs server library using the swagger-tools project.  By default, " +
      "it will also generate service classes--which you can disable with the `-Dnoservice` environment variable.";
  }

  public NodeJSServerCodegen() {
    super();

    // set the output folder here
    outputFolder = "generated-code/nodejs";

    /**
     * Models.  You can write model files using the modelTemplateFiles map.
     * if you want to create one template for file, you can do so here.
     * for multiple files for model, just put another entry in the `modelTemplateFiles` with
     * a different extension
     */
    modelTemplateFiles.clear();

    /**
     * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
     * as with models, add multiple entries with different extensions for multiple files per
     * class
     */
    apiTemplateFiles.put(
      "controller.mustache",   // the template to use
      ".js");       // the extension for each file to write

    /**
     * Template Location.  This is the location which templates will be read from.  The generator
     * will use the resource stream to attempt to read the templates.
     */
    templateDir = "nodejs";

    /**
     * Reserved words.  Override this with reserved words specific to your language
     */
    reservedWords = new HashSet<String> (
      Arrays.asList(
        "break", "case", "class", "catch", "const", "continue", "debugger",
        "default", "delete", "do", "else", "export", "extends", "finally",
        "for", "function", "if", "import", "in", "instanceof", "let", "new",
        "return", "super", "switch", "this", "throw", "try", "typeof", "var",
        "void", "while", "with", "yield")
    );

    /**
     * Additional Properties.  These values can be passed to the templates and
     * are available in models, apis, and supporting files
     */
    additionalProperties.put("apiVersion", apiVersion);
    additionalProperties.put("serverPort", serverPort);

    /**
     * Supporting Files.  You can write single files for the generator with the
     * entire object tree available.  If the input file has a suffix of `.mustache
     * it will be processed by the template engine.  Otherwise, it will be copied
     */
    // supportingFiles.add(new SupportingFile("controller.mustache",
    //   "controllers",
    //   "controller.js")
    // );
    supportingFiles.add(new SupportingFile("swagger.mustache",
      "api",
      "swagger.json")
    );
    supportingFiles.add(new SupportingFile("index.mustache",
      "",
      "index.js")
    );
    supportingFiles.add(new SupportingFile("package.mustache",
      "",
      "package.json")
    );
    if(System.getProperty("noservice") == null) {
      apiTemplateFiles.put(
        "service.mustache",   // the template to use
        "Service.js");       // the extension for each file to write
    }
  }

  @Override
  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultController";
    return initialCaps(name);
  }

  @Override
  public String toApiFilename(String name) {
    return toApiName(name);
  }
  /**
   * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
   * those terms here.  This logic is only called if a variable matches the reseved words
   * 
   * @return the escaped term
   */
  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;  // add an underscore to the name
  }

  /**
   * Location to write api files.  You can use the apiPackage() as defined when the class is
   * instantiated
   */
  @Override
  public String apiFileFolder() {
    return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> objectMap = (Map<String, Object>)objs.get("operations");
    List<CodegenOperation> operations = (List<CodegenOperation>)objectMap.get("operation");
    for(CodegenOperation operation : operations) {
      operation.httpMethod = operation.httpMethod.toLowerCase();
      List<CodegenParameter> params = operation.allParams;
      if(params != null && params.size() == 0)
        operation.allParams = null;
      List<CodegenResponse> responses = operation.responses;
      if(responses != null) {
        for(CodegenResponse resp : responses) {
          if("0".equals(resp.code))
            resp.code = "default";
        }
      }
      if(operation.examples != null && operation.examples.size() > 0) {
        List<Map<String, String>> examples = operation.examples;
        for(int i = examples.size() - 1; i >= 0; i--) {
          Map<String, String> example = examples.get(i);
          String contentType = example.get("contentType");
          if(contentType != null && contentType.indexOf("application/json") == 0) {
            String jsonExample = example.get("example");
            if(jsonExample != null) {
              jsonExample = jsonExample.replaceAll("\\\\n", "\n");
              example.put("example", jsonExample);
            }            
          }
          else
            examples.remove(i);
        }
      }
    }
    return objs;
  }
}