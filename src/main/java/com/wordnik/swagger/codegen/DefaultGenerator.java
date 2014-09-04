package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.*;

import com.samskivert.mustache.*;

import org.apache.commons.io.FileUtils;

import java.util.*;
import java.io.*;

public class DefaultGenerator implements Generator {
  private CodegenConfig config;

  public Generator config(CodegenConfig config) {
    this.config = config;
    return this;
  }

  public void generate(Swagger swagger) {
    try {
      Map<String, Object> models = null;
      Map<String, Object> operations = null;
      // models
      Map<String, Model> definitions = swagger.getDefinitions();
      for(String name: definitions.keySet()) {
        Model model = definitions.get(name);
        Map<String, Model> modelMap = new HashMap<String, Model>();
        modelMap.put(name, model);
        models = processModels(config, modelMap);
        models.putAll(config.additionalProperties());
        for(String templateName: config.modelTemplateFiles().keySet()) {
          String suffix = config.modelTemplateFiles().get(templateName);
          String filename = config.modelFileFolder() + File.separator + config.toModelFilename(name) + suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
            .defaultValue("")
            .compile(template);

          writeToFile(filename, tmpl.execute(models));
        }
      }
      // apis
      Map<String, List<CodegenOperation>> paths = groupPaths(swagger.getPaths());
      for(String tag: paths.keySet()) {
        List<CodegenOperation> ops = paths.get(tag);

        operations = processOperations(config, tag, ops);
        operations.putAll(config.additionalProperties());
        for(String templateName: config.apiTemplateFiles().keySet()) {
          String suffix = config.apiTemplateFiles().get(templateName);
          String filename = config.apiFileFolder() +
            File.separator +
            config.toApiFilename(tag) +
            suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
            .defaultValue("")
            .compile(template);

          writeToFile(filename, tmpl.execute(operations));
        }
      }

      // supporting files
      Map<String, Object> bundle = new HashMap<String, Object>();
      bundle.putAll(config.additionalProperties());
      for(SupportingFile support: config.supportingFiles()) {
        String outputFolder = config.outputFolder();
        if(support.folder != null && !"".equals(support.folder))
          outputFolder += File.separator + support.folder;
        File of = new File(outputFolder);
        if(!of.isDirectory())
          of.mkdirs();
        String outputFilename = outputFolder + File.separator + support.destinationFilename;

        if(support.templateFile.endsWith("mustache")) {
          String template = readTemplate(config.templateDir() + File.separator + support.templateFile);
          Template tmpl = Mustache.compiler()
            .defaultValue("")
            .compile(template);

          writeToFile(outputFilename, tmpl.execute(bundle));
        }
        else {
          String template = readTemplate(config.templateDir() + File.separator + support.templateFile);
          FileUtils.writeStringToFile(new File(outputFilename), template);
          System.out.println("copying file to " + outputFilename);
        }
      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  public Map<String, List<CodegenOperation>> groupPaths(Map<String, Path> paths) {
    // group by tag, create a Default grouping if none
    Map<String, List<CodegenOperation>> ops = new HashMap<String, List<CodegenOperation>>();
    List<String> tags = null;

    for(String resourcePath: paths.keySet()) {
      Path path = paths.get(resourcePath);
      processOperation(resourcePath, "get", path.getGet(), ops);
      processOperation(resourcePath, "put", path.getPut(), ops);
      processOperation(resourcePath, "post", path.getPost(), ops);
      processOperation(resourcePath, "delete", path.getDelete(), ops);
      processOperation(resourcePath, "patch", path.getPatch(), ops);
      processOperation(resourcePath, "options", path.getOptions(), ops);
    }
    // Json.prettyPrint(ops);
    return ops;
  }

  public void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations) {
    if(operation != null) {
      List<String> tags = operation.getTags();
      if(tags == null) {
        tags = new ArrayList<String>();
        tags.add("default");
      }

      for(String tag: tags) {
        List<CodegenOperation> opList = operations.get(tag);
        if(opList == null) {
          opList = new ArrayList<CodegenOperation>();
          operations.put(tag, opList);
        }
        CodegenOperation co = config.fromOperation(resourcePath, httpMethod, operation);
        opList.add(co);
      }
    }
  }

  public File writeToFile(String filename, String contents) throws IOException {
    System.out.println("writing file " + filename);
    File output = new File(filename);

    if(output.getParent() != null && !new File(output.getParent()).exists()) {
      File parent = new File(output.getParent());
      parent.mkdirs();
    }
    Writer out = new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream(output), "UTF-8"));

    out.write(contents);
    out.close();
    return output;
  }

  public String readTemplate(String name) {
    try{
      InputStream is = this.getClass().getClassLoader().getResourceAsStream(name);
      if(is == null)
        is = new FileInputStream(new File(name));
      if(is == null)
        throw new RuntimeException("no file found");
      java.util.Scanner s = new java.util.Scanner(is).useDelimiter("\\A");
      return s.hasNext() ? s.next() : "";
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    throw new RuntimeException("can't load template " + name);
  }

  public Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops) {
    Map<String, Object> operations = new HashMap<String, Object>();
    Map<String, Object> objs = new HashMap<String, Object>();
    objs.put("classname", config.toApiName(tag));
    objs.put("operation", ops);
    operations.put("operations", objs);
    operations.put("package", config.apiPackage());

    Set<String> allImports = new HashSet<String>();
    for(CodegenOperation op: ops) {
      allImports.addAll(op.imports);
    }

    List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
    for(String i: allImports) {
      Map<String, String> im = new HashMap<String, String>();
      String m = config.importMapping().get(i);
      if(m == null)
        m = config.toModelImport(i);
      if(m != null) {
        im.put("import", m);
        imports.add(im);
      }
    }

    operations.put("imports", imports);

    return operations;
  }

  public Map<String, Object> processModels(CodegenConfig config, Map<String, Model> definitions) {
    Map<String, Object> objs = new HashMap<String, Object>();
    objs.put("package", config.modelPackage());
    List<Object> models = new ArrayList<Object>();
    List<Object> model = new ArrayList<Object>();
    Set<String> allImports = new HashSet<String>();
    for(String key: definitions.keySet()) {
      Model mm = definitions.get(key);
      if(mm instanceof ModelImpl) {
        CodegenModel cm = config.fromModel(key, (ModelImpl) mm);
        Map<String, Object> mo = new HashMap<String, Object>();
        mo.put("model", cm);
        models.add(mo);
        allImports.addAll(cm.imports);
      }
    }
    objs.put("models", models);

    List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
    for(String i: allImports) {
      Map<String, String> im = new HashMap<String, String>();
      String m = config.importMapping().get(i);
      if(m == null)
        m = config.toModelImport(i);
      if(m != null) {
        im.put("import", m);
        imports.add(im);
      }
    }

    objs.put("imports", imports);
    return objs;
  }
}