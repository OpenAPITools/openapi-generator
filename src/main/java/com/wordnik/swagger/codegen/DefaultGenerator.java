package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.*;

import com.samskivert.mustache.*;


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
      // models
      Map<String, Model> definitions = swagger.getDefinitions();
      for(String name: definitions.keySet()) {
        Model model = definitions.get(name);
        Map<String, Model> modelMap = new HashMap<String, Model>();
        modelMap.put(name, model);
        Object models = processModels(config, modelMap);
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

        Object tagObject = processOperations(config, tag, ops);
        Json.prettyPrint(tagObject);

        for(String templateName: config.apiTemplateFiles().keySet()) {
          String suffix = config.apiTemplateFiles().get(templateName);
          String filename = config.apiFileFolder() + File.separator + config.toApiFilename(tag) + suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
            .defaultValue("")
            .compile(template);

          writeToFile(filename, tmpl.execute(tagObject));
        }
        
      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void processOperation(String tag, String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations) {
    if(tag == null)
      tag = "default";

    List<CodegenOperation> opList = operations.get(tag);
    if(opList == null) {
      opList = new ArrayList<CodegenOperation>();
      operations.put(tag, opList);
    }
    CodegenOperation co = config.fromOperation(resourcePath, httpMethod, operation);
    opList.add(co);
  }

  public Map<String, List<CodegenOperation>> groupPaths(Map<String, Path> paths) {
    // group by tag, create a Default grouping if none
    Map<String, List<CodegenOperation>> ops = new HashMap<String, List<CodegenOperation>>();
    List<String> tags = null;

    for(String resourcePath: paths.keySet()) {
      Path path = paths.get(resourcePath);
      Operation get = path.getGet();
      if(get != null) {
        tags = get.getTags();
        if(tags != null && tags.size() > 0) {
          for(String tag: tags) {
            processOperation(tag, resourcePath, "get", get, ops);
          }
        }
        else {
          processOperation(null, resourcePath, "get", get, ops);
        }
      }
      // List<CodegenOperation> ops = ops
      Operation put = path.getPut();
      Operation post = path.getPost();
      Operation delete = path.getDelete();
      Operation patch = path.getPatch();
      Operation options = path.getOptions();

    }
    Json.prettyPrint(ops);
    return ops;
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