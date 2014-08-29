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
    }
    catch (Exception e) {
      e.printStackTrace();
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

  public Map<String, Object> processModels(CodegenConfig config, Map<String, Model> definitions) {
    Map<String, Object> objs = new HashMap<String, Object>();
    objs.put("package", config.modelPackage());
    List<Object> models = new ArrayList<Object>();
    List<Object> model = new ArrayList<Object>();
    for(String key: definitions.keySet()) {
      Model mm = definitions.get(key);
      if(mm instanceof ModelImpl) {
        CodegenModel cm = config.fromModel(key, (ModelImpl) mm);
        Map<String, Object> mo = new HashMap<String, Object>();
        mo.put("model", cm);
        models.add(mo);
      }
    }
    objs.put("models", models);
    return objs;
  }
}