package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.*;

import com.wordnik.swagger.codegen.languages.*;
import com.samskivert.mustache.*;

import org.apache.commons.io.FileUtils;

import java.util.*;
import java.io.*;

public class DefaultGenerator implements Generator {
  private CodegenConfig config;
  private ClientOptInput opts = null;
  private Swagger swagger = null;

  public Generator opts(ClientOptInput opts) {
    this.opts = opts;

    this.swagger = opts.getSwagger();
    ClientOpts clientOpts = opts.getOpts();
    this.config = opts.getConfig();
    this.config.additionalProperties().putAll(clientOpts.getProperties());

    return this;
  }

  public void generate() {
    if(swagger == null || config == null) {
      throw new RuntimeException("missing swagger input or config!");
    }
    try {
      config.processOpts();
      Map<String, Object> models = null;
      List<Object> allOperations = new ArrayList<Object>();

      // models
      Map<String, Model> definitions = swagger.getDefinitions();
      for(String name: definitions.keySet()) {
        Model model = definitions.get(name);
        Map<String, Model> modelMap = new HashMap<String, Model>();
        modelMap.put(name, model);
        models = processModels(config, modelMap);
        models.putAll(config.additionalProperties());
        for(String templateName : config.modelTemplateFiles().keySet()) {
          String suffix = config.modelTemplateFiles().get(templateName);
          String filename = config.modelFileFolder() + File.separator + config.toModelFilename(name) + suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
            .withLoader(new Mustache.TemplateLoader() {
              public Reader getTemplate (String name) {
                return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
              };
            })
            .defaultValue("")
            .compile(template);

          writeToFile(filename, tmpl.execute(models));
        }
      }
      // apis
      Map<String, List<CodegenOperation>> paths = processPaths(swagger.getPaths());
      for(String tag : paths.keySet()) {
        List<CodegenOperation> ops = paths.get(tag);
        Map<String, Object> operation = processOperations(config, tag, ops);
        operation.put("baseName", tag);
        operation.put("modelPackage", config.modelPackage());
        operation.putAll(config.additionalProperties());
        operation.put("classname", config.toApiName(tag));
        allOperations.add(operation);
        for(String templateName : config.apiTemplateFiles().keySet()) {
          String suffix = config.apiTemplateFiles().get(templateName);
          String filename = config.apiFileFolder() +
            File.separator +
            config.toApiFilename(tag) +
            suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
            .withLoader(new Mustache.TemplateLoader() {
              public Reader getTemplate (String name) {
                return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
              };
            })
            .defaultValue("")
            .compile(template);

          writeToFile(filename, tmpl.execute(operation));
        }
      }

      // supporting files
      Map<String, Object> bundle = new HashMap<String, Object>();
      bundle.putAll(config.additionalProperties());
      bundle.put("apiPackage", config.apiPackage());

      Map<String, Object> apis = new HashMap<String, Object>();
      apis.put("apis", allOperations);
      bundle.put("apiInfo", apis);

      for(SupportingFile support : config.supportingFiles()) {
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
            .withLoader(new Mustache.TemplateLoader() {
              public Reader getTemplate (String name) {
                return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
              };
            })
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

  public Map<String, List<CodegenOperation>> processPaths(Map<String, Path> paths) {
    Map<String, List<CodegenOperation>> ops = new HashMap<String, List<CodegenOperation>>();
    List<String> tags = null;

    for(String resourcePath : paths.keySet()) {
      Path path = paths.get(resourcePath);
      processOperation(resourcePath, "get", path.getGet(), ops);
      processOperation(resourcePath, "put", path.getPut(), ops);
      processOperation(resourcePath, "post", path.getPost(), ops);
      processOperation(resourcePath, "delete", path.getDelete(), ops);
      processOperation(resourcePath, "patch", path.getPatch(), ops);
      processOperation(resourcePath, "options", path.getOptions(), ops);
    }
    return ops;
  }

  public void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations) {
    if(operation != null) {
      List<String> tags = operation.getTags();
      if(tags == null) {
        tags = new ArrayList<String>();
        tags.add("default");
      }

      for(String tag : tags) {
        CodegenOperation co = config.fromOperation(resourcePath, httpMethod, operation);
        co.tags = new ArrayList<String>();
        co.tags.add(sanitizeTag(tag));

        config.addOperationToGroup(sanitizeTag(tag), resourcePath, operation, co, operations);
      }
    }
  }

  protected String sanitizeTag(String tag) {
    // remove spaces and make strong case
    String [] parts = tag.split(" ");
    StringBuffer buf = new StringBuffer();
    for(String part: parts) {
      if(!"".equals(part)) {
        buf.append(Character.toUpperCase(part.charAt(0)));
        if(part.length() > 1)
          buf.append(part.substring(1));
      }
    }
    return buf.toString().replaceAll("[^a-zA-Z ]", "");
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
      Reader reader = getTemplateReader(name);
      if(reader == null)
        throw new RuntimeException("no file found");
      java.util.Scanner s = new java.util.Scanner(reader).useDelimiter("\\A");
      return s.hasNext() ? s.next() : "";
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    throw new RuntimeException("can't load template " + name);
  }

  public Reader getTemplateReader(String name) {
    try{
      InputStream is = this.getClass().getClassLoader().getResourceAsStream(name);
      if(is == null)
        is = new FileInputStream(new File(name));
      if(is == null)
        throw new RuntimeException("no file found");
      return new InputStreamReader(is);
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
    config.postProcessOperations(operations);
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
      CodegenModel cm = config.fromModel(key, mm);
      Map<String, Object> mo = new HashMap<String, Object>();
      mo.put("model", cm);
      models.add(mo);
      allImports.addAll(cm.imports);
    }
    objs.put("models", models);

    List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
    for(String i: allImports) {
      Map<String, String> im = new HashMap<String, String>();
      String m = config.importMapping().get(i);
      if(m == null)
        m = config.toModelImport(i);
      if(m != null && !config.defaultIncludes().contains(m)) {
        im.put("import", m);
        imports.add(im);
      }
      // add instantiation types
      m = config.instantiationTypes().get(i);
      if(m != null && !config.defaultIncludes().contains(m)) {
        im.put("import", m);
        imports.add(im);
      }      
    }

    objs.put("imports", imports);
    config.postProcessModels(objs);

    return objs;
  }
}