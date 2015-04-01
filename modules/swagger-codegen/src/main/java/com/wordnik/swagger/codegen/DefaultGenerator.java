package com.wordnik.swagger.codegen;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import com.wordnik.swagger.models.Contact;
import com.wordnik.swagger.models.Info;
import com.wordnik.swagger.models.License;
import com.wordnik.swagger.models.Model;
import com.wordnik.swagger.models.Operation;
import com.wordnik.swagger.models.Path;
import com.wordnik.swagger.models.Swagger;
import com.wordnik.swagger.models.auth.SecuritySchemeDefinition;
import com.wordnik.swagger.util.Json;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.Reader;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class DefaultGenerator extends AbstractGenerator implements Generator {
  protected CodegenConfig config;
  protected ClientOptInput opts = null;
  protected Swagger swagger = null;

  public Generator opts(ClientOptInput opts) {
    this.opts = opts;

    this.swagger = opts.getSwagger();
    this.config = opts.getConfig();
    this.config.additionalProperties().putAll(opts.getOpts().getProperties());

    return this;
  }

  public List<File> generate() {
    if (swagger == null || config == null) {
      throw new RuntimeException("missing swagger input or config!");
    }
    if (System.getProperty("debugSwagger") != null) {
      Json.prettyPrint(swagger);
    }
    List<File> files = new ArrayList<File>();
    try {
      config.processOpts();
      if (swagger.getInfo() != null) {
        Info info = swagger.getInfo();
        if (info.getTitle() != null) {
          config.additionalProperties().put("appName", info.getTitle());
        }
        if (info.getVersion() != null) {
          config.additionalProperties().put("appVersion", info.getVersion());
        }
        if (info.getDescription() != null) {
          config.additionalProperties().put("appDescription", info.getDescription());
        }
        if (info.getContact() != null) {
          Contact contact = info.getContact();
          config.additionalProperties().put("infoUrl", contact.getUrl());
          if (contact.getEmail() != null) {
            config.additionalProperties().put("infoEmail", contact.getEmail());
          }
        }
        if (info.getLicense() != null) {
          License license = info.getLicense();
          if (license.getName() != null) {
            config.additionalProperties().put("licenseInfo", license.getName());
          }
          if (license.getUrl() != null) {
            config.additionalProperties().put("licenseUrl", license.getUrl());
          }
        }
        if (info.getVersion() != null) {
          config.additionalProperties().put("version", info.getVersion());
        }
      }

      StringBuilder hostBuilder = new StringBuilder();
      if (swagger.getSchemes() != null && swagger.getSchemes().size() > 0) {
        hostBuilder.append(swagger.getSchemes().get(0).toValue());
        hostBuilder.append("://");
      } else {
        hostBuilder.append("https://");
      }
      if (swagger.getHost() != null) {
        hostBuilder.append(swagger.getHost());
      } else {
        hostBuilder.append("localhost");
      }
      if (swagger.getBasePath() != null) {
        hostBuilder.append(swagger.getBasePath());
      } else {
        hostBuilder.append("/");
      }
      String contextPath = swagger.getBasePath() == null ? "/" : swagger.getBasePath();
      String basePath = hostBuilder.toString();


      List<Object> allOperations = new ArrayList<Object>();
      List<Object> allModels = new ArrayList<Object>();

      // models
      Map<String, Model> definitions = swagger.getDefinitions();
      if (definitions != null) {
        for (String name : definitions.keySet()) {
          Model model = definitions.get(name);
          Map<String, Model> modelMap = new HashMap<String, Model>();
          modelMap.put(name, model);
          Map<String, Object> models = processModels(config, modelMap);
          models.putAll(config.additionalProperties());

          allModels.add(((List<Object>) models.get("models")).get(0));

          for (String templateName : config.modelTemplateFiles().keySet()) {
            String suffix = config.modelTemplateFiles().get(templateName);
            String filename = config.modelFileFolder() + File.separator + config.toModelFilename(name) + suffix;
            String template = readTemplate(config.templateDir() + File.separator + templateName);
            Template tmpl = Mustache.compiler()
                    .withLoader(new Mustache.TemplateLoader() {
                      public Reader getTemplate(String name) {
                        return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                      }
                    })
                    .defaultValue("")
                    .compile(template);
            writeToFile(filename, tmpl.execute(models));
            files.add(new File(filename));
          }
        }
      }
      if (System.getProperty("debugModels") != null) {
        System.out.println("############ Model info ############");
        Json.prettyPrint(allModels);
      }

      // apis
      Map<String, List<CodegenOperation>> paths = processPaths(swagger.getPaths());
      for (String tag : paths.keySet()) {
        List<CodegenOperation> ops = paths.get(tag);
        Map<String, Object> operation = processOperations(config, tag, ops);
        operation.put("basePath", basePath);
        operation.put("contextPath", contextPath);
        operation.put("baseName", tag);
        operation.put("modelPackage", config.modelPackage());
        operation.putAll(config.additionalProperties());
        operation.put("classname", config.toApiName(tag));
        operation.put("classVarName", config.toApiVarName(tag));

        allOperations.add(new HashMap<String, Object>(operation));
        for (int i = 0; i < allOperations.size(); i++) {
          Map<String, Object> oo = (Map<String, Object>) allOperations.get(i);
          if (i < (allOperations.size() - 1)) {
            oo.put("hasMore", "true");
          }
        }

        for (String templateName : config.apiTemplateFiles().keySet()) {
          String suffix = config.apiTemplateFiles().get(templateName);
          String filename = config.apiFileFolder() +
                  File.separator +
                  config.toApiFilename(tag) +
                  suffix;

          String template = readTemplate(config.templateDir() + File.separator + templateName);
          Template tmpl = Mustache.compiler()
                  .withLoader(new Mustache.TemplateLoader() {
                    public Reader getTemplate(String name) {
                      return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                    }
                  })
                  .defaultValue("")
                  .compile(template);

          writeToFile(filename, tmpl.execute(operation));
          files.add(new File(filename));
        }
      }
      if (System.getProperty("debugOperations") != null) {
        System.out.println("############ Operation info ############");
        Json.prettyPrint(allOperations);
      }

      // supporting files
      Map<String, Object> bundle = new HashMap<String, Object>();
      bundle.putAll(config.additionalProperties());
      bundle.put("apiPackage", config.apiPackage());

      Map<String, Object> apis = new HashMap<String, Object>();
      apis.put("apis", allOperations);
      if (swagger.getHost() != null) {
        bundle.put("host", swagger.getHost());
      }
      bundle.put("basePath", basePath);
      bundle.put("contextPath", contextPath);
      bundle.put("apiInfo", apis);
      bundle.put("models", allModels);
      bundle.put("apiFolder", config.apiPackage().replace('.', File.separatorChar));
      bundle.put("modelPackage", config.modelPackage());
      if (swagger.getExternalDocs() != null) {
        bundle.put("externalDocs", swagger.getExternalDocs());
      }
      for (int i = 0; i < allModels.size() - 1; i++) {
        HashMap<String, CodegenModel> cm = (HashMap<String, CodegenModel>) allModels.get(i);
        CodegenModel m = cm.get("model");
        m.hasMoreModels = true;
      }

      config.postProcessSupportingFileData(bundle);

      if (System.getProperty("debugSupportingFiles") != null) {
        System.out.println("############ Supporting file info ############");
        Json.prettyPrint(bundle);
      }

      for (SupportingFile support : config.supportingFiles()) {
        String outputFolder = config.outputFolder();
        if (isNotEmpty(support.folder)) {
          outputFolder += File.separator + support.folder;
        }
        File of = new File(outputFolder);
        if (!of.isDirectory()) {
          of.mkdirs();
        }
        String outputFilename = outputFolder + File.separator + support.destinationFilename;

        if (support.templateFile.endsWith("mustache")) {
          String template = readTemplate(config.templateDir() + File.separator + support.templateFile);
          Template tmpl = Mustache.compiler()
                  .withLoader(new Mustache.TemplateLoader() {
                    public Reader getTemplate(String name) {
                      return getTemplateReader(config.templateDir() + File.separator + name + ".mustache");
                    }
                  })
                  .defaultValue("")
                  .compile(template);

          writeToFile(outputFilename, tmpl.execute(bundle));
          files.add(new File(outputFilename));
        } else {
          InputStream in = this.getClass().getClassLoader().getResourceAsStream(config.templateDir() + File.separator + support.templateFile);
          File outputFile = new File(outputFilename);
          OutputStream out = new FileOutputStream(outputFile);
          IOUtils.copy(in,out);

          files.add(outputFile);
        }
      }

      config.processSwagger(swagger);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return files;
  }

  public Map<String, List<CodegenOperation>> processPaths(Map<String, Path> paths) {
    Map<String, List<CodegenOperation>> ops = new HashMap<String, List<CodegenOperation>>();

    for (String resourcePath : paths.keySet()) {
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

  public SecuritySchemeDefinition fromSecurity(String name) {
    Map<String, SecuritySchemeDefinition> map = swagger.getSecurityDefinitions();
    if (map == null) {
      return null;
    }
    return map.get(name);
  }


  public void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations) {
    if (operation != null) {
      List<String> tags = operation.getTags();
      if (tags == null) {
        tags = new ArrayList<String>();
        tags.add("default");
      }

      for (String tag : tags) {
        CodegenOperation co = config.fromOperation(resourcePath, httpMethod, operation, swagger.getDefinitions());
        co.tags = new ArrayList<String>();
        co.tags.add(sanitizeTag(tag));
        config.addOperationToGroup(sanitizeTag(tag), resourcePath, operation, co, operations);

        List<Map<String, List<String>>> securities = operation.getSecurity();
        if (securities == null) {
          continue;
        }
        Map<String, SecuritySchemeDefinition> authMethods = new HashMap<String, SecuritySchemeDefinition>();
        for (Map<String, List<String>> security : securities) {
          if (security.size() != 1) {
            //Not sure what to do
            continue;
          }
          String securityName = security.keySet().iterator().next();
          SecuritySchemeDefinition securityDefinition = fromSecurity(securityName);
          if (securityDefinition != null) {
            authMethods.put(securityName, securityDefinition);
          }
        }
        if (!authMethods.isEmpty()) {
          co.authMethods = config.fromSecurity(authMethods);
        }
      }
    }
  }

  protected String sanitizeTag(String tag) {
    // remove spaces and make strong case
    String[] parts = tag.split(" ");
    StringBuilder buf = new StringBuilder();
    for (String part : parts) {
      if (isNotEmpty(part)) {
        buf.append(capitalize(part));
      }
    }
    return buf.toString().replaceAll("[^a-zA-Z ]", "");
  }

  public Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops) {
    Map<String, Object> operations = new HashMap<String, Object>();
    Map<String, Object> objs = new HashMap<String, Object>();
    objs.put("classname", config.toApiName(tag));
    objs.put("operation", ops);
    operations.put("operations", objs);
    operations.put("package", config.apiPackage());

    Set<String> allImports = new LinkedHashSet<String>();
    for (CodegenOperation op : ops) {
      allImports.addAll(op.imports);
    }

    List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
    for (String nextImport : allImports) {
      Map<String, String> im = new LinkedHashMap<String, String>();
      String mapping = config.importMapping().get(nextImport);
      if (mapping == null) {
        mapping = config.toModelImport(nextImport);
      }
      if (mapping != null) {
        im.put("import", mapping);
        imports.add(im);
      }
    }

    operations.put("imports", imports);
    config.postProcessOperations(operations);
    if (objs.size() > 0) {
      List<CodegenOperation> os = (List<CodegenOperation>) objs.get("operation");

      if (os != null && os.size() > 0) {
        CodegenOperation op = os.get(os.size() - 1);
        op.hasMore = null;
      }
    }
    return operations;
  }

  public Map<String, Object> processModels(CodegenConfig config, Map<String, Model> definitions) {
    Map<String, Object> objs = new HashMap<String, Object>();
    objs.put("package", config.modelPackage());
    List<Object> models = new ArrayList<Object>();
    Set<String> allImports = new LinkedHashSet<String>();
    for (String key : definitions.keySet()) {
      Model mm = definitions.get(key);
      CodegenModel cm = config.fromModel(key, mm);
      Map<String, Object> mo = new HashMap<String, Object>();
      mo.put("model", cm);
      models.add(mo);
      allImports.addAll(cm.imports);
    }
    objs.put("models", models);

    List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
    for (String nextImport : allImports) {
      Map<String, String> im = new LinkedHashMap<String, String>();
      String mapping = config.importMapping().get(nextImport);
      if (mapping == null) {
        mapping = config.toModelImport(nextImport);
      }
      if (mapping != null && !config.defaultIncludes().contains(mapping)) {
        im.put("import", mapping);
        imports.add(im);
      }
      // add instantiation types
      mapping = config.instantiationTypes().get(nextImport);
      if (mapping != null && !config.defaultIncludes().contains(mapping)) {
        im.put("import", mapping);
        imports.add(im);
      }
    }

    objs.put("imports", imports);
    config.postProcessModels(objs);

    return objs;
  }
}
