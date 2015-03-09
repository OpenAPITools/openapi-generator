package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;
import com.wordnik.swagger.util.Json;

import java.util.*;
import java.io.File;

public class ScalatraServerCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";
  protected String sourceFolder = "src/main/scala";

  public CodegenType getTag() {
    return CodegenType.SERVER;
  }

  public String getName() {
    return "scalatra";
  }

  public String getHelp() {
    return "Generates a Scala server application with Scalatra.";
  }

  public ScalatraServerCodegen() {
    super();
    outputFolder = "generated-code/scalatra";
    modelTemplateFiles.put("model.mustache", ".scala");
    apiTemplateFiles.put("api.mustache", ".scala");
    templateDir = "scalatra";
    apiPackage = "com.wordnik.client.api";
    modelPackage = "com.wordnik.client.model";

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "abstract", "continue", "for", "new", "switch", "assert", 
        "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", 
        "this", "break", "double", "implements", "protected", "throw", "byte", "else", 
        "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", 
        "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", 
        "void", "class", "finally", "long", "strictfp", "volatile", "const", "float", 
        "native", "super", "while")
    );

    defaultIncludes = new HashSet<String>(
      Arrays.asList("double",
        "Int",
        "Long",
        "Float",
        "Double",
        "char",
        "float",
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Integer",
        "Long",
        "Float",
        "List",
        "Set",
        "Map")
      );

    typeMapping.put("integer", "Int");
    typeMapping.put("long", "Long");

    additionalProperties.put("appName", "Swagger Sample");
    additionalProperties.put("appName", "Swagger Sample");
    additionalProperties.put("appDescription", "A sample swagger server");
    additionalProperties.put("infoUrl", "http://developers.helloreverb.com");
    additionalProperties.put("infoEmail", "hello@helloreverb.com");
    additionalProperties.put("licenseInfo", "All rights reserved");
    additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("build.sbt", "", "build.sbt")); 
    supportingFiles.add(new SupportingFile("web.xml", "/src/main/webapp/WEB-INF", "web.xml"));
    supportingFiles.add(new SupportingFile("JettyMain.scala", sourceFolder, "JettyMain.scala"));
    supportingFiles.add(new SupportingFile("Bootstrap.mustache", sourceFolder, "ScalatraBootstrap.scala"));
    supportingFiles.add(new SupportingFile("ServletApp.mustache", sourceFolder, "ServletApp.scala"));
    supportingFiles.add(new SupportingFile("project/build.properties", "project", "build.properties"));
    supportingFiles.add(new SupportingFile("project/plugins.sbt", "project", "plugins.sbt"));
    supportingFiles.add(new SupportingFile("sbt", "", "sbt"));

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Integer",
        "Long",
        "Float",
        "Object")
      );
    instantiationTypes.put("array", "ArrayList");
    instantiationTypes.put("map", "HashMap");

    importMapping = new HashMap<String, String> ();
    importMapping.put("BigDecimal", "java.math.BigDecimal");
    importMapping.put("UUID", "java.util.UUID");
    importMapping.put("File", "java.io.File");
    importMapping.put("Date", "java.util.Date");
    importMapping.put("Timestamp", "java.sql.Timestamp");
    importMapping.put("Map", "java.util.Map");
    importMapping.put("HashMap", "java.util.HashMap");
    importMapping.put("Array", "java.util.List");
    importMapping.put("ArrayList", "java.util.ArrayList");
    importMapping.put("DateTime", "org.joda.time.DateTime");
    importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
    importMapping.put("LocalDate", "org.joda.time.LocalDate");
    importMapping.put("LocalTime", "org.joda.time.LocalTime");
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
    for(CodegenOperation op: operationList) {
      op.httpMethod = op.httpMethod.toLowerCase();
    }
    return objs;
  }


  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type))
        return toModelName(type);
    }
    else
      type = swaggerType;
    return toModelName(type);
  }
}