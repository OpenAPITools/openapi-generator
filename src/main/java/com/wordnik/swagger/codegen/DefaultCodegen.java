package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class DefaultCodegen {
  String outputFolder = "";
  Set<String> defaultIncludes;
  Map<String, String> typeMapping;
  Set<String> reservedWords;
  Map<String, String> importMapping;
  String modelPackage = "", apiPackage, fileSuffix;
  Map<String, String> apiTemplateFiles = new HashMap<String, String>();
  Map<String, String> modelTemplateFiles = new HashMap<String, String>();
  String templateDir;

  public Set<String> defaultIncludes() {
    return defaultIncludes;
  }
  public Map<String, String> typeMapping() {
    return typeMapping;
  }
  public Set<String> reservedWords() {
    return reservedWords;
  }
  public Map<String, String> importMapping() {
    return importMapping;
  }
  public String modelPackage() {
    return modelPackage;
  }
  public String apiPackage() {
    return apiPackage;
  }
  public String fileSuffix() {
    return fileSuffix;
  }
  public String templateDir() {
    return templateDir;
  }
  public Map<String, String> modelTemplateFiles() {
    return modelTemplateFiles;
  }
  public String modelFileFolder() {
    return outputFolder + File.separator + modelPackage().replaceAll("\\.", File.separator);
  }

  public void setTemplateDir(String templateDir) {
    this.templateDir = templateDir;
  }

  public String toModelFilename(String name) {
    return name;
  }

  public DefaultCodegen() {
    defaultIncludes = new HashSet<String>(
    Arrays.asList("double",
      "int",
      "long",
      "short",
      "char",
      "float",
      "String",
      "boolean",
      "Boolean",
      "Double",
      "Integer",
      "Long",
      "Float")
    );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("Array", "List");
    typeMapping.put("array", "List");
    typeMapping.put("List", "List");
    typeMapping.put("boolean", "Boolean");
    typeMapping.put("string", "String");
    typeMapping.put("int", "Integer");
    typeMapping.put("float", "Float");
    typeMapping.put("number", "BigDecimal");
    typeMapping.put("long", "Long");
    typeMapping.put("short", "Short");
    typeMapping.put("char", "String");
    typeMapping.put("double", "Double");
    typeMapping.put("object", "Object");
    typeMapping.put("integer", "Integer");

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

    importMapping = new HashMap<String, String> ();
    importMapping.put("BigDecimal", "java.math.BigDecimal");
    importMapping.put("UUID", "java.util.UUID");
    importMapping.put("File", "java.io.File");
    importMapping.put("Date", "java.util.Date");
    importMapping.put("Timestamp", "java.sql.Timestamp");
    importMapping.put("Array", "java.util.*");
    importMapping.put("ArrayList", "java.util.*");
    importMapping.put("List", "java.util.*");
    importMapping.put("Set", "java.util.*");
    importMapping.put("DateTime", "org.joda.time.*");
    importMapping.put("LocalDateTime", "org.joda.time.*");
    importMapping.put("LocalDate", "org.joda.time.*");
    importMapping.put("LocalTime", "org.joda.time.*");
  }

  public String toDefaultValue(Property p) {
    if(p instanceof StringProperty)
      return "null";
    else if (p instanceof BooleanProperty)
      return "null";
    else if(p instanceof DateProperty)
      return "null";
    else if(p instanceof DateTimeProperty)
      return "null";
    else if (p instanceof DoubleProperty)
      return "null";
    else if (p instanceof FloatProperty)
      return "null";
    else if (p instanceof IntegerProperty)
      return "null";
    else if (p instanceof LongProperty)
      return "null";
    else if (p instanceof MapProperty)
      return "null";
    else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = toDeclaredType(ap.getItems());
      return "ArrayList<" + inner + ">() ";
    }
    return null;
  }

  public String toDeclaredType(Property p) {
    String datatype = null;
    if(p instanceof StringProperty)
      datatype = "string";
    else if (p instanceof BooleanProperty)
      datatype = "boolean";
    else if(p instanceof DateProperty)
      datatype = "date";
    else if(p instanceof DateTimeProperty)
      datatype = "dateTime";
    else if (p instanceof DoubleProperty)
      datatype = "double";
    else if (p instanceof FloatProperty)
      datatype = "float";
    else if (p instanceof IntegerProperty)
      datatype = "integer";
    else if (p instanceof LongProperty)
      datatype = "long";
    else if (p instanceof MapProperty)
      datatype = "map";
    else {
      if(p != null)
        datatype = p.getType();
    }
    return datatype;
  }

  public CodegenModel fromModel(String name, ModelImpl model) {
    CodegenModel m = new CodegenModel();
    m.name = name;
    m.description = model.getDescription();
    m.classname = name;

    for(String key: model.getProperties().keySet()) {
      Property prop = model.getProperties().get(key);
      m.vars.add(fromProperty(key, prop));
    }
    return m;
  }

  public CodegenProperty fromProperty(String name, Property p) {
    CodegenProperty property = new CodegenProperty();

    property.name = name;

    property.getter = "get" + name.substring(0, 1).toUpperCase() + name.substring(1);
    property.setter = "set" + name.substring(0, 1).toUpperCase() + name.substring(1);

    property.defaultValue = toDefaultValue(p);
    String datatype = toDeclaredType(p);

    if(typeMapping.containsKey(datatype)) {
      property.datatype = typeMapping.get(datatype);
    }
    else
      property.datatype = datatype;
    return property;
  }
}