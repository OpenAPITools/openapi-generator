package com.wordnik.swagger.codegen.languages;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.Model;
import com.wordnik.swagger.models.Operation;
import com.wordnik.swagger.models.parameters.HeaderParameter;
import com.wordnik.swagger.models.parameters.Parameter;
import com.wordnik.swagger.models.properties.*;
import org.apache.commons.lang.StringUtils;

import javax.annotation.Nullable;
import java.util.*;
import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SwiftGenerator extends DefaultCodegen implements CodegenConfig {
  private static final Pattern PATH_PARAM_PATTERN = Pattern.compile("\\{[a-zA-Z_]+\\}");
  protected String sourceFolder = "Classes/Swaggers";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "swift";
  }

  public String getHelp() {
    return "Generates a swift client library.";
  }

  public SwiftGenerator() {
    super();
    outputFolder = "generated-code/swift";
    modelTemplateFiles.put("model.mustache", ".swift");
    apiTemplateFiles.put("api.mustache", ".swift");
    templateDir = "swift";
    apiPackage = "/APIs";
    modelPackage = "/Models";

    // Inject application name
    String appName = System.getProperty("appName");
    if (appName == null) {
      appName = "SwaggerClient";
    }
    additionalProperties.put("projectName", appName);

    // Inject base url override
    String basePathOverride = System.getProperty("basePathOverride");
    if (basePathOverride != null) {
      additionalProperties.put("basePathOverride", basePathOverride);
    }

    sourceFolder = appName + "/" + sourceFolder;

    supportingFiles.add(new SupportingFile("Cartfile.mustache", "", "Cartfile"));
    supportingFiles.add(new SupportingFile("APIHelper.mustache", sourceFolder, "APIHelper.swift"));
    supportingFiles.add(new SupportingFile("AlamofireImplementations.mustache", sourceFolder, "AlamofireImplementations.swift"));
    supportingFiles.add(new SupportingFile("Extensions.mustache", sourceFolder, "Extensions.swift"));
    supportingFiles.add(new SupportingFile("Models.mustache", sourceFolder, "Models.swift"));
    supportingFiles.add(new SupportingFile("APIs.mustache", sourceFolder, "APIs.swift"));

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "Int",
        "Float",
        "Double",
        "Bool",
        "Void",
        "String",
        "Character")
    );
    defaultIncludes = new HashSet<String>(
      Arrays.asList(
        "NSDate",
        "Array",
        "Dictionary",
        "Set",
        "Any",
        "Empty",
        "AnyObject")
    );
    reservedWords = new HashSet<String>(
      Arrays.asList(
        "class", "break", "as", "associativity", "deinit", "case", "dynamicType", "convenience", "enum", "continue",
        "false", "dynamic", "extension", "default", "is", "didSet", "func", "do", "nil", "final", "import", "else",
        "self", "get", "init", "fallthrough", "Self", "infix", "internal", "for", "super", "inout", "let", "if",
        "true", "lazy", "operator", "in", "COLUMN", "left", "private", "return", "FILE", "mutating", "protocol",
        "switch", "FUNCTION", "none", "public", "where", "LINE", "nonmutating", "static", "while", "optional",
        "struct", "override", "subscript", "postfix", "typealias", "precedence", "var", "prefix", "Protocol",
        "required", "right", "set", "Type", "unowned", "weak")
    );

    typeMapping = new HashMap<String, String>();
    typeMapping.put("array", "Array");
    typeMapping.put("List", "Array");
    typeMapping.put("map", "Dictionary");
    typeMapping.put("date", "NSDate");
    typeMapping.put("Date", "NSDate");
    typeMapping.put("DateTime", "NSDate");
    typeMapping.put("boolean", "Bool");
    typeMapping.put("string", "String");
    typeMapping.put("char", "Character");
    typeMapping.put("short", "Int");
    typeMapping.put("int", "Int");
    typeMapping.put("long", "Int");
    typeMapping.put("integer", "Int");
    typeMapping.put("Integer", "Int");
    typeMapping.put("float", "Float");
    typeMapping.put("number", "Double");
    typeMapping.put("double", "Double");
    typeMapping.put("object", "AnyObject");
    typeMapping.put("file", "NSData");

    importMapping = new HashMap<String, String>();
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;  // add an underscore to the name
  }

  @Override
  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + apiPackage().replace('.', File.separatorChar);
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return "[" + getTypeDeclaration(inner) + "]";
    } else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();
      return "[String:" + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if (typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if (languageSpecificPrimitives.contains(type))
        return toModelName(type);
    } else
      type = swaggerType;
    return toModelName(type);
  }

  @Override
  public String toDefaultValue(Property p) {
    // nil
    return null;
  }

  @Override
  public String toInstantiationType(Property p) {
    if (p instanceof MapProperty) {
      MapProperty ap = (MapProperty) p;
      String inner = getSwaggerType(ap.getAdditionalProperties());
      return "[String:" + inner + "]";
    } else if (p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      String inner = getSwaggerType(ap.getItems());
      return "[" + inner + "]";
    }
    return null;
  }

  @Override
  public CodegenProperty fromProperty(String name, Property p) {
    CodegenProperty codegenProperty = super.fromProperty(name, p);
    if (codegenProperty.isEnum) {
      List<Map<String, String>> swiftEnums = new ArrayList<Map<String, String>>();
      List<String> values = (List<String>) codegenProperty.allowableValues.get("values");
      for (String value : values) {
        Map<String, String> map = new HashMap<String, String>();
        map.put("enum", StringUtils.capitalize(value));
        map.put("raw", value);
        swiftEnums.add(map);
      }
      codegenProperty.allowableValues.put("values", swiftEnums);
      codegenProperty.datatypeWithEnum =
              StringUtils.left(codegenProperty.datatypeWithEnum, codegenProperty.datatypeWithEnum.length() - "Enum".length());
    }
    return codegenProperty;
  }

  @Override
  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultAPI";
    return initialCaps(name) + "API";
  }

  @Override
  public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions) {
    path = normalizePath(path);
    List<Parameter> parameters = operation.getParameters();
    parameters = Lists.newArrayList(Iterators.filter(parameters.iterator(), new Predicate<Parameter>() {
      @Override
      public boolean apply(@Nullable Parameter parameter) {
        return !(parameter instanceof HeaderParameter);
      }
    }));
    operation.setParameters(parameters);
    return super.fromOperation(path, httpMethod, operation, definitions);
  }

  private static String normalizePath(String path) {
    StringBuilder builder = new StringBuilder();

    int cursor = 0;
    Matcher matcher = PATH_PARAM_PATTERN.matcher(path);
    boolean found = matcher.find();
    while (found) {
      String stringBeforeMatch = path.substring(cursor, matcher.start());
      builder.append(stringBeforeMatch);

      String group = matcher.group().substring(1, matcher.group().length() - 1);
      group = camelize(group, true);
      builder
              .append("{")
              .append(group)
              .append("}");

      cursor = matcher.end();
      found = matcher.find();
    }

    String stringAfterMatch = path.substring(cursor);
    builder.append(stringAfterMatch);

    return builder.toString();
  }
}