package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

public class ScalaLagomServerCodegen extends AbstractScalaCodegen implements CodegenConfig {

  private String authScheme = "";
  private boolean authPreemptive=false;
  protected String groupId = "io.swagger";
  protected String artifactId = "scala-lagom-server";
  protected String artifactVersion = "1.0.0";

  public ScalaLagomServerCodegen() {
    super();
    outputFolder = "generated-code/scala-lagom-server";
    modelTemplateFiles.put("model.mustache", ".scala");
    apiTemplateFiles.put("api.mustache", ".scala");
    embeddedTemplateDir = templateDir = "scala-lagom-server";
    apiPackage = "io.swagger.client.api";
    modelPackage = "io.swagger.client.model";

    setReservedWordsLowerCase(
        Arrays.asList(
            // local variable names used in API methods (endpoints)
            "path", "contentTypes", "contentType", "queryParams", "headerParams",
            "formParams", "postBody", "mp", "basePath", "apiInvoker",

            // scala reserved words
            "abstract", "case", "catch", "class", "def", "do", "else", "extends",
            "false", "final", "finally", "for", "forSome", "if", "implicit",
            "import", "lazy", "match", "new", "null", "object", "override", "package",
            "private", "protected", "return", "sealed", "super", "this", "throw",
            "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
    );

    additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
    additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
    additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
    additionalProperties.put("authScheme", authScheme);
    additionalProperties.put("authPreemptive", authPreemptive);

    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
    supportingFiles.add(new SupportingFile("build.properties.mustache", "", "project/build.properties"));
    supportingFiles.add(new SupportingFile("plugins.sbt.mustache", "", "project/plugins.sbt"));

    importMapping.remove("List");
    importMapping.remove("Set");
    importMapping.remove("Map");

    importMapping.put("DateTime", "org.joda.time.DateTime");
    importMapping.put("ListBuffer", "scala.collection.mutable.ListBuffer");

    typeMapping = new HashMap<String, String>();
    typeMapping.put("Integer", "Int");
    typeMapping.put("enum", "NSString");
    typeMapping.put("array", "Seq");
    typeMapping.put("set", "Set");
    typeMapping.put("boolean", "Boolean");
    typeMapping.put("string", "String");
    typeMapping.put("int", "Int");
    typeMapping.put("long", "Long");
    typeMapping.put("float", "Float");
    typeMapping.put("byte", "Byte");
    typeMapping.put("short", "Short");
    typeMapping.put("char", "Char");
    typeMapping.put("long", "Long");
    typeMapping.put("double", "Double");
    typeMapping.put("object", "Any");
    typeMapping.put("file", "File");

    //TODO binary should be mapped to byte array
    // mapped to String as a workaround
    typeMapping.put("binary", "String");
    typeMapping.put("ByteArray", "String");

    instantiationTypes.put("array", "ListBuffer");
    instantiationTypes.put("map", "HashMap");

    cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING,
        CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));
  }

  @Override
  public void processOpts() {
    super.processOpts();

    if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
      setModelPropertyNaming(
          (String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
    }
  }

  public void setModelPropertyNaming(String naming) {
    if ("original".equals(naming) || "camelCase".equals(naming) ||
        "PascalCase".equals(naming) || "snake_case".equals(naming)) {
      this.modelPropertyNaming = naming;
    } else {
      throw new IllegalArgumentException("Invalid model property naming '" +
          naming + "'. Must be 'original', 'camelCase', " +
          "'PascalCase' or 'snake_case'");
    }
  }

  public String getModelPropertyNaming() {
    return this.modelPropertyNaming;
  }

  @Override
  public String toVarName(String name) {
    // sanitize name
    name = sanitizeName(
        name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

    if ("_".equals(name)) {
      name = "_u";
    }

    // if it's all uppper case, do nothing
    if (name.matches("^[A-Z_]*$")) {
      return name;
    }

    name = getNameUsingModelPropertyNaming(name);

    // for reserved word or word starting with number, append _
    if (isReservedWord(name) || name.matches("^\\d.*")) {
      name = escapeReservedWord(name);
    }

    return name;
  }

  @Override
  public String toParamName(String name) {
    // should be the same as variable name
    return toVarName(name);
  }

  private String getNameUsingModelPropertyNaming(String name) {
    switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
      case original:
        return name;
      case camelCase:
        return camelize(name, true);
      case PascalCase:
        return camelize(name);
      case snake_case:
        return underscore(name);
      default:
        throw new IllegalArgumentException("Invalid model property naming '" +
            name + "'. Must be 'original', 'camelCase', " +
            "'PascalCase' or 'snake_case'");
    }

  }

  @Override
  public CodegenType getTag() {
    return CodegenType.SERVER;
  }

  @Override
  public String getName() {
    return "scala-lagom-server";
  }

  @Override
  public String getHelp() {
    return "Generates a Lagom API (Beta) in scala";
  }

  @Override
  public String toOperationId(String operationId) {
    // throw exception if method name is empty
    if (StringUtils.isEmpty(operationId)) {
      throw new RuntimeException("Empty method name (operationId) not allowed");
    }

    // method name cannot use reserved keyword, e.g. return
    if (isReservedWord(operationId)) {
      throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
    }

    return camelize(operationId, true);
  }

  @Override
  public String toModelName(final String name) {
    final String sanitizedName = sanitizeName(modelNamePrefix + name + modelNameSuffix);

    // camelize the model name
    // phone_number => PhoneNumber
    final String camelizedName = camelize(sanitizedName);

    // model name cannot use reserved keyword, e.g. return
    if (isReservedWord(camelizedName)) {
      final String modelName = "Model" + camelizedName;
      LOGGER.warn(
          camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
      return modelName;
    }

    // model name starts with number
    if (name.matches("^\\d.*")) {
      final String modelName =
          "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
      LOGGER.warn(
          name + " (model name starts with number) cannot be used as model name. Renamed to "
              + modelName);
      return modelName;
    }

    return camelizedName;
  }

  @Override
  public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
    objs = super.postProcessModelsEnum(objs);
    List<Object> models = (List<Object>) objs.get("models");
    for (Object _mo : models) {
      Map<String, Object> mo = (Map<String, Object>) _mo;
      CodegenModel cm = (CodegenModel) mo.get("model");

      for (CodegenProperty var : cm.vars) {
        if (var.isEnum) {
          List<Object> enumValues = (List<Object>) var.allowableValues.get("values");

          for (final ListIterator<Object> i = enumValues.listIterator(); i.hasNext(); ) {
            final String element = String.valueOf(i.next());
            i.set(element.replaceAll("^\"|\"$", ""));
          }
        }
      }
    }

    //Needed import for Gson based libraries
    if (additionalProperties.containsKey("gson")) {
      List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");

      for (Object _mo : models) {
        Map<String, Object> mo = (Map<String, Object>) _mo;
        CodegenModel cm = (CodegenModel) mo.get("model");
        // for enum model
        if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
          cm.imports.add(importMapping.get("SerializedName"));
          Map<String, String> item = new HashMap<String, String>();
          item.put("import", importMapping.get("SerializedName"));
          imports.add(item);
        }
      }
    }

    return objs;
  }

  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
    Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
    ArrayList<CodegenOperation> oplist = (ArrayList<CodegenOperation>) operations.get("operation");

    for (CodegenOperation codegenOperation : oplist) {
      String path = codegenOperation.path;
      codegenOperation.path = path.replaceAll("\\{", ":").replaceAll("}", "");
    }
    return objs;
  }


}
