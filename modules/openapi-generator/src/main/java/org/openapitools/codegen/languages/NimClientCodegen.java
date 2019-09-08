package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class NimClientCodegen extends DefaultCodegen implements CodegenConfig {
  static Logger LOGGER = LoggerFactory.getLogger(NimClientCodegen.class);

  public static final String PROJECT_NAME = "projectName";

  protected String packageName = "openapiclient";
  protected String packageVersion = "1.0.0";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "nim";
  }

  public String getHelp() {
    return "Generates a nim client.";
  }

  public NimClientCodegen() {
    super();

    outputFolder = "generated-code" + File.separator + "nim";
    modelTemplateFiles.put("model.mustache", ".nim");
    apiTemplateFiles.put("api.mustache", ".nim");
    embeddedTemplateDir = templateDir = "nim-client";
    apiPackage = File.separator + packageName + File.separator + "apis";
    modelPackage = File.separator + packageName + File.separator + "models";
    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("sample_client.mustache", "", "sample_client.nim"));
    supportingFiles.add(new SupportingFile("config.mustache", "", "config.nim"));

    setReservedWordsLowerCase(
            Arrays.asList(
                    "addr", "and", "as", "asm",
                    "bind", "block", "break",
                    "case", "cast", "concept", "const", "continue", "converter",
                    "defer", "discard", "distinct", "div", "do",
                    "elif", "else", "end", "enum", "except", "export",
                    "finally", "for", "from", "func",
                    "if", "import", "in", "include", "interface", "is", "isnot", "iterator",
                    "let",
                    "macro", "method", "mixin", "mod",
                    "nil", "not", "notin",
                    "object", "of", "or", "out",
                    "proc", "ptr",
                    "raise", "ref", "return",
                    "shl", "shr", "static",
                    "template", "try", "tuple", "type",
                    "using",
                    "var",
                    "when", "while",
                    "xor",
                    "yield"
            )
    );

    defaultIncludes = new HashSet<String>(
            Arrays.asList(
                    "array"
            )
    );

    languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList(
                    "int",
                    "int8",
                    "int16",
                    "int32",
                    "int64",
                    "uint",
                    "uint8",
                    "uint16",
                    "uint32",
                    "uint64",
                    "float",
                    "float32",
                    "float64",
                    "bool",
                    "char",
                    "string",
                    "cstring",
                    "pointer")
    );

    typeMapping.clear();
    typeMapping.put("integer", "int");
    typeMapping.put("long", "int64");
    typeMapping.put("number", "float");
    typeMapping.put("float", "float");
    typeMapping.put("double", "float64");
    typeMapping.put("boolean", "bool");
    typeMapping.put("UUID", "string");
    typeMapping.put("URI", "string");
    typeMapping.put("date", "string");
    typeMapping.put("DateTime", "string");
    typeMapping.put("password", "string");
    typeMapping.put("file", "string");
  }

  public void setPackageName(String packageName) {
    this.packageName = packageName;
  }

  public void setPackageVersion(String packageVersion) {
    this.packageVersion = packageVersion;
  }

  @Override
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    return postProcessModelsEnum(objs);
  }

  @Override
  public void processOpts() {
    super.processOpts();

    if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
      setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
    }

    if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
      setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
    }

    additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
    additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

    apiPackage = File.separator + packageName + File.separator + "apis";
    modelPackage = File.separator + packageName + File.separator + "models";
  }

  @Override
  public String escapeReservedWord(String name) {
    LOGGER.warn("A reserved word \"" + name + "\" is used. Consider renaming the field name");
    if (this.reservedWordsMappings().containsKey(name)) {
      return this.reservedWordsMappings().get(name);
    }
    return "`" + name + "`";
  }

  @Override
  public String escapeQuotationMark(String input) {
    return input.replace("\"", "");
  }

  @Override
  public String escapeUnsafeCharacters(String input) {
    return input.replace("*/", "*_/").replace("/*", "/_*");
  }

  @Override
  public String toModelImport(String name) {
    name = name.replaceAll("-", "_");
    if (importMapping.containsKey(name)) {
      return "model_" + StringUtils.underscore(importMapping.get(name));
    } else {
      return "model_" + StringUtils.underscore(name);
    }
  }

  @Override
  public String toApiImport(String name) {
    name = name.replaceAll("-", "_");
    if (importMapping.containsKey(name)) {
      return "api_" + StringUtils.underscore(importMapping.get(name));
    } else {
      return "api_" + StringUtils.underscore(name);
    }
  }

  @Override
  public String toModelFilename(String name) {
    name = name.replaceAll("-", "_");
    return "model_" + StringUtils.underscore(name);
  }

  @Override
  public String toApiFilename(String name) {
    name = name.replaceAll("-", "_");
    return "api_" + StringUtils.underscore(name);
  }

  @Override
  public String toOperationId(String operationId) {
    String sanitizedOperationId = sanitizeName(operationId);

    if (isReservedWord(sanitizedOperationId)) {
      sanitizedOperationId = "call" + StringUtils.camelize(sanitizedOperationId, false);
    }

    return StringUtils.camelize(sanitizedOperationId, true);
  }

  @Override
  public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
    @SuppressWarnings("unchecked")
    Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
    @SuppressWarnings("unchecked")
    List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
    for (CodegenOperation operation : operations) {
      operation.httpMethod = operation.httpMethod.toLowerCase(Locale.ROOT);
    }

    return objs;
  }

  @Override
  public String getTypeDeclaration(Schema p) {
    if (ModelUtils.isArraySchema(p)) {
      ArraySchema ap = (ArraySchema) p;
      Schema inner = ap.getItems();
      if (inner == null) {
        return null;
      }
      return "seq[" + getTypeDeclaration(inner) + "]";
    } else if (ModelUtils.isMapSchema(p)) {
      Schema inner = ModelUtils.getAdditionalProperties(p);
      if (inner == null) {
        inner = new StringSchema();
      }
      return "Table[string, " + getTypeDeclaration(inner) + "]";
    }

    String schemaType = getSchemaType(p);
    if (typeMapping.containsKey(schemaType)) {
      return typeMapping.get(schemaType);
    }

    return schemaType;
  }

  @Override
  protected boolean needToImport(String type) {
    if (defaultIncludes.contains(type)) {
      return false;
    } else if (languageSpecificPrimitives.contains(type)) {
      return false;
    } else if (typeMapping.containsKey(type) && languageSpecificPrimitives.contains(typeMapping.get(type))) {
      return false;
    }

    return true;
  }

  @Override
  public String toEnumName(CodegenProperty property) {
    return StringUtils.camelize(property.name, false);
  }

  @Override
  public String toEnumVarName(String name, String datatype) {
    return StringUtils.camelize(name, false);
  }
}
