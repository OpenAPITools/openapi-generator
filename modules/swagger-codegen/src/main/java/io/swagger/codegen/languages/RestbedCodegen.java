package io.swagger.codegen.languages;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Model;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BaseIntegerProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

public class RestbedCodegen extends AbstractCppCodegen {

  public static final String DECLSPEC = "declspec";
  public static final String DEFAULT_INCLUDE = "defaultInclude";

  protected String packageVersion = "1.0.0";
  protected String declspec = "";
  protected String defaultInclude = "";

  /**
   * Configures the type of generator.
   * 
   * @return the CodegenType for this generator
   * @see io.swagger.codegen.CodegenType
   */
  public CodegenType getTag() {
      return CodegenType.SERVER;
  }

  /**
   * Configures a friendly name for the generator. This will be used by the
   * generator to select the library with the -l flag.
   * 
   * @return the friendly name for the generator
   */
  public String getName() {
      return "restbed";
  }

  /**
   * Returns human-friendly help for the generator. Provide the consumer with
   * help tips, parameters here
   * 
   * @return A string value for the help message
   */
  public String getHelp() {
      return "Generates a C++ API Server with Restbed (https://github.com/Corvusoft/restbed).";
  }

  public RestbedCodegen() {
      super();

      apiPackage = "io.swagger.server.api";
      modelPackage = "io.swagger.server.model";

      modelTemplateFiles.put("model-header.mustache", ".h");
      modelTemplateFiles.put("model-source.mustache", ".cpp");

      apiTemplateFiles.put("api-header.mustache", ".h");
      apiTemplateFiles.put("api-source.mustache", ".cpp");

      embeddedTemplateDir = templateDir = "restbed";

      cliOptions.clear();

      // CLI options
      addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
              this.modelPackage);
      addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
              this.apiPackage);
      addOption(CodegenConstants.PACKAGE_VERSION, "C++ package version.", this.packageVersion);
      addOption(DECLSPEC, "C++ preprocessor to place before the class name for handling dllexport/dllimport.",
              this.declspec);
      addOption(DEFAULT_INCLUDE,
              "The default include statement that should be placed in all headers for including things like the declspec (convention: #include \"Commons.h\" ",
              this.defaultInclude);
      
      supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
      supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
      supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
      
      languageSpecificPrimitives = new HashSet<String>(
              Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

      typeMapping = new HashMap<String, String>();
      typeMapping.put("date", "std::string");
      typeMapping.put("DateTime", "std::string");
      typeMapping.put("string", "std::string");
      typeMapping.put("integer", "int32_t");
      typeMapping.put("long", "int64_t");
      typeMapping.put("boolean", "bool");
      typeMapping.put("array", "std::vector");
      typeMapping.put("map", "std::map");
      typeMapping.put("file", "std::string");
      typeMapping.put("object", "Object");
      typeMapping.put("binary", "restbed::Bytes");
      typeMapping.put("number", "double");
      typeMapping.put("UUID", "std::string");

      super.importMapping = new HashMap<String, String>();
      importMapping.put("std::vector", "#include <vector>");
      importMapping.put("std::map", "#include <map>");
      importMapping.put("std::string", "#include <string>");
      importMapping.put("Object", "#include \"Object.h\"");
      importMapping.put("restbed::Bytes", "#include <corvusoft/restbed/byte.hpp>");
  }

  @Override
  public void processOpts() {
      super.processOpts();

      if (additionalProperties.containsKey(DECLSPEC)) {
          declspec = additionalProperties.get(DECLSPEC).toString();
      }

      if (additionalProperties.containsKey(DEFAULT_INCLUDE)) {
          defaultInclude = additionalProperties.get(DEFAULT_INCLUDE).toString();
      }

      additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
      additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
      additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
      additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
      additionalProperties.put("declspec", declspec);
      additionalProperties.put("defaultInclude", defaultInclude);
  }

  /**
   * Escapes a reserved word as defined in the `reservedWords` array. Handle
   * escaping those terms here. This logic is only called if a variable
   * matches the reserved words
   * 
   * @return the escaped term
   */
  @Override
  public String escapeReservedWord(String name) {
      return "_" + name; // add an underscore to the name
  }

  /**
   * Location to write model files. You can use the modelPackage() as defined
   * when the class is instantiated
   */
  public String modelFileFolder() {
      return (outputFolder + "/model").replace("/", File.separator);
  }

  /**
   * Location to write api files. You can use the apiPackage() as defined when
   * the class is instantiated
   */
  @Override
  public String apiFileFolder() {
      return (outputFolder + "/api").replace("/", File.separator);
  }

  @Override
  public String toModelImport(String name) {
      if (importMapping.containsKey(name)) {
          return importMapping.get(name);
      } else {
          return "#include \"" + name + ".h\"";
      }
  }

  @Override
  public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
      CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

      Set<String> oldImports = codegenModel.imports;
      codegenModel.imports = new HashSet<String>();
      for (String imp : oldImports) {
          String newImp = toModelImport(imp);
          if (!newImp.isEmpty()) {
              codegenModel.imports.add(newImp);
          }
      }

      return codegenModel;
  }


  @Override
  public String toModelFilename(String name) {
      return initialCaps(name);
  }

  @Override
  public String toApiFilename(String name) {
      return initialCaps(name) + "Api";
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
      Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
      List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
      List<CodegenOperation> newOpList = new ArrayList<CodegenOperation>();
      for (CodegenOperation op : operationList) {
          String path = new String(op.path);

          String[] items = path.split("/", -1);
          String resourceNameCamelCase = "";
          op.path = "";
          for (String item: items) {
              if (item.length() > 1) {
                  if (item.matches("^\\{(.*)\\}$")) { 
                      String tmpResourceName = item.substring(1, item.length()-1);
                      resourceNameCamelCase += Character.toUpperCase(tmpResourceName.charAt(0)) + tmpResourceName.substring(1);
                      item = item.substring(0, item.length()-1);
                      item += ": .*}";
                  } else {
                      resourceNameCamelCase +=  Character.toUpperCase(item.charAt(0)) + item.substring(1);
                  }
              } else if (item.length() == 1) {
                  resourceNameCamelCase +=  Character.toUpperCase(item.charAt(0));
              }
              op.path += item + "/";
          }
          op.vendorExtensions.put("x-codegen-resourceName", resourceNameCamelCase);
          boolean foundInNewList = false;
          for (CodegenOperation op1 : newOpList) {
              if (!foundInNewList) {
                  if (op1.path.equals(op.path)) {
                      foundInNewList = true;
                      List<CodegenOperation> currentOtherMethodList = (List<CodegenOperation>) op1.vendorExtensions.get("x-codegen-otherMethods");
                      if (currentOtherMethodList == null) {
                          currentOtherMethodList = new ArrayList<CodegenOperation>();
                      }
                      op.operationIdCamelCase = op1.operationIdCamelCase;
                      currentOtherMethodList.add(op);
                      op1.vendorExtensions.put("x-codegen-otherMethods", currentOtherMethodList);
                  }
              }
          }
          if (!foundInNewList) {
              newOpList.add(op);
          }
      }
      operations.put("operation", newOpList);
      return objs;
  }

  /**
   * Optional - type declaration. This is a String which is used by the
   * templates to instantiate your types. There is typically special handling
   * for different property types
   *
   * @return a string value used as the `dataType` field for model templates,
   *         `returnType` for api templates
   */
  @Override
  public String getTypeDeclaration(Property p) {
      String swaggerType = getSwaggerType(p);

      if (p instanceof ArrayProperty) {
          ArrayProperty ap = (ArrayProperty) p;
          Property inner = ap.getItems();
          return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
      }
      if (p instanceof MapProperty) {
          MapProperty mp = (MapProperty) p;
          Property inner = mp.getAdditionalProperties();
          return getSwaggerType(p) + "<std::string, " + getTypeDeclaration(inner) + ">";
      }
      if (p instanceof StringProperty || p instanceof DateProperty
              || p instanceof DateTimeProperty || p instanceof FileProperty
              || languageSpecificPrimitives.contains(swaggerType)) {
          return toModelName(swaggerType);
      }

      return "std::shared_ptr<" + swaggerType + ">";
  }

  @Override
  public String toDefaultValue(Property p) {
      if (p instanceof StringProperty) {
          return "\"\"";
      } else if (p instanceof BooleanProperty) {
          return "false";
      } else if (p instanceof DateProperty) {
          return "\"\"";
      } else if (p instanceof DateTimeProperty) {
          return "\"\"";
      } else if (p instanceof DoubleProperty) {
          return "0.0";
      } else if (p instanceof FloatProperty) {
          return "0.0f";
      } else if (p instanceof LongProperty) {
          return "0L";
      } else if (p instanceof IntegerProperty || p instanceof BaseIntegerProperty) {
          return "0";
      } else if (p instanceof DecimalProperty) {
          return "0.0";
      } else if (p instanceof MapProperty) {
          MapProperty ap = (MapProperty) p;
          String inner = getSwaggerType(ap.getAdditionalProperties());
          return "std::map<std::string, " + inner + ">()";
      } else if (p instanceof ArrayProperty) {
          ArrayProperty ap = (ArrayProperty) p;
          String inner = getSwaggerType(ap.getItems());
          if (!languageSpecificPrimitives.contains(inner)) {
              inner = "std::shared_ptr<" + inner + ">";
          }
          return "std::vector<" + inner + ">()";
      } else if (p instanceof RefProperty) {
          RefProperty rp = (RefProperty) p;
          return "new " + toModelName(rp.getSimpleRef()) + "()";
      }
      return "nullptr";
  }

  @Override
  public void postProcessParameter(CodegenParameter parameter) {
      super.postProcessParameter(parameter);

      boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
      boolean isListContainer = parameter.isListContainer == Boolean.TRUE;
      boolean isString = parameter.isString == Boolean.TRUE;

      if (!isPrimitiveType && !isListContainer && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
          parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
      }
  }

  /**
   * Optional - swagger type conversion. This is used to map swagger types in
   * a `Property` into either language specific types via `typeMapping` or
   * into complex models if there is not a mapping.
   *
   * @return a string value of the type or complex model for this property
   * @see io.swagger.models.properties.Property
   */
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
  public String toModelName(String type) {
      if (typeMapping.keySet().contains(type) || typeMapping.values().contains(type)
              || importMapping.values().contains(type) || defaultIncludes.contains(type)
              || languageSpecificPrimitives.contains(type)) {
          return type;
      } else {
          return Character.toUpperCase(type.charAt(0)) + type.substring(1);
      }
  }

  @Override
  public String toApiName(String type) {
      return Character.toUpperCase(type.charAt(0)) + type.substring(1) + "Api";
  }

  @Override
  public String escapeQuotationMark(String input) {
      // remove " to avoid code injection
      return input.replace("\"", "");
  }

  @Override
  public String escapeUnsafeCharacters(String input) {
      return input.replace("*/", "*_/").replace("/*", "/_*");
  }


}
