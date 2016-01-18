package io.swagger.codegen.languages;

import com.google.common.base.Predicate;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import io.swagger.codegen.*;
import io.swagger.models.Swagger;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.parameters.HeaderParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;

import javax.annotation.Nullable;
import java.util.*;
import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SwiftCodegen extends DefaultCodegen implements CodegenConfig {
  public static final String PROJECT_NAME = "projectName";
  public static final String RESPONSE_AS = "responseAs";
  public static final String UNWRAP_REQUIRED = "unwrapRequired";
  public static final String POD_SOURCE = "podSource";
  public static final String POD_AUTHORS = "podAuthors";
  public static final String POD_SOCIAL_MEDIA_URL = "podSocialMediaURL";
  public static final String POD_DOCSET_URL = "podDocsetURL";
  public static final String POD_LICENSE = "podLicense";
  public static final String POD_HOMEPAGE = "podHomepage";
  public static final String POD_SUMMARY = "podSummary";
  public static final String POD_DESCRIPTION = "podDescription";
  public static final String POD_SCREENSHOTS = "podScreenshots";
  public static final String POD_DOCUMENTATION_URL = "podDocumentationURL";
  protected static final String LIBRARY_PROMISE_KIT = "PromiseKit";
  protected static final String[] RESPONSE_LIBRARIES = { LIBRARY_PROMISE_KIT };
  protected String projectName = "SwaggerClient";
  protected boolean unwrapRequired;
  protected String[] responseAs = new String[0];
  protected String sourceFolder = "Classes" + File.separator + "Swaggers";
  private static final Pattern PATH_PARAM_PATTERN = Pattern.compile("\\{[a-zA-Z_]+\\}");

  @Override
  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  @Override
  public String getName() {
    return "swift";
  }

  @Override
  public String getHelp() {
    return "Generates a swift client library.";
  }

  public SwiftCodegen() {
    super();
    outputFolder = "generated-code" + File.separator + "swift";
    modelTemplateFiles.put("model.mustache", ".swift");
    apiTemplateFiles.put("api.mustache", ".swift");
    embeddedTemplateDir = templateDir = "swift";
    apiPackage = File.separator + "APIs";
    modelPackage = File.separator + "Models";

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
    typeMapping.put("object", "String");
    typeMapping.put("file", "NSURL");
    //TODO binary should be mapped to byte array
    // mapped to String as a workaround
    typeMapping.put("binary", "String");

    importMapping = new HashMap<String, String>();

    cliOptions.add(new CliOption(PROJECT_NAME, "Project name in Xcode"));
    cliOptions.add(new CliOption(RESPONSE_AS, "Optionally use libraries to manage response.  Currently " +
            StringUtils.join(RESPONSE_LIBRARIES, ", ") + " are available."));
    cliOptions.add(new CliOption(UNWRAP_REQUIRED, "Treat 'required' properties in response as non-optional " +
            "(which would crash the app if api returns null as opposed to required option specified in json schema"));
    cliOptions.add(new CliOption(POD_SOURCE, "Source information used for Podspec"));
    cliOptions.add(new CliOption(CodegenConstants.POD_VERSION, "Version used for Podspec"));
    cliOptions.add(new CliOption(POD_AUTHORS, "Authors used for Podspec"));
    cliOptions.add(new CliOption(POD_SOCIAL_MEDIA_URL, "Social Media URL used for Podspec"));
    cliOptions.add(new CliOption(POD_DOCSET_URL, "Docset URL used for Podspec"));
    cliOptions.add(new CliOption(POD_LICENSE, "License used for Podspec"));
    cliOptions.add(new CliOption(POD_HOMEPAGE, "Homepage used for Podspec"));
    cliOptions.add(new CliOption(POD_SUMMARY, "Summary used for Podspec"));
    cliOptions.add(new CliOption(POD_DESCRIPTION, "Description used for Podspec"));
    cliOptions.add(new CliOption(POD_SCREENSHOTS, "Screenshots used for Podspec"));
    cliOptions.add(new CliOption(POD_DOCUMENTATION_URL, "Documentation URL used for Podspec"));
  }

  @Override
  public void processOpts() {
    super.processOpts();

    // Setup project name
    if (additionalProperties.containsKey(PROJECT_NAME)) {
      setProjectName((String) additionalProperties.get(PROJECT_NAME));
    } else {
      additionalProperties.put(PROJECT_NAME, projectName);
    }
    sourceFolder = projectName + File.separator + sourceFolder;

    // Setup unwrapRequired option, which makes all the properties with "required" non-optional
    if (additionalProperties.containsKey(UNWRAP_REQUIRED)) {
      setUnwrapRequired(Boolean.parseBoolean(String.valueOf(additionalProperties.get(UNWRAP_REQUIRED))));
    }
    additionalProperties.put(UNWRAP_REQUIRED, unwrapRequired);

    // Setup unwrapRequired option, which makes all the properties with "required" non-optional
    if (additionalProperties.containsKey(RESPONSE_AS)) {
      Object responseAsObject = additionalProperties.get(RESPONSE_AS);
      if (responseAsObject instanceof String) {
        setResponseAs(((String)responseAsObject).split(","));
      } else {
        setResponseAs((String[]) responseAsObject);
      }
    }
    additionalProperties.put(RESPONSE_AS, responseAs);
    if (ArrayUtils.contains(responseAs, LIBRARY_PROMISE_KIT)) {
      additionalProperties.put("usePromiseKit", true);
    }

    supportingFiles.add(new SupportingFile("Podspec.mustache", "", projectName + ".podspec"));
    supportingFiles.add(new SupportingFile("Cartfile.mustache", "", "Cartfile"));
    supportingFiles.add(new SupportingFile("APIHelper.mustache", sourceFolder, "APIHelper.swift"));
    supportingFiles.add(new SupportingFile("AlamofireImplementations.mustache", sourceFolder,
            "AlamofireImplementations.swift"));
    supportingFiles.add(new SupportingFile("Extensions.mustache", sourceFolder, "Extensions.swift"));
    supportingFiles.add(new SupportingFile("Models.mustache", sourceFolder, "Models.swift"));
    supportingFiles.add(new SupportingFile("APIs.mustache", sourceFolder, "APIs.swift"));
  }

  @Override
  public String escapeReservedWord(String name) {
    return "Swagger" + name;  // add an underscore to the name
  }

  @Override
  public String modelFileFolder() {
    return outputFolder + File.separator + sourceFolder + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + File.separator + sourceFolder + apiPackage().replace('.', File.separatorChar);
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
        map.put("enum", toSwiftyEnumName(value));
        map.put("raw", value);
        swiftEnums.add(map);
      }
      codegenProperty.allowableValues.put("values", swiftEnums);
      codegenProperty.datatypeWithEnum =
              StringUtils.left(codegenProperty.datatypeWithEnum, codegenProperty.datatypeWithEnum.length() - "Enum".length());
      if (reservedWords.contains(codegenProperty.datatypeWithEnum)) {
        codegenProperty.datatypeWithEnum = escapeReservedWord(codegenProperty.datatypeWithEnum);
      }
    }
    return codegenProperty;
  }

    @SuppressWarnings("static-method")
    public String toSwiftyEnumName(String value) {
        // Prevent from breaking properly cased identifier
        if (value.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            return value;
        }
        char[] separators = {'-', '_', ' '};
        return WordUtils.capitalizeFully(StringUtils.lowerCase(value), separators).replaceAll("[-_ ]", "");
    }


  @Override
  public String toApiName(String name) {
    if(name.length() == 0)
      return "DefaultAPI";
    return initialCaps(name) + "API";
  }

  @Override
  public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
    path = normalizePath(path); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
    List<Parameter> parameters = operation.getParameters();
    parameters = Lists.newArrayList(Iterators.filter(parameters.iterator(), new Predicate<Parameter>() {
      @Override
      public boolean apply(@Nullable Parameter parameter) {
        return !(parameter instanceof HeaderParameter);
      }
    }));
    operation.setParameters(parameters);
    return super.fromOperation(path, httpMethod, operation, definitions, swagger);
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

  public void setProjectName(String projectName) {
    this.projectName = projectName;
  }

  public void setUnwrapRequired(boolean unwrapRequired) {
    this.unwrapRequired = unwrapRequired;
  }

  public void setResponseAs(String[] responseAs) {
    this.responseAs = responseAs;
  }
}
