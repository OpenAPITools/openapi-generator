package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import org.apache.commons.lang3.StringUtils;

public class PhpClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "Swagger\\Client";
    protected String composerVendorName = "swagger";
    protected String composerProjectName = "swagger-client";
    protected String packagePath = "SwaggerClient-php";
    protected String artifactVersion = "1.0.0";
    protected String srcBasePath = "lib";
    protected String variableNamingConvention= "snake_case";

    public PhpClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "php";
        modelTemplateFiles.put("model.mustache", ".php");
        apiTemplateFiles.put("api.mustache", ".php");
        templateDir = "php";
        apiPackage = invokerPackage + "\\Api";
        modelPackage = invokerPackage + "\\Model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
        );

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "object",
                        "DateTime",
                        "mixed",
                        "number",
                        "void",
                        "byte")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("map", "map");


        // provide primitives to mustache template
        String primitives = "'" + StringUtils.join(languageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        // ref: https://github.com/swagger-api/swagger-spec/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<String, String>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "\\DateTime");
        typeMapping.put("datetime", "\\DateTime");
        typeMapping.put("file", "\\SplFileObject");
        typeMapping.put("map", "map");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "object");
        typeMapping.put("DateTime", "\\DateTime");

        cliOptions.add(new CliOption("variableNamingConvention", "naming convention of variable name, e.g. camelCase. Default: snake_case"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, "The main namespace to use for all classes. e.g. Yay\\Pets"));
        cliOptions.add(new CliOption("packagePath", "The main package name for classes. e.g. GeneratedPetstore"));
        cliOptions.add(new CliOption("srcBasePath", "The directory under packagePath to serve as source root."));
        cliOptions.add(new CliOption("composerVendorName", "The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets"));
        cliOptions.add(new CliOption("composerProjectName", "The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client"));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, "The version to use in the composer package version field. e.g. 1.2.3"));
    }

    public String getPackagePath() {
        return packagePath;
    }

    public String toPackagePath(String packageName, String basePath) {
        packageName = packageName.replace(invokerPackage, "");
        if (basePath != null && basePath.length() > 0) {
            basePath = basePath.replaceAll("[\\\\/]?$", "") + File.separatorChar;
        }

        String regFirstPathSeparator;
        if ("/".equals(File.separator)) { // for mac, linux
            regFirstPathSeparator = "^/";
        } else { // for windows
            regFirstPathSeparator = "^\\\\";
        }

        String regLastPathSeparator;
        if ("/".equals(File.separator)) { // for mac, linux
            regLastPathSeparator = "/$";
        } else { // for windows
            regLastPathSeparator = "\\\\$";
        }

        return (getPackagePath() + File.separatorChar + basePath
                    // Replace period, backslash, forward slash with file separator in package name
                    + packageName.replaceAll("[\\.\\\\/]", File.separator)
                    // Trim prefix file separators from package path
                    .replaceAll(regFirstPathSeparator, ""))
                    // Trim trailing file separators from the overall path
                    .replaceAll(regLastPathSeparator+ "$", "");
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "php";
    }

    public String getHelp() {
        return "Generates a PHP client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("packagePath")) {
            this.setPackagePath((String) additionalProperties.get("packagePath"));
        } else {
            additionalProperties.put("packagePath", packagePath);
        }

        if (additionalProperties.containsKey("srcBasePath")) {
            this.setSrcBasePath((String) additionalProperties.get("srcBasePath"));
        } else {
            additionalProperties.put("srcBasePath", srcBasePath);
        }
        
        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        } else {
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }
        
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            this.setModelPackage((String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        } else {
            additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            this.setApiPackage((String) additionalProperties.get(CodegenConstants.API_PACKAGE));
        } else {
            additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
        }
                
        if (additionalProperties.containsKey("composerProjectName")) {
            this.setComposerProjectName((String) additionalProperties.get("composerProjectName"));
        } else {
            additionalProperties.put("composerProjectName", composerProjectName);
        }
        
        if (additionalProperties.containsKey("composerVendorName")) {
            this.setComposerVendorName((String) additionalProperties.get("composerVendorName"));
        } else {
            additionalProperties.put("composerVendorName", composerVendorName);
        }
        
        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }
        
        additionalProperties.put("escapedInvokerPackage", invokerPackage.replace("\\", "\\\\"));

        supportingFiles.add(new SupportingFile("configuration.mustache", toPackagePath(invokerPackage, srcBasePath), "Configuration.php"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", toPackagePath(invokerPackage, srcBasePath), "ApiClient.php"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", toPackagePath(invokerPackage, srcBasePath), "ApiException.php"));
        supportingFiles.add(new SupportingFile("ObjectSerializer.mustache", toPackagePath(invokerPackage, srcBasePath), "ObjectSerializer.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", getPackagePath(), "composer.json"));
        supportingFiles.add(new SupportingFile("autoload.mustache", getPackagePath(), "autoload.php"));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + "/" + toPackagePath(apiPackage(), srcBasePath));
    }

    public String modelFileFolder() {
        return (outputFolder + "/" + toPackagePath(modelPackage(), srcBasePath));
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner) + "[]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "[string," + getTypeDeclaration(inner) + "]";
        } else if (p instanceof RefProperty) {
            String type = super.getTypeDeclaration(p);
            return (!languageSpecificPrimitives.contains(type))
                    ? "\\" + modelPackage + "\\" + type : type;
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (!languageSpecificPrimitives.contains(name)) {
            return "\\" + modelPackage + "\\" + name;
        }
        return super.getTypeDeclaration(name);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            } else if (instantiationTypes.containsKey(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        if (type == null) {
            return null;
        }
        return toModelName(type);
    }

    public String toDefaultValue(Property p) {
        return "null";
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }
        
    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setPackagePath(String packagePath) {
        this.packagePath = packagePath;
    }

    public void setSrcBasePath(String srcBasePath) {
        this.srcBasePath = srcBasePath;
    }
    
    public void setParameterNamingConvention(String variableNamingConvention) {
        this.variableNamingConvention = variableNamingConvention;
    }

    private void setComposerVendorName(String composerVendorName) {
        this.composerVendorName = composerVendorName;
    }
    
    public void setComposerProjectName(String composerProjectName) {
        this.composerProjectName = composerProjectName;
    }

    @Override
    public String toVarName(String name) {
        if (additionalProperties.containsKey("variableNamingConvention")) {
            this.setParameterNamingConvention((String) additionalProperties.get("variableNamingConvention"));
        }

        // sanitize name
        name = sanitizeName(name);

        if ("camelCase".equals(variableNamingConvention)) {
          // return the name in camelCase style
          // phone_number => phoneNumber
          name =  camelize(name, true);
        } else { // default to snake case
          // return the name in underscore style
          // PhoneNumber => phone_number
          name =  underscore(name);
        }

        // parameter name starting with number won't compile
        // need to escape it by appending _ at the beginning
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // Note: backslash ("\\") is allowed for e.g. "\\DateTime"
        name = name.replaceAll("[^\\w\\\\]+", "_");

        // model name cannot use reserved keyword
        if (reservedWords.contains(name)) {
            escapeReservedWord(name); // e.g. return => _return
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(sanitizeName(operationId), true);
    }

}
