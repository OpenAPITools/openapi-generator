package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class PhpClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-client";
    protected String artifactVersion = "1.0.0";
    protected String rootNamespace;
    protected String invokerNamespace;
    protected String modelNamespace;
    protected String apiNamespace;

    public PhpClientCodegen() {
        super();

        rootNamespace = "Swagger";
        invokerPackage = "Client";
        modelPackage = "Models";
        apiPackage = "Api";
        outputFolder = "generated-code/php";
        modelTemplateFiles.put("model.mustache", ".php");
        apiTemplateFiles.put("api.mustache", ".php");
        templateDir = "php";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
        );

        additionalProperties.put("invokerPackage", invokerPackage);
        additionalProperties.put("groupId", groupId);
        additionalProperties.put("artifactId", artifactId);
        additionalProperties.put("artifactVersion", artifactVersion);

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "object",
                        "DateTime",
                        "mixed",
                        "number")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("map", "map");

        // ref: https://github.com/swagger-api/swagger-spec/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<String, String>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("date", "DateTime");
        typeMapping.put("datetime", "DateTime");
        typeMapping.put("file", "string");
        typeMapping.put("map", "map");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "object");

        cliOptions.add(new CliOption("rootNamespace", "root namespace from which other namespaces derive"));
        cliOptions.add(new CliOption("invokerPackage", "namespace for core, non-api-specific classes"));
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

        if (additionalProperties.containsKey("invokerPackage")) {
            this.setInvokerPackage((String) additionalProperties.get("invokerPackage"));
        }

        if (additionalProperties.containsKey("rootNamespace")) {
            this.setRootNamespace((String) additionalProperties.get("rootNamespace"));
        }

        setNamespacesFromPackages();
        prefixPackages();

        supportingFiles.add(new SupportingFile("configuration.mustache", invokerPackage.replace('/', File.separatorChar), "Configuration.php"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerPackage.replace('/', File.separatorChar), "ApiClient.php"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", invokerPackage.replace('/', File.separatorChar), "ApiException.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("autoload.mustache", "", "autoload.php"));
    }

    protected String getSrcDir(String packageName) {
        return rootNamespace + "/src/" + packageName;
    }

    protected void prefixPackages() {
        setApiPackage(getSrcDir(apiPackage));
        setInvokerPackage(getSrcDir(invokerPackage));
        setModelPackage(getSrcDir(modelPackage));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + "/" + apiPackage()).replace('/', File.separatorChar);
    }

    public String modelFileFolder() {
        return (outputFolder + "/" + modelPackage()).replace('/', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "[string," + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
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

    public void setRootNamespace(String rootNamespace) {
        this.rootNamespace = rootNamespace;
    }

    @Override
    public String toVarName(String name) {
        // parameter name starting with number won't compile
        // need to escape it by appending _ at the beginning
        if (name.matches("^[0-9]")) {
            name = "_" + name;
        }

        // return the name in underscore style
        // PhoneNumber => phone_number
        return underscore(name);
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
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

    public String toNamespace(String packageName) {
        return rootNamespace + "\\" + packageName.replace('/', '\\').replace('.', '\\');
    }

    protected void setNamespacesFromPackages() {
        invokerNamespace = toNamespace(invokerPackage);
        apiNamespace = toNamespace(apiPackage);
        modelNamespace = toNamespace(modelPackage);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return addNamespaces(super.postProcessModels(objs));
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        objs = addNamespaces(super.postProcessOperations(objs));
        objs = formatImports(objs, "imports", "import");

        return objs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = addNamespaces(super.postProcessSupportingFileData(objs));
        objs = formatImports(objs, "models", "importPath");

        return objs;
    }

    protected Map<String, Object> addNamespaces(Map<String, Object> objs) {
        objs.put("rootNamespace", rootNamespace);
        objs.put("invokerNamespace", invokerNamespace);
        objs.put("apiNamespace", apiNamespace);
        objs.put("modelNamespace", modelNamespace);

        return objs;
    }

    protected Map<String, Object> formatImports(Map<String, Object> objs, String objsKey, String importKey) {
        if (objs.containsKey(objsKey)) {
            String modelName;
            List<Map<String, Object>> newImportList = new ArrayList<Map<String, Object>>();

            for (Map<String, Object> importMap : (List<Map<String, Object>>) objs.get(objsKey)) {
                modelName = ((String) importMap.get(importKey)).replace(modelPackage + ".", "");

                if (reservedWords.contains(modelName)) {
                    continue;
                }

                importMap.put(importKey, modelNamespace + "\\" + modelName);
                newImportList.add(importMap);
            }

            objs.put(objsKey, newImportList);
        }

        return objs;
    }
}
