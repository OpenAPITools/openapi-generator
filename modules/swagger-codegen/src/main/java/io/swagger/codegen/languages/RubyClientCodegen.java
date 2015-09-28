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
import java.util.Arrays;
import java.util.HashSet;

import org.apache.commons.lang.StringUtils;

public class RubyClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String gemName = null;
    protected String moduleName = null;
    protected String gemVersion = "1.0.0";
    protected String libFolder = "lib";

    public RubyClientCodegen() {
        super();
        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separator + "ruby";
        modelTemplateFiles.put("model.mustache", ".rb");
        apiTemplateFiles.put("api.mustache", ".rb");
        templateDir = "ruby";

        typeMapping.clear();
        languageSpecificPrimitives.clear();

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "__FILE__", "and", "def", "end", "in", "or", "self", "unless", "__LINE__",
                        "begin", "defined?", "ensure", "module", "redo", "super", "until", "BEGIN",
                        "break", "do", "false", "next", "rescue", "then", "when", "END", "case",
                        "else", "for", "nil", "retry", "true", "while", "alias", "class", "elsif",
                        "if", "not", "return", "undef", "yield")
        );

        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("array");
        languageSpecificPrimitives.add("map");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("DateTime");

        typeMapping.put("string", "String");
        typeMapping.put("char", "String");
        typeMapping.put("int", "Integer");
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("number", "Float");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("boolean", "BOOLEAN");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("map", "Hash");
        typeMapping.put("object", "Object");
        typeMapping.put("file", "File");

        // remove modelPackage and apiPackage added by default
        cliOptions.clear();
        cliOptions.add(new CliOption("gemName", "gem name (convention: underscore_case), default: swagger_client"));
        cliOptions.add(new CliOption("moduleName", "top module name (convention: CamelCase, usually corresponding to gem name), default: SwaggerClient"));
        cliOptions.add(new CliOption("gemVersion", "gem version, default: 1.0.0"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("gemName")) {
            setGemName((String) additionalProperties.get("gemName"));
        }
        if (additionalProperties.containsKey("moduleName")) {
            setModuleName((String) additionalProperties.get("moduleName"));
        }

        if (gemName == null && moduleName == null) {
            setGemName("swagger_client");
            setModuleName(generateModuleName(gemName));
        } else if (gemName == null) {
            setGemName(generateGemName(moduleName));
        } else if (moduleName == null) {
            setModuleName(generateModuleName(gemName));
        }

        additionalProperties.put("gemName", gemName);
        additionalProperties.put("moduleName", moduleName);

        if (additionalProperties.containsKey("gemVersion")) {
            setGemVersion((String) additionalProperties.get("gemVersion"));
        } else {
            // not set, pass the default value to template
            additionalProperties.put("gemVersion", gemVersion);
        }

        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("gemspec.mustache", "", gemName + ".gemspec"));
        supportingFiles.add(new SupportingFile("gem.mustache", libFolder, gemName + ".rb"));
        String gemFolder = libFolder + File.separator + gemName;
        supportingFiles.add(new SupportingFile("api_client.mustache", gemFolder, "api_client.rb"));
        supportingFiles.add(new SupportingFile("api_error.mustache", gemFolder, "api_error.rb"));
        supportingFiles.add(new SupportingFile("configuration.mustache", gemFolder, "configuration.rb"));
        supportingFiles.add(new SupportingFile("version.mustache", gemFolder, "version.rb"));
        String modelFolder = gemFolder + File.separator + modelPackage.replace("/", File.separator);
        supportingFiles.add(new SupportingFile("base_object.mustache", modelFolder, "base_object.rb"));
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "ruby";
    }

    public String getHelp() {
        return "Generates a Ruby client library.";
    }

    /**
     * Generate Ruby module name from the gem name, e.g. use "SwaggerClient" for "swagger_client".
     */
    public String generateModuleName(String gemName) {
        return camelize(gemName.replaceAll("[^\\w]+", "_"));
    }

    /**
     * Generate Ruby gem name from the module name, e.g. use "swagger_client" for "SwaggerClient".
     */
    public String generateGemName(String moduleName) {
        return underscore(moduleName.replaceAll("[^\\w]+", ""));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + apiPackage.replace("/", File.separator);
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + modelPackage.replace("/", File.separator);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
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
            }
        } else {
            type = swaggerType;
        }
        if (type == null) {
            return null;
        }
        return type;
    }

    public String toDefaultValue(Property p) {
        return "null";
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
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
        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // underscore the model file name
        // PhoneNumber.rb => phone_number.rb
        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return underscore(name) + "_api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
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

        return underscore(sanitizeName(operationId));
    }

    @Override
    public String toModelImport(String name) {
        return gemName + "/" + modelPackage() + "/" + toModelFilename(name);
    }

    @Override
    public String toApiImport(String name) {
        return gemName + "/" + apiPackage() + "/" + toApiFilename(name);
    }

    public void setGemName(String gemName) {
        this.gemName = gemName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setGemVersion(String gemVersion) {
        this.gemVersion = gemVersion;
    }
}
