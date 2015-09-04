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

public class PythonClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String packageName = null;
    protected String packageVersion = null;

    public PythonClientCodegen() {
        super();

        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separatorChar + "python";
        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");
        templateDir = "python";

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("list");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("str");
        languageSpecificPrimitives.add("datetime");
        languageSpecificPrimitives.add("date");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "int");
        typeMapping.put("double", "float");
        typeMapping.put("array", "list");
        typeMapping.put("map", "dict");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "str");
        typeMapping.put("date", "date");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("object", "object");
        typeMapping.put("file", "file");

        // from https://docs.python.org/release/2.5.4/ref/keywords.html
        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try"));

        cliOptions.clear();
        cliOptions.add(new CliOption("packageName", "python package name (convention: snake_case), default: swagger_client"));
        cliOptions.add(new CliOption("packageVersion", "python package version, default: 1.0.0"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("packageName")) {
            setPackageName((String) additionalProperties.get("packageName"));
        }
        else {
            setPackageName("swagger_client");
        }

        if (additionalProperties.containsKey("packageVersion")) {
            setPackageVersion((String) additionalProperties.get("packageVersion"));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put("packageName", packageName);
        additionalProperties.put("packageVersion", packageVersion);

        String swaggerFolder = packageName;

        modelPackage = swaggerFolder + File.separatorChar + "models";
        apiPackage = swaggerFolder + File.separatorChar + "apis";

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
        supportingFiles.add(new SupportingFile("api_client.mustache", swaggerFolder, "api_client.py"));
        supportingFiles.add(new SupportingFile("rest.mustache", swaggerFolder, "rest.py"));
        supportingFiles.add(new SupportingFile("configuration.mustache", swaggerFolder, "configuration.py"));
        supportingFiles.add(new SupportingFile("__init__package.mustache", swaggerFolder, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", modelPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__api.mustache", apiPackage, "__init__.py"));
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "python";
    }

    public String getHelp() {
        return "Generates a Python client library.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage().replace('.', File.separatorChar);
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

            return getSwaggerType(p) + "(str, " + getTypeDeclaration(inner) + ")";
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
            type = toModelName(swaggerType);
        }
        return type;
    }

    public String toDefaultValue(Property p) {
        return "None";
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // underscore the variable name
        // petId => pet_id
        name = underscore(name);

        // remove leading underscore
        name = name.replaceAll("^_*", "");

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
        // PhoneNumber => phone_number
        return underscore(dropDots(name));
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
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_api";
        }
        return underscore(name) + "_api";
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

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    /**
     * Generate Python package name from String `packageName`
     *
     * (PEP 0008) Python packages should also have short, all-lowercase names, 
     * although the use of underscores is discouraged.
     */
    public String generatePackageName(String packageName) {
        return underscore(packageName.replaceAll("[^\\w]+", "")); 
    }
}


