package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.codegen.CliOption;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

public class CsharpDotNet2ClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String CLIENT_PACKAGE = "clientPackage";
    protected String packageName = "IO.Swagger";
    protected String packageVersion = "1.0.0";
    protected String clientPackage = "IO.Swagger.Client";
    protected String sourceFolder = "src" + File.separator + "main" + File.separator + "CsharpDotNet2";
    protected String apiDocPath = "docs/"; 
    protected String modelDocPath = "docs/";


    public CsharpDotNet2ClientCodegen() {
        super();

        // clear import mapping (from default generator) as C# (2.0) does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code" + File.separator + "CsharpDotNet2";
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");
        embeddedTemplateDir = templateDir = "CsharpDotNet2";
        apiPackage = "IO.Swagger.Api";
        modelPackage = "IO.Swagger.Model";
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        setReservedWordsLowerCase(
                Arrays.asList(
                    // local variable names in API methods (endpoints)
                    "path", "queryParams", "headerParams", "formParams", "fileParams", "postBody",
                    "authSettings", "response", "StatusCode",
                    // C# reserved word
                    "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while")
        );


        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "string",
                        "bool?",
                        "double?",
                        "int?",
                        "long?",
                        "float?",
                        "byte[]",
                        "List",
                        "Dictionary",
                        "DateTime?",
                        "String",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "Guid?",
                        "System.IO.Stream", // not really a primitive, we include it to avoid model import
                        "Object")
        );
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Dictionary");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("boolean", "bool?");
        typeMapping.put("integer", "int?");
        typeMapping.put("float", "float?");
        typeMapping.put("long", "long?");
        typeMapping.put("double", "double?");
        typeMapping.put("number", "double?");
        typeMapping.put("datetime", "DateTime?");
        typeMapping.put("date", "DateTime?");
        typeMapping.put("file", "System.IO.Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");
        typeMapping.put("uuid", "Guid?");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "C# package name (convention: Camel.Case).")
                .defaultValue("IO.Swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "C# package version.").defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CLIENT_PACKAGE, "C# client package name (convention: Camel.Case).")
                .defaultValue("IO.Swagger.Client"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            apiPackage = packageName + ".Api";
            modelPackage = packageName + ".Model";
            clientPackage = packageName + ".Client";
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CLIENT_PACKAGE)) {
            this.setClientPackage((String) additionalProperties.get(CLIENT_PACKAGE));
        } else {
            additionalProperties.put(CLIENT_PACKAGE, clientPackage);
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("Configuration.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "ApiException.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor", "packages.config"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

    }

    public void setClientPackage(String clientPackage) {
        this.clientPackage = clientPackage;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "CsharpDotNet2";
    }

    @Override
    public String getHelp() {
        return "Generates a C# .Net 2.0 client library.";
    }

    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
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
        if (typeMapping.containsKey(swaggerType.toLowerCase())) {
            type = typeMapping.get(swaggerType.toLowerCase());
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(sanitizeName(operationId));
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

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

}
