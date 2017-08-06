package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RustClientCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(RustClientCodegen.class);
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";

    protected String packageName = "swagger";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiFolder = "src/apis";
    protected String modelFolder= "src/models";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "rust";
    }

    public String getHelp() {
        return "Generates a Rust client library (beta).";
    }

    public RustClientCodegen() {
        super();
        outputFolder = "generated-code/rust";
        modelTemplateFiles.put("model.mustache", ".rs");
        apiTemplateFiles.put("api.mustache", ".rs");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        embeddedTemplateDir = templateDir = "rust";

        setReservedWordsLowerCase(
            Arrays.asList(
                "abstract", "alignof", "as", "become", "box",
                "break", "const", "continue", "crate", "do",
                "else", "enum", "extern", "false", "final",
                "fn", "for", "if", "impl", "in",
                "let", "loop", "macro", "match", "mod",
                "move", "mut", "offsetof", "override", "priv",
                "proc", "pub", "pure", "ref", "return",
                "Self", "self", "sizeof", "static", "struct",
                "super", "trait", "true", "type", "typeof",
                "unsafe", "unsized", "use", "virtual", "where",
                "while", "yield"
            )
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                    "map",
                    "array")
                );

        languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList(
                "i8", "i16", "i32", "i64",
                "u8", "u16", "u32", "u64",
                "f32", "f64",
                "char", "bool", "String", "Vec<u8>", "File")
            );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("integer", "i32");
        typeMapping.put("long", "i64");
        typeMapping.put("number", "f32");
        typeMapping.put("float", "f32");
        typeMapping.put("double", "f64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("UUID", "String");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "String");
        typeMapping.put("password", "String");
        // TODO(farcaller): map file
        typeMapping.put("file", "File");
        typeMapping.put("binary", "Vec<u8>");
        typeMapping.put("ByteArray", "String");
        // TODO  what should 'object' mapped to
        typeMapping.put("object", "Object");

        // no need for rust
        //importMapping = new HashMap<String, String>();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Rust package name (convention: lowercase).")
                .defaultValue("swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Rust package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "hides the timestamp when files were generated")
                .defaultValue(Boolean.TRUE.toString()));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        // default HIDE_GENERATION_TIMESTAMP to true
        if (!additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, Boolean.TRUE.toString());
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP,
                    Boolean.valueOf(additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("configuration.mustache", apiFolder, "configuration.rs"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));

        supportingFiles.add(new SupportingFile("client.mustache", apiFolder, "client.rs"));
        supportingFiles.add(new SupportingFile("api_mod.mustache", apiFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile("model_mod.mustache", modelFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile("lib.rs", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));
    }

    @Override
    public String escapeReservedWord(String name)
    {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return '_' + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + apiFolder).replace("/", File.separator);
    }

    public String modelFileFolder() {
        return (outputFolder + File.separator + modelFolder).replace("/", File.separator);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // snake_case, e.g. PetId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = "var_" + name;

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(toModelFilename(name));
    }

    @Override
    public String toModelFilename(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.rs => pet_api.rs
        return underscore(name) + "_api";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "Vec<" + getTypeDeclaration(inner) + ">";
        }
        else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "::std::collections::HashMap<String, " + getTypeDeclaration(inner) + ">";
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }

        if (typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }

        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }

        // return fully-qualified model name
        // ::models::{{classnameFile}}::{{classname}}
        return "::models::" + toModelName(swaggerType);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if(typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if(languageSpecificPrimitives.contains(type))
                return (type);
        }
        else
            type = swaggerType;
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return camelize(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase());
            // update return type to conform to rust standard
            /*
            if (operation.returnType != null) {
                if ( operation.returnType.startsWith("Vec") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    // array of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "Vec<super::" + rt.substring("Vec<".length(), end).trim() + ">");
                        operation.returnContainer = "List";
                    }
                } else if (operation.returnType.startsWith("::std::collections::HashMap<String, ") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    LOGGER.info("return base type:" + operation.returnBaseType);
                    // map of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">");
                        operation.returnContainer = "Map";
                    }
                } else if (!languageSpecificPrimitives.contains(operation.returnType)) {
                    // add super:: to model, e.g. super::pet
                    operation.vendorExtensions.put("x-returnTypeInMethod", "super::" + operation.returnType);
                } else {
                    // primitive type or array/map of primitive type
                    operation.vendorExtensions.put("x-returnTypeInMethod", operation.returnType);
                }
            }

            for (CodegenParameter p : operation.allParams) {
                if (p.isListContainer && !languageSpecificPrimitives.contains(p.dataType)) {
                    // array of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "Vec<" + rt.substring("Vec<".length(), end).trim() + ">";
                    }
                } else if (p.isMapContainer && !languageSpecificPrimitives.contains(p.dataType)) {
                    // map of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">";
                    }
                } else if (!languageSpecificPrimitives.contains(p.dataType)) {
                    // add super:: to model, e.g. super::pet
                    p.dataType = "super::" + p.dataType;
                }
            }*/
        }

        return objs;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
            && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
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
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return escapeText(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name).toUpperCase();
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase());
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName) || enumName.matches("\\d.*")) { // reserved word or starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = underscore(toModelName(property.name)).toUpperCase();

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }
}
