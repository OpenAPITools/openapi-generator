/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.base.Strings;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(RustClientCodegen.class);
    private boolean useSingleRequestParameter = false;
    private boolean supportAsync = true;
    private boolean supportMultipleResponses = false;

    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";
    public static final String HYPER_LIBRARY = "hyper";
    public static final String REQWEST_LIBRARY = "reqwest";
    public static final String SUPPORT_ASYNC = "supportAsync";
    public static final String SUPPORT_MULTIPLE_RESPONSES = "supportMultipleResponses";

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiFolder = "src/apis";
    protected String modelFolder = "src/models";
    protected String enumSuffix = ""; // default to empty string for backward compatibility

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

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/rust";
        modelTemplateFiles.put("model.mustache", ".rs");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

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
                        "while", "yield", "async", "await", "dyn", "try"
                )
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "i8", "i16", "i32", "i64",
                        "u8", "u16", "u32", "u64",
                        "f32", "f64", "isize", "usize",
                        "char", "bool", "str", "String")
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
        typeMapping.put("URI", "String");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "String");
        typeMapping.put("password", "String");
        // TODO(bcourtine): review file mapping.
        // I tried to map as "std::io::File", but Reqwest multipart file requires a "AsRef<Path>" param.
        // Getting a file from a Path is simple, but the opposite is difficult. So I map as "std::path::Path".
        typeMapping.put("file", "std::path::PathBuf");
        typeMapping.put("binary", "crate::models::File");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("object", "serde_json::Value");
        typeMapping.put("AnyType", "serde_json::Value");

        // no need for rust
        //importMapping = new HashMap<String, String>();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Rust package name (convention: lowercase).")
                .defaultValue("openapi"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Rust package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, CodegenConstants.USE_SINGLE_REQUEST_PARAMETER_DESC, SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(SUPPORT_ASYNC, "If set, generate async function call instead. This option is for 'reqwest' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(SUPPORT_MULTIPLE_RESPONSES, "If set, return type wraps an enum of all possible 2xx schemas. This option is for 'reqwest' library only", SchemaTypeUtil.BOOLEAN_TYPE)
            .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.ENUM_NAME_SUFFIX, CodegenConstants.ENUM_NAME_SUFFIX_DESC).defaultValue(this.enumSuffix));

        supportedLibraries.put(HYPER_LIBRARY, "HTTP client: Hyper.");
        supportedLibraries.put(REQWEST_LIBRARY, "HTTP client: Reqwest.");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use.");
        libraryOption.setEnum(supportedLibraries);
        // set reqwest as the default
        libraryOption.setDefault(REQWEST_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(REQWEST_LIBRARY);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = new HashMap<>();

        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            String modelName = toModelName(entry.getKey());
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                allModels.put(modelName, cm);
            }
        }

        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.discriminator != null) {
                    List<Object> discriminatorVars = new ArrayList<>();
                    for (CodegenDiscriminator.MappedModel mappedModel : cm.discriminator.getMappedModels()) {
                        CodegenModel model = allModels.get(mappedModel.getModelName());
                        Map<String, Object> mas = new HashMap<>();
                        mas.put("modelName", camelize(mappedModel.getModelName()));
                        mas.put("mappingName", mappedModel.getMappingName());

                        // TODO: deleting the variable from the array was
                        // problematic; I don't know what this is supposed to do
                        // so I'm just cloning it for the moment
                        List<CodegenProperty> vars = new ArrayList<>(model.getVars());
                        vars.removeIf(p -> p.name.equals(cm.discriminator.getPropertyName()));
                        mas.put("vars", vars);
                        discriminatorVars.add(mas);
                    }
                    // TODO: figure out how to properly have the original property type that didn't go through toVarName
                    String vendorExtensionTagName = cm.discriminator.getPropertyName().replace("_", "");
                    cm.vendorExtensions.put("x-tag-name", vendorExtensionTagName);
                    cm.vendorExtensions.put("x-mapped-models", discriminatorVars);
                }
            }
        }
        return objs;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.ENUM_NAME_SUFFIX)) {
            enumSuffix = additionalProperties.get(CodegenConstants.ENUM_NAME_SUFFIX).toString();
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        // If no version is provided in additional properties, version from API specification is used.
        // If none of them is provided then fallback to default version
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else if (openAPI != null && openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
            setPackageVersion(openAPI.getInfo().getVersion());
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER)) {
            this.setUseSingleRequestParameter(convertPropertyToBoolean(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER));
        }
        writePropertyBack(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, getUseSingleRequestParameter());

        if (additionalProperties.containsKey(SUPPORT_ASYNC)) {
            this.setSupportAsync(convertPropertyToBoolean(SUPPORT_ASYNC));
        }
        writePropertyBack(SUPPORT_ASYNC, getSupportAsync());

        if (additionalProperties.containsKey(SUPPORT_MULTIPLE_RESPONSES)) {
            this.setSupportMultipleReturns(convertPropertyToBoolean(SUPPORT_MULTIPLE_RESPONSES));
        }
        writePropertyBack(SUPPORT_MULTIPLE_RESPONSES, getSupportMultipleReturns());

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (HYPER_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(HYPER_LIBRARY, "true");
        } else if (REQWEST_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(REQWEST_LIBRARY, "true");
        } else {
            LOGGER.error("Unknown library option (-l/--library): {}", getLibrary());
        }

        apiTemplateFiles.put(getLibrary() + "/api.mustache", ".rs");

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("model_mod.mustache", modelFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));

        supportingFiles.add(new SupportingFile(getLibrary() + "/api_mod.mustache", apiFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile(getLibrary() + "/configuration.mustache", apiFolder, "configuration.rs"));
        if (HYPER_LIBRARY.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("request.rs", apiFolder, "request.rs"));
            supportingFiles.add(new SupportingFile(getLibrary() + "/client.mustache", apiFolder, "client.rs"));
        }
    }

    private boolean getSupportAsync() {
        return supportAsync;
    }

    private void setSupportAsync(boolean supportAsync) {
        this.supportAsync = supportAsync;
    }

    public boolean getSupportMultipleReturns() {
        return supportMultipleResponses;
    }

    public void setSupportMultipleReturns(boolean supportMultipleResponses) {
        this.supportMultipleResponses = supportMultipleResponses;
    }

    private boolean getUseSingleRequestParameter() {
        return useSingleRequestParameter;
    }

    private void setUseSingleRequestParameter(boolean useSingleRequestParameter) {
        this.useSingleRequestParameter = useSingleRequestParameter;
    }

    @Override
    public String escapeReservedWord(String name) {
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
        // translate @ for properties (like @type) to at_. 
        // Otherwise an additional "type" property will leed to duplcates
        name = name.replaceAll("^@", "at_");

        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all upper case, do nothing
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

        if (!Strings.isNullOrEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!Strings.isNullOrEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, "model_" + name);
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    "model_" + name);
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
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                LOGGER.warn("{}(array property) does not have a proper inner type defined.Default to string",
                        ap.getName());
                inner = new StringSchema().description("TODO default missing array inner type to string");
            }
            return "Vec<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            if (inner == null) {
                LOGGER.warn("{}(map property) does not have a proper inner type defined. Default to string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
            }
            return "::std::collections::HashMap<String, " + getTypeDeclaration(inner) + ">";
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (typeMapping.containsValue(schemaType)) {
            return schemaType;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }

        // return fully-qualified model name
        // crate::models::{{classnameFile}}::{{classname}}
        return "crate::models::" + toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }
        return schemaType;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, StringUtils.underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return StringUtils.underscore(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion, depending on client library (e.g. Hyper: PUT => Put, Reqwest: PUT => put)
            if (HYPER_LIBRARY.equals(getLibrary())) {
                operation.httpMethod = StringUtils.camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
            } else if (REQWEST_LIBRARY.equals(getLibrary())) {
                operation.httpMethod = operation.httpMethod.toUpperCase(Locale.ROOT);
            }

            // add support for single request parameter using x-group-parameters
            if (!operation.vendorExtensions.containsKey("x-group-parameters") && useSingleRequestParameter) {
                operation.vendorExtensions.put("x-group-parameters", Boolean.TRUE);
            }

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
                if (p.isArray && !languageSpecificPrimitives.contains(p.dataType)) {
                    // array of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "Vec<" + rt.substring("Vec<".length(), end).trim() + ">";
                    }
                } else if (p.isMap && !languageSpecificPrimitives.contains(p.dataType)) {
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
            return "Empty";
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "Minus");
            varName = varName.replaceAll("\\+", "Plus");
            varName = varName.replaceAll("\\.", "Dot");
            return varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name);
        }

        // string
        String enumName = camelize(sanitizeName(name));
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
        String name = property.name;
        if (!org.apache.commons.lang3.StringUtils.isEmpty(enumSuffix)) {
            name = name + "_" + enumSuffix;
        }
        // camelize the enum name
        // phone_number => PhoneNumber
        String enumName = camelize(toModelName(name));

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() != null) {
            return p.getDefault().toString();
        } else {
            return null;
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.RUST; }
}
