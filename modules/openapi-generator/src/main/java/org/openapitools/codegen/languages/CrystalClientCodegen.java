/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.media.Schema;
import lombok.Setter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.PrefixWithHashLambda;
import org.openapitools.codegen.templating.mustache.PascalCaseLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

/**
 * <p>Mustache templates are located in {@code src/main/resources/crystal/}.
 */
public class CrystalClientCodegen extends DefaultCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CrystalClientCodegen.class);
    private static final String NUMERIC_ENUM_PREFIX = "N";
    protected static int emptyMethodNameCounter = 0;

    private java.util.Set<String> resourceSegments = java.util.Collections.emptySet();
    private String apiBasePrefix = "";

    @Setter protected String shardName = "openapi_client";
    @Setter protected String moduleName = "OpenAPIClient";
    @Setter protected String shardVersion = "1.0.0";
    protected String specFolder = "spec";
    protected String srcFolder = "src";
    @Setter protected String shardLicense = "unlicense";
    @Setter protected String shardHomepage = "https://openapitools.org";
    @Setter protected String shardSummary = "A Crystal SDK for the REST API";
    @Setter protected String shardDescription = "This shard maps to a REST API";
    @Setter protected String shardAuthor = "";
    @Setter protected String shardAuthorEmail = "";
    @Setter protected String paramsEncoder = "Crest::NestedParamsEncoder";
    @Setter protected String apiNamespace = "Api";
    protected List<String> primitiveTypes = new ArrayList<String>();

    public static final String SHARD_NAME = "shardName";
    public static final String MODULE_NAME = "moduleName";
    public static final String SHARD_VERSION = "shardVersion";
    public static final String SHARD_LICENSE = "shardLicense";
    public static final String SHARD_HOMEPAGE = "shardHomepage";
    public static final String SHARD_SUMMARY = "shardSummary";
    public static final String SHARD_DESCRIPTION = "shardDescription";
    public static final String SHARD_AUTHOR = "shardAuthor";
    public static final String SHARD_AUTHOR_EMAIL = "shardAuthorEmail";
    public static final String PARAMS_ENCODER = "paramsEncoder";
    public static final String API_NAMESPACE = "apiNamespace";

    // Generated infrastructure classes that live at the moduleName root; a model named like one
    // of these would clash, so toModelName renames it.
    private static final Set<String> RESERVED_MODEL_NAMES = new HashSet<>(Arrays.asList(
        "Client", "Connection", "Configuration", "Response", "ApiError", "Serializable", "Validation"));

    public CrystalClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.ParameterizedServer,
                        GlobalFeature.MultiServer
                )
                .includeSchemaSupportFeatures(
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

        supportsInheritance = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.BETA).build();

        // clear import mapping (from default generator) as crystal does not use it
        // at the moment
        importMapping.clear();

        embeddedTemplateDir = templateDir = "crystal";
        outputFolder = "generated-code" + File.separator + "crystal";

        modelPackage = "models";
        apiPackage = "api";
        modelTemplateFiles.put("model.mustache", ".cr");
        apiTemplateFiles.put("api.mustache", ".cr");

        modelTestTemplateFiles.put("model_test.mustache", ".cr");
        apiTestTemplateFiles.put("api_test.mustache", ".cr");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // reserved word. Ref:
        // https://crystal-lang.org/reference/1.18/crystal_for_rubyists/index.html#available-keywords
        // https://crystal-lang.org/api/1.18.2/Reference.html
        reservedWords = new HashSet<>(
                Arrays.asList(
                    // language reserved words (keywords)
                    "abstract",   "do",       "if",                "nil?",            "return",      "uninitialized",
                    "alias",      "else",     "in",                "of",              "select",      "union",
                    "as",         "elsif",    "include",           "out",             "self",        "unless",
                    "as?",        "end",      "instance_sizeof",   "pointerof",       "sizeof",      "until",
                    "asm",        "ensure",   "is_a?",             "previous_def",    "struct",      "verbatim",
                    "begin",      "enum",     "lib",               "private",         "super",       "when",
                    "break",      "extend",   "macro",             "protected",       "then",        "while",
                    "case",       "false",    "module",            "require",         "true",        "with",
                    "class",      "for",      "next",              "rescue",          "type",        "yield",
                    "def",        "fun",      "nil",               "responds_to?",    "typeof",
                    // additional reserved words (methods)
                    "annotation", "object_id"
                ));

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("String");
        languageSpecificPrimitives.add("Boolean");
        languageSpecificPrimitives.add("Integer");
        languageSpecificPrimitives.add("Float");
        languageSpecificPrimitives.add("Date");
        languageSpecificPrimitives.add("Time");
        languageSpecificPrimitives.add("Array");
        languageSpecificPrimitives.add("Hash");
        languageSpecificPrimitives.add("::File");
        languageSpecificPrimitives.add("Object");

        typeMapping.clear();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("char", "Char");
        typeMapping.put("int", "Int32");
        typeMapping.put("integer", "Int32");
        typeMapping.put("long", "Int64");
        typeMapping.put("short", "Int32");
        typeMapping.put("float", "Float32");
        typeMapping.put("double", "Float64");
        typeMapping.put("number", "Float64");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("date", "Time");
        typeMapping.put("DateTime", "Time");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("set", "Set");
        typeMapping.put("map", "Hash");
        typeMapping.put("object", "JSON::Any");
        typeMapping.put("AnyType", "JSON::Any");
        typeMapping.put("file", "::File");
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");

        instantiationTypes.put("map", "Hash");
        instantiationTypes.put("array", "Array");
        instantiationTypes.put("set", "Set");
        primitiveTypes = new ArrayList<String>(typeMapping.values());

        // remove modelPackage and apiPackage added by default
        cliOptions.removeIf(opt -> CodegenConstants.MODEL_PACKAGE.equals(opt.getOpt()) || CodegenConstants.API_PACKAGE.equals(opt.getOpt()));

        cliOptions.add(new CliOption(SHARD_NAME, "shard name (e.g. twitter_client").defaultValue("openapi_client"));
        cliOptions.add(new CliOption(MODULE_NAME, "module name (e.g. TwitterClient").defaultValue("OpenAPIClient"));
        cliOptions.add(new CliOption(SHARD_VERSION, "shard version.").defaultValue("1.0.0"));
        cliOptions.add(new CliOption(SHARD_LICENSE, "shard license.").defaultValue("unlicense"));
        cliOptions.add(new CliOption(SHARD_HOMEPAGE, "shard homepage.").defaultValue("http://org.openapitools"));
        cliOptions.add(new CliOption(SHARD_DESCRIPTION, "shard description.").defaultValue("This shard maps to a REST API"));
        cliOptions.add(new CliOption(SHARD_AUTHOR, "shard author (only one is supported)."));
        cliOptions.add(new CliOption(SHARD_AUTHOR_EMAIL, "shard author email (only one is supported)."));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(PARAMS_ENCODER, "params_encoder setting (e.g. Crest::NestedParamsEncoder, Crest::EnumeratedFlatParamsEncoder, Crest::ZeroEnumeratedFlatParamsEncoder").defaultValue("Crest::NestedParamsEncoder"));
        cliOptions.add(new CliOption(API_NAMESPACE, "sub-namespace the api resource classes are nested under, to separate them from same-named models (e.g. \"Api\" -> Foo::Api::Pet). Set to an empty string to nest the api classes directly under moduleName (e.g. when moduleName already ends with \"Api\").").defaultValue("Api"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("CRYSTAL_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable 'CRYSTAL_POST_PROCESS_FILE' (optional) not defined. E.g. to format the source code, please try 'export CRYSTAL_POST_PROCESS_FILE=\"/usr/local/bin/crystal tool format\"' (Linux/Mac)");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'CRYSTAL_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(SHARD_NAME)) {
            setShardName((String) additionalProperties.get(SHARD_NAME));
        } else {
            additionalProperties.put(SHARD_NAME, shardName);
        }

        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName((String) additionalProperties.get(MODULE_NAME));
        } else {
            additionalProperties.put(MODULE_NAME, moduleName);
        }

        if (additionalProperties.containsKey(SHARD_VERSION)) {
            setShardVersion((String) additionalProperties.get(SHARD_VERSION));
        } else {
            additionalProperties.put(SHARD_VERSION, shardVersion);
        }

        if (additionalProperties.containsKey(SHARD_LICENSE)) {
            setShardLicense((String) additionalProperties.get(SHARD_LICENSE));
        } else {
            additionalProperties.put(SHARD_LICENSE, shardLicense);
        }

        if (additionalProperties.containsKey(SHARD_HOMEPAGE)) {
            setShardHomepage((String) additionalProperties.get(SHARD_HOMEPAGE));
        } else {
            additionalProperties.put(SHARD_HOMEPAGE, shardHomepage);
        }

        if (additionalProperties.containsKey(SHARD_SUMMARY)) {
            setShardSummary((String) additionalProperties.get(SHARD_SUMMARY));
        } else {
            additionalProperties.put(SHARD_SUMMARY, shardSummary);
        }

        if (additionalProperties.containsKey(SHARD_DESCRIPTION)) {
            setShardDescription((String) additionalProperties.get(SHARD_DESCRIPTION));
        } else {
            additionalProperties.put(SHARD_DESCRIPTION, shardDescription);
        }

        if (additionalProperties.containsKey(SHARD_AUTHOR)) {
            setShardAuthor((String) additionalProperties.get(SHARD_AUTHOR));
        } else {
            additionalProperties.put(SHARD_AUTHOR, shardAuthor);
        }

        if (additionalProperties.containsKey(SHARD_AUTHOR_EMAIL)) {
            setShardAuthorEmail((String) additionalProperties.get(SHARD_AUTHOR_EMAIL));
        } else {
            additionalProperties.put(SHARD_AUTHOR_EMAIL, shardAuthorEmail);
        }

        if (additionalProperties.containsKey(PARAMS_ENCODER)) {
            setParamsEncoder((String) additionalProperties.get(PARAMS_ENCODER));
        } else {
            additionalProperties.put(PARAMS_ENCODER, paramsEncoder);
        }

        // apiNamespace: the sub-namespace api classes are nested under (default "Api").
        // An empty value nests api classes directly under moduleName (no extra namespace).
        if (additionalProperties.containsKey(API_NAMESPACE)) {
            setApiNamespace((String) additionalProperties.get(API_NAMESPACE));
        }
        additionalProperties.put(API_NAMESPACE, apiNamespace);
        // explicit boolean: jmustache treats an empty string section as truthy, so guard the
        // api namespace `module` wrapper on this instead of on {{#apiNamespace}}.
        additionalProperties.put("apiNamespacePresent", apiNamespace != null && !apiNamespace.isEmpty());

        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("shard_name.mustache", srcFolder, shardName + ".cr"));
        String shardFolder = srcFolder + File.separator + shardName;
        supportingFiles.add(new SupportingFile("api_error.mustache", shardFolder, "api_error.cr"));
        supportingFiles.add(new SupportingFile("configuration.mustache", shardFolder, "configuration.cr"));
        supportingFiles.add(new SupportingFile("response.mustache", shardFolder, "response.cr"));
        supportingFiles.add(new SupportingFile("connection.mustache", shardFolder, "connection.cr"));
        supportingFiles.add(new SupportingFile("client.mustache", shardFolder, "client.cr"));
        supportingFiles.add(new SupportingFile("serializable.mustache", shardFolder, "serializable.cr"));
        supportingFiles.add(new SupportingFile("validation.mustache", shardFolder, "validation.cr"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("shard.mustache", "", "shard.yml"));

        // crystal spec files
        supportingFiles.add(new SupportingFile("spec_helper.mustache", specFolder, "spec_helper.cr").doNotOverwrite());

        // add lambda for mustache templates
        additionalProperties.put("lambdaPrefixWithHash", new PrefixWithHashLambda());
        additionalProperties.put("lambdaPascalcase", new PascalCaseLambda());
    }

    @Override
    public void preprocessOpenAPI(io.swagger.v3.oas.models.OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        java.util.Set<String> paths = openAPI.getPaths() == null
            ? java.util.Collections.emptySet() : openAPI.getPaths().keySet();
        this.resourceSegments = org.openapitools.codegen.languages.crystal.CrystalApiRouting.resourceSegments(paths);
        this.apiBasePrefix = additionalProperties.containsKey("apiBasePath")
            ? stripSlashes((String) additionalProperties.get("apiBasePath"))
            : org.openapitools.codegen.languages.crystal.CrystalApiRouting.commonBasePrefix(paths);
    }

    private static String stripSlashes(String s) {
        return s.replaceAll("^/+", "").replaceAll("/+$", "");
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, io.swagger.v3.oas.models.Operation operation,
                                    CodegenOperation co, java.util.Map<String, java.util.List<CodegenOperation>> operations) {
        org.openapitools.codegen.languages.crystal.CrystalApiRouting.Route r =
            org.openapitools.codegen.languages.crystal.CrystalApiRouting.route(
                co.path, co.httpMethod, co.operationId, resourceSegments, apiBasePrefix);
        String groupKey = r.resource == null ? r.namespace : r.namespace + "/" + r.resource;
        co.operationId = toOperationId(r.action);
        co.vendorExtensions.put("x-cr-namespace", r.namespace);
        if (r.resource != null) co.vendorExtensions.put("x-cr-resource", r.resource);

        java.util.List<CodegenOperation> opList = operations.computeIfAbsent(groupKey, k -> new java.util.ArrayList<>());
        // operationId uniqueness within the group (verb suffix on collision);
        // re-scan after each mutation so 2+ prior collisions are handled correctly.
        String unique = co.operationId;
        int counter = 0;
        boolean clash = uniqueClash(opList, unique);
        while (clash) {
            unique = co.operationId + "_" + co.httpMethod.toLowerCase(java.util.Locale.ROOT)
                     + (counter == 0 ? "" : "_" + counter);
            counter++;
            clash = uniqueClash(opList, unique);
        }
        if (!unique.equals(co.operationId)) {
            LOGGER.warn("Crystal: action collision, renamed `{}` -> `{}`", co.operationId, unique);
            co.operationId = unique;
        }
        co.operationIdSnakeCase = underscore(co.operationId);
        co.operationIdCamelCase = camelize(co.operationId);
        opList.add(co);
        co.baseName = groupKey;
    }

    /** Returns true if any operation in {@code opList} already uses {@code candidate} as its operationId. */
    private static boolean uniqueClash(java.util.List<CodegenOperation> opList, String candidate) {
        for (CodegenOperation op : opList) {
            if (op.operationId.equals(candidate)) return true;
        }
        return false;
    }

    @Override
    public String getHelp() {
        return "Generates a Crystal client library (beta).";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "crystal";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + srcFolder + File.separator + shardName + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + srcFolder + File.separator + shardName + File.separator + modelPackage.replace("/", File.separator);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + specFolder + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + specFolder + File.separator + modelPackage.replace("/", File.separator);
    }

    @Override
    public String getSchemaType(Schema schema) {
        String openAPIType = super.getSchemaType(schema);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        if (type == null) {
            return null;
        }

        return toModelName(type);
    }

    @Override
    public String toModelImport(String name) {
        if (primitiveTypes.contains(name)) {
            return null;
        } else {
            return toModelFilename(name);
        }
    }

    @Override
    public String toModelName(final String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return modelNameMapping.get(name);
        }

        String modelName;
        modelName = sanitizeModelName(name);

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            modelName = modelNamePrefix + "_" + modelName;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            modelName = modelName + "_" + modelNameSuffix;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(modelName)) {
            modelName = camelize("Model" + modelName);
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, modelName);
            return modelName;
        }

        // model name cannot clash with a generated infrastructure class that lives at the
        // moduleName root (Client, Connection, ...). Rename it (e.g. "Client" -> ModelClient).
        if (RESERVED_MODEL_NAMES.contains(modelName)) {
            String renamed = camelize("Model" + modelName);
            LOGGER.warn("{} clashes with a generated class and cannot be used as model name. Renamed to {}", name, renamed);
            return renamed;
        }

        // model name starts with number
        if (modelName.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", modelName, camelize("model_" + modelName));
            // e.g. 200Response => Model200Response (after camelize)
            modelName = "model_" + modelName;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(modelName);
    }

    public String sanitizeModelName(String modelName) {
        String[] parts = modelName.split("::");
        ArrayList<String> new_parts = new ArrayList<String>();
        for (String part : parts) {
            new_parts.add(sanitizeName(part, "\\W", new ArrayList<>(List.of(":"))));
        }
        return String.join("::", new_parts);
    }

    @Override
    public String toModelFilename(String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return underscore(modelNameMapping.get(name));
        }

        return underscore(toModelName(name));
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(final String name) {
        // If the name contains '/', it's a namespaced group key (e.g. "dcim/cable-terminations")
        // produced by addOperationToGroup — sanitize, convert each segment to underscore and join with File.separator.
        if (name.contains("/")) {
            String[] parts = name.split("/");
            java.util.List<String> us = new java.util.ArrayList<>();
            for (String p : parts) us.add(underscore(sanitizeName(p.replace('-', '_'))));
            return String.join(File.separator, us);
        }
        // Single-segment group key or legacy name: sanitize and underscore it directly.
        return underscore(sanitizeName(name.replace('-', '_')));
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "_spec";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "_spec";
    }

    @Override
    public String toApiName(String name) {
        // Group key format: "namespace" or "namespace/resource" (may contain hyphens or other non-word chars)
        // Sanitize each segment to produce valid Crystal identifiers.
        String[] parts = name.split("/");
        StringBuilder cls = new StringBuilder();
        if (apiNamespace != null && !apiNamespace.isEmpty()) cls.append(apiNamespace);
        for (String p : parts) {
            if (cls.length() > 0) cls.append("::");
            cls.append(camelize(sanitizeName(p.replace('-', '_'))));
        }
        return cls.toString();   // e.g. Api::Dcim::CableTerminations (or Dcim::CableTerminations when apiNamespace is empty)
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("Integer".equals(datatype) || "Float".equals(datatype)
                || "Int32".equals(datatype) || "Int64".equals(datatype)
                || "Float32".equals(datatype) || "Float64".equals(datatype)
                || "BigDecimal".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if ("Integer".equals(datatype) || "Float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return NUMERIC_ENUM_PREFIX + varName;
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return NUMERIC_ENUM_PREFIX + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = underscore(toModelName(property.name)).toUpperCase(Locale.ROOT);
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return NUMERIC_ENUM_PREFIX + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // flag models that hold a non-JSON-serializable property (e.g. ::File), so the
        // generated model spec uses a lighter assertion instead of a from_json round-trip
        // (JSON::Serializable cannot deserialise a ::File field, which fails to compile).
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            boolean notJsonSerializable = false;
            for (CodegenProperty p : cm.getAllVars()) {
                if (p.dataType != null && p.dataType.contains("::File")) {
                    notJsonSerializable = true;
                    break;
                }
            }
            if (notJsonSerializable) {
                cm.vendorExtensions.put("x-cr-not-json-serializable", Boolean.TRUE);
            }
        }
        // process enum in models (sets isEnum flags on properties)
        ModelsMap processed = postProcessModelsEnum(objs);
        // flag each property that needs the unified validates macro (must run AFTER postProcessModelsEnum)
        for (ModelMap mo : processed.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty p : cm.vars) {
                if (p.isEnum || p.hasValidation) {
                    p.vendorExtensions.put("x-cr-validated", Boolean.TRUE);
                }
            }

            // Scalar default values for optional properties: emit the spec default instead of nil.
            // Restricted to plain scalars (string/number/bool) excluding date/time, whose default
            // rendering isn't guaranteed to be valid Crystal.
            for (CodegenProperty p : cm.optionalVars) {
                // Only emit plain scalar literals. Skip: containers; date/time (rendering not
                // guaranteed valid Crystal); and any default rendered as a constant reference
                // (contains "::", e.g. a referenced enum's `EnumName::CONST` — named enums are
                // plain aliases here so that constant doesn't exist).
                if (p.defaultValue != null && !p.isContainer && !p.isDate && !p.isDateTime
                        && !p.isEnum && !p.defaultValue.contains("::")) {
                    p.vendorExtensions.put("x-cr-default", p.defaultValue);
                }
            }

            // Models that allow additional properties (additionalProperties: true or a schema)
            // capture + round-trip unknown keys via JSON::Serializable::Unmapped instead of
            // silently dropping them.
            if (cm.getAdditionalPropertiesType() != null) {
                cm.vendorExtensions.put("x-cr-additional-properties", Boolean.TRUE);
            }

            // Polymorphic deserialization: when a model declares a discriminator with a concrete
            // mapping, emit Crystal's use_json_discriminator/use_yaml_discriminator so deserialising
            // the base type dispatches to the mapped subtype.
            org.openapitools.codegen.CodegenDiscriminator disc = cm.getDiscriminator();
            if (disc != null && disc.getMappedModels() != null && !disc.getMappedModels().isEmpty()) {
                List<String> entries = new ArrayList<>();
                for (org.openapitools.codegen.CodegenDiscriminator.MappedModel mm : disc.getMappedModels()) {
                    entries.add("\"" + mm.getMappingName() + "\" => " + mm.getModelName());
                }
                String prop = disc.getPropertyBaseName() != null ? disc.getPropertyBaseName() : disc.getPropertyName();
                cm.vendorExtensions.put("x-cr-discriminator-prop", prop);
                cm.vendorExtensions.put("x-cr-discriminator-map", "{" + String.join(", ", entries) + "}");
            }
        }
        return processed;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        // Build a name -> model index so parent relationships can be resolved here (parentModel
        // is not yet populated during postProcessModels).
        Map<String, CodegenModel> byName = new HashMap<>();
        for (ModelsMap mm : processed.values()) {
            for (ModelMap mo : mm.getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm != null) byName.put(cm.classname, cm);
            }
        }

        for (CodegenModel cm : byName.values()) {
            if (cm.parent == null) continue;
            CodegenModel parent = byName.get(cm.parent);
            if (parent == null) continue;

            // Crystal forbids re-annotating an ivar already defined in a superclass, and
            // JSON::Serializable inherits the parent's fields, so a child must NOT re-declare
            // inherited properties. Mark them so the template skips their declaration/validation.
            Set<String> inheritedBaseNames = new HashSet<>();
            for (CodegenProperty pp : parent.getAllVars()) inheritedBaseNames.add(pp.baseName);

            List<CodegenProperty> all = new ArrayList<>(cm.vars);
            all.addAll(cm.requiredVars);
            all.addAll(cm.optionalVars);
            for (CodegenProperty p : all) {
                p.vendorExtensions.put("x-cr-inherited", inheritedBaseNames.contains(p.baseName));
            }

            // Arguments passed to `super(...)` from the child constructor, in the parent's own
            // constructor order (required vars then optional vars). The child accepts these as
            // plain (non-@) params with the same names.
            List<String> superArgs = new ArrayList<>();
            for (CodegenProperty pp : parent.requiredVars) superArgs.add(pp.name);
            for (CodegenProperty pp : parent.optionalVars) superArgs.add(pp.name);
            cm.vendorExtensions.put("x-cr-parent-args", String.join(", ", superArgs));
        }

        return processed;
    }

    /**
     * Qualify bare model-name tokens in a Crystal type string with the module name, so that inside
     * an api resource class they resolve to the model and not to a same-named resource class.
     * e.g. with module "Foo": "Array(Pet)" -> "Array(Foo::Pet)". Primitives (Int32, String, Array,
     * Hash, ...) and already-qualified names are left untouched.
     */
    private String qualifyModelTypes(String type, Set<String> modelNames) {
        if (type == null || type.isEmpty() || modelNames.isEmpty()) return type;
        java.util.regex.Matcher m = java.util.regex.Pattern.compile("[A-Za-z_][A-Za-z0-9_]*").matcher(type);
        StringBuilder out = new StringBuilder();
        int last = 0;
        while (m.find()) {
            out.append(type, last, m.start());
            String tok = m.group();
            boolean alreadyQualified = m.start() >= 1 && type.charAt(m.start() - 1) == ':';
            if (modelNames.contains(tok) && !alreadyQualified) {
                out.append(moduleName).append("::").append(tok);
            } else {
                out.append(tok);
            }
            last = m.end();
        }
        out.append(type.substring(last));
        return out.toString();
    }

    @Override
    public String toOperationId(String operationId) {
        // rename to empty_method_name_1 (e.g.) if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            operationId = underscore("empty_method_name_" + emptyMethodNameCounter++);
            LOGGER.warn("Empty method name (operationId) found. Renamed to {}", operationId);
            return operationId;
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = underscore("call_" + operationId);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(sanitizeName(operationId));
    }

    @Override
    public String toApiImport(String name) {
        return shardName + "/" + apiPackage() + "/" + toApiFilename(name);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        final Schema additionalProperties = ModelUtils.getAdditionalProperties(schema);

        if (additionalProperties != null) {
            codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        OperationMap operations0 = objs.getOperations();
        String classname = (operations0 != null) ? operations0.getClassname() : "";

        // The api classname is "<apiNamespace>::<rest>" (toApiName prefixes the configured
        // api namespace, default "Api"). The template opens that namespace as an explicit
        // nested `module <apiNamespace>`, so it needs the bare resource path with the prefix
        // stripped. Defining it via an explicit `module` (rather than `class Api::<rest>`)
        // makes the fully-qualified path actually exist even when moduleName itself ends with
        // "Api"; with apiNamespace="" the api classes nest directly under moduleName.
        String apiNsPrefix = (apiNamespace == null || apiNamespace.isEmpty()) ? "" : apiNamespace + "::";
        String innerClass = (!apiNsPrefix.isEmpty() && classname.startsWith(apiNsPrefix))
            ? classname.substring(apiNsPrefix.length()) : classname;
        if (operations0 != null) {
            operations0.put("x-cr-api-inner-class", innerClass);
        }

        // Compute specHelperPath: relative path from the spec test file back to spec_helper.cr.
        // The spec file lives at spec/<apiPackage>/<namespace>/<resource>_spec.cr, i.e. one
        // directory level for api/ plus one per "::" in the (prefix-free) resource class path.
        // "Pet"          -> 0 "::" -> 1 level  -> "../spec_helper"
        // "Store::Order" -> 1 "::" -> 2 levels -> "../../spec_helper"
        int colonPairs = 0;
        int idx = innerClass.indexOf("::");
        while (idx != -1) {
            colonPairs++;
            idx = innerClass.indexOf("::", idx + 2);
        }
        int totalLevels = colonPairs + 1;
        StringBuilder specHelperPath = new StringBuilder();
        for (int i = 0; i < totalLevels; i++) {
            specHelperPath.append("../");
        }
        specHelperPath.append("spec_helper");
        objs.put("specHelperPath", specHelperPath.toString());

        if (isSkipOperationExample()) {
            return objs;
        }

        OperationMap operations = objs.getOperations();
        HashMap<String, CodegenModel> modelMaps = ModelMap.toCodegenModelMap(allModels);
        HashMap<String, Integer> processedModelMaps = new HashMap<>();

        List<CodegenOperation> operationList = operations.getOperation();
        for (CodegenOperation op : operationList) {
            boolean hasHeaderParams = op.headerParams != null && !op.headerParams.isEmpty();
            boolean hasCookieParams = op.cookieParams != null && !op.cookieParams.isEmpty();
            boolean hasNamed = (op.queryParams != null && !op.queryParams.isEmpty())
                            || hasHeaderParams || hasCookieParams;
            op.vendorExtensions.put("x-cr-has-named-params", hasNamed);
            op.vendorExtensions.put("x-cr-has-header-params", hasHeaderParams);
            op.vendorExtensions.put("x-cr-has-cookie-params", hasCookieParams);
            // an operation needs a `header:` hash if it has header params and/or cookie params
            // (cookie params are sent via a combined Cookie header).
            op.vendorExtensions.put("x-cr-has-header-or-cookie", hasHeaderParams || hasCookieParams);

            // raw body: an operation whose response media type isn't JSON (text/plain, binary, ...)
            // returns the body untouched instead of JSON-decoding it.
            boolean rawBody = op.produces != null && !op.produces.isEmpty()
                && op.produces.stream().noneMatch(p -> {
                    String mt = p.get("mediaType");
                    return mt != null && mt.toLowerCase(Locale.ROOT).contains("json");
                });
            op.vendorExtensions.put("x-cr-raw-body", rawBody);

            // collectionFormat: array query params that aren't "multi" must be joined into a single
            // value with the right separator (csv/ssv/tsv/pipes); "multi" is left as an array and
            // serialised key=a&key=b by the configured params encoder.
            if (op.queryParams != null) {
                for (CodegenParameter p : op.queryParams) {
                    if (!p.isArray || p.collectionFormat == null) continue;
                    String sep;
                    switch (p.collectionFormat) {
                        case "ssv": sep = " "; break;
                        case "tsv": sep = "\\t"; break;
                        case "pipes": case "pipe": sep = "|"; break;
                        case "csv": sep = ","; break;
                        default: sep = null; // "multi" (or unknown) -> no join
                    }
                    if (sep != null) p.vendorExtensions.put("x-cr-join-sep", sep);
                }
            }
            // Inside an api resource class (e.g. Api::Pet) the unqualified name `Pet` resolves to
            // the class itself, shadowing a same-named model. Qualify model types in the method
            // signature and body with the module so they resolve to the model (Foo::Pet).
            op.vendorExtensions.put("x-cr-return-type", qualifyModelTypes(op.returnType, modelMaps.keySet()));
            // The template reads param types from the per-kind lists (pathParams, queryParams, ...),
            // which are distinct objects from allParams, so qualify each of those.
            for (List<CodegenParameter> pl : Arrays.asList(op.pathParams, op.queryParams,
                    op.headerParams, op.formParams, op.cookieParams, op.allParams)) {
                if (pl == null) continue;
                for (CodegenParameter p : pl) {
                    p.vendorExtensions.put("x-cr-data-type", qualifyModelTypes(p.dataType, modelMaps.keySet()));
                }
            }
            if (op.bodyParam != null) {
                op.bodyParam.vendorExtensions.put("x-cr-data-type",
                    qualifyModelTypes(op.bodyParam.dataType, modelMaps.keySet()));
            }

            for (CodegenParameter p : op.allParams) {
                p.vendorExtensions.put("x-crystal-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.requiredParams) {
                p.vendorExtensions.put("x-crystal-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.optionalParams) {
                p.vendorExtensions.put("x-crystal-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.bodyParams) {
                p.vendorExtensions.put("x-crystal-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.pathParams) {
                p.vendorExtensions.put("x-crystal-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
        }

        return objs;
    }

    private String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenParameter.isArray) { // array
            if (codegenParameter.items == null) {
                return "[]";
            }
            return "[" + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "]";
        } else if (codegenParameter.isMap) {
            if (codegenParameter.items == null) {
                return "{}";
            }
            return "{ key: " + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "}";
        } else if (codegenParameter.isPrimitiveType) { // primitive type
            if (codegenParameter.isEnum) {
                // When inline enum, set example to first allowable value
                List<Object> values = (List<Object>) codegenParameter.allowableValues.get("values");
                codegenParameter.setExample(String.valueOf(values.get(0)));
            }
            if (codegenParameter.isString || "String".equalsIgnoreCase(codegenParameter.baseType)) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "'" + codegenParameter.example + "'";
                }
                return "'" + codegenParameter.paramName + "_example'";
            } else if (codegenParameter.isBoolean) { // boolean
                if (Boolean.parseBoolean(codegenParameter.example)) {
                    return "true";
                }
                return "false";
            } else if (codegenParameter.isUri) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "'" + codegenParameter.example + "'";
                }
                return "'https://example.com'";
            } else if (codegenParameter.isDateTime) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "Time.parse('" + codegenParameter.example + "')";
                }
                return "Time.now";
            } else if (codegenParameter.isDate) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return "Date.parse('" + codegenParameter.example + "')";
                }
                return "Date.today";
            } else if (codegenParameter.isFile) {
                return "File.new('/path/to/some/file')";
            } else if (codegenParameter.isInteger) {
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return codegenParameter.example;
                }
                return "37";
            } else { // number
                if (!StringUtils.isEmpty(codegenParameter.example) && !"null".equals(codegenParameter.example)) {
                    return codegenParameter.example;
                }
                return "3.56";
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, processedModelMap);
            } else {
                // LOGGER.error("Error in constructing examples. Failed to look up the model " +
                // codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenProperty.isArray) { // array
            return "[" + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "]";
        } else if (codegenProperty.isMap) {
            if (codegenProperty.items != null) {
                return "{ key: " + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "}";
            } else {
                return "{ ... }";
            }
        } else if (codegenProperty.isPrimitiveType) { // primitive type
            if (codegenProperty.isEnum) {
                // When inline enum, set example to first allowable value
                List<Object> values = (List<Object>) codegenProperty.allowableValues.get("values");
                codegenProperty.example = String.valueOf(values.get(0));
            }
            if (codegenProperty.isString || "String".equalsIgnoreCase(codegenProperty.baseType)) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "'" + codegenProperty.example + "'";
                } else {
                    return "'" + codegenProperty.name + "_example'";
                }
            } else if (codegenProperty.isBoolean) { // boolean
                if (Boolean.parseBoolean(codegenProperty.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if (codegenProperty.isUri) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "'" + codegenProperty.example + "'";
                }
                return "'https://example.com'";
            } else if (codegenProperty.isDateTime) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "Time.parse('" + codegenProperty.example + "')";
                }
                return "Time.now";
            } else if (codegenProperty.isDate) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return "Date.parse('" + codegenProperty.example + "')";
                }
                return "Date.today";
            } else if (codegenProperty.isFile) {
                return "File.new('/path/to/some/file')";
            } else if (codegenProperty.isInteger) {
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return codegenProperty.example;
                }
                return "37";
            } else { // number
                if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                    return codegenProperty.example;
                }
                return "3.56";
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, processedModelMap);
            } else {
                // LOGGER.error("Error in constructing examples. Failed to look up the model " +
                // codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        // break infinite recursion. Return, in case a model is already processed in the
        // current context.
        String model = codegenModel.name;
        if (processedModelMap.containsKey(model)) {
            int count = processedModelMap.get(model);
            if (count == 1) {
                processedModelMap.put(model, 2);
            } else if (count == 2) {
                return "";
            } else {
                throw new RuntimeException("Invalid count when constructing example: " + count);
            }
        } else if (codegenModel.isEnum) {
            List<Map<String, String>> enumVars = (List<Map<String, String>>) codegenModel.allowableValues.get("enumVars");
            return moduleName + "::" + codegenModel.classname + "::" + enumVars.get(0).get("name");
        } else if (codegenModel.oneOf != null && !codegenModel.oneOf.isEmpty()) {
            String subModel = (String) codegenModel.oneOf.toArray()[0];
            if (modelMaps.get(subModel) == null) {
                if (subModel.startsWith("Array(")) {
                    subModel = StringUtils.removeEnd(subModel.substring(6), ")");
                    if (modelMaps.get(subModel) == null) {
                        LOGGER.warn("Cannot find codegen for SubModel: {} (model: {})", subModel, model);
                        return "";
                    } else {
                        LOGGER.info("Found Array codegen for SubModel: {} (model: {})", subModel, model);
                        String oneOf = "[" + constructExampleCode(modelMaps.get(subModel), modelMaps, processedModelMap) + "]";
                        return oneOf;
                    }
                }
            } else {
                LOGGER.info("Found codegen for SubModel: {} (model: {})", subModel, model);
                String oneOf = constructExampleCode(modelMaps.get(subModel), modelMaps, processedModelMap);
                return oneOf;
            }
        } else {
            processedModelMap.put(model, 1);
        }

        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.requiredVars) {
            propertyExamples.add(
                    codegenProperty.name + ": " + constructExampleCode(codegenProperty, modelMaps, processedModelMap));
        }
        String example = moduleName + "::" + toModelName(model) + ".new";
        if (!propertyExamples.isEmpty()) {
            example += "(" + StringUtils.join(propertyExamples, ", ") + ")";
        }
        return example;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        if (ModelUtils.isArraySchema(schema)) {
            Schema inner = ModelUtils.getSchemaItems(schema);
            // unresolved element type (e.g. nested array with no `items`) would emit a
            // bare `Array(Array)` which Crystal reads as Array(Array(T)) and cannot
            // (de)serialise; fall back to JSON::Any so the type stays valid and round-trippable
            if (inner == null) {
                return getSchemaType(schema) + "(JSON::Any)";
            }
            return getSchemaType(schema) + "(" + getTypeDeclaration(inner) + ")";
        } else if (ModelUtils.isMapSchema(schema)) {
            Schema inner = ModelUtils.getAdditionalProperties(schema);
            if (inner == null) {
                return getSchemaType(schema) + "(String, JSON::Any)";
            }
            return getSchemaType(schema) + "(String, " + getTypeDeclaration(inner) + ")";
        }

        // A schema whose element type cannot be resolved (e.g. an inner `array`/`object`
        // with no items/additionalProperties) falls through to a bare collection name
        // ("Array"/"Set"/"Hash"), which Crystal treats as the uninstantiated generic
        // Array(T) and cannot (de)serialise. Pin the element type to JSON::Any.
        String decl = super.getTypeDeclaration(schema);
        if ("Array".equals(decl) || "Set".equals(decl)) {
            return decl + "(JSON::Any)";
        }
        if ("Hash".equals(decl)) {
            return decl + "(String, JSON::Any)";
        }
        return decl;
    }

    @Override
    public String toInstantiationType(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            return instantiationTypes.get("map");
        } else if (ModelUtils.isArraySchema(schema)) {
            String parentType;
            if (ModelUtils.isSet(schema)) {
                parentType = "set";
            } else {
                parentType = "array";
            }
            return instantiationTypes.get(parentType);
        }
        return super.toInstantiationType(schema);
    }

    @Override
    public String toDefaultValue(Schema p) {
        p = ModelUtils.getReferencedSchema(this.openAPI, p);
        if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                if (p.getDefault() instanceof Date) {
                    Date date = (Date) p.getDefault();
                    LocalDate localDate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
                    return "Date.parse(\"" + String.format(Locale.ROOT, localDate.toString(), "") + "\")";
                } else if (p.getDefault() instanceof java.time.OffsetDateTime) {
                    return "Time.parse(\"" + String.format(Locale.ROOT, ((java.time.OffsetDateTime) p.getDefault())
                            .atZoneSameInstant(ZoneId.systemDefault()).toString(), "") + "\")";
                } else {
                    return "\"" + escapeText((String.valueOf(p.getDefault()))) + "\"";
                }
            }
        }

        return null;
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "::" + value;
    }

    @Override
    public String toVarName(final String name) {
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

        String varName;
        // sanitize name
        varName = sanitizeName(name);
        // if it's all upper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            varName = varName.toLowerCase(Locale.ROOT);
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        varName = underscore(varName);

        // for reserved word or word starting with number, append _
        if (isReservedWord(varName) || varName.matches("^\\d.*")) {
            varName = escapeReservedWord(varName);
        }

        return varName;
    }

    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("=end", "=_end").replace("=begin", "=_begin").replace("#{", "\\#{");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }
        String crystalPostProcessFile = System.getenv("CRYSTAL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(crystalPostProcessFile)) {
            return; // skip if CRYSTAL_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with cr extension
        if ("cr".equals(FilenameUtils.getExtension(file.toString()))) {
            this.executePostProcessor(new String[]{crystalPostProcessFile, file.toString()});
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Map<String, Object>> nsMap = new TreeMap<>();
        Map<String, Set<String>> resourcesByNs = new TreeMap<>();

        org.openapitools.codegen.model.ApiInfoMap apiInfo =
            (org.openapitools.codegen.model.ApiInfoMap) objs.get("apiInfo");
        if (apiInfo != null) {
            List<org.openapitools.codegen.model.OperationsMap> apis = apiInfo.getApis();
            if (apis != null) {
                for (org.openapitools.codegen.model.OperationsMap operationsMap : apis) {
                    org.openapitools.codegen.model.OperationMap opMap = operationsMap.getOperations();
                    if (opMap == null) continue;
                    List<CodegenOperation> ops = opMap.getOperation();
                    if (ops == null) continue;
                    for (CodegenOperation co : ops) {
                        String ns = (String) co.vendorExtensions.get("x-cr-namespace");
                        String res = (String) co.vendorExtensions.get("x-cr-resource");
                        if (ns == null) continue;
                        nsMap.computeIfAbsent(ns, k -> {
                            Map<String, Object> m = new HashMap<>();
                            m.put("name", underscore(sanitizeName(ns.replace('-', '_'))));
                            m.put("className", toApiName(ns));
                            return m;
                        });
                        if (res != null) {
                            resourcesByNs.computeIfAbsent(ns, k -> new TreeSet<>()).add(res);
                        }
                    }
                }
            }
        }

        List<Map<String, Object>> nsList = new ArrayList<>();
        for (Map.Entry<String, Map<String, Object>> e : nsMap.entrySet()) {
            List<Map<String, Object>> resList = new ArrayList<>();
            for (String res : resourcesByNs.getOrDefault(e.getKey(), Collections.emptySet())) {
                Map<String, Object> rm = new HashMap<>();
                rm.put("accessor", underscore(sanitizeName(res.replace('-', '_'))));
                rm.put("className", toApiName(e.getKey() + "/" + res));
                resList.add(rm);
            }
            e.getValue().put("resources", resList);
            nsList.add(e.getValue());
        }
        additionalProperties.put("crNamespaces", nsList);
        objs.put("crNamespaces", nsList);

        orderModelsByInheritance(objs);

        return super.postProcessSupportingFileData(objs);
    }

    /**
     * Crystal resolves a superclass at the point a subclass is parsed, so a model file
     * must be {@code require}d after the file defining its parent. The default model order
     * is alphabetical, which breaks e.g. {@code class Child < Parent} when "child" sorts
     * before "parent". Reorder the model list used by the shard entrypoint so every parent
     * precedes its children (inheritance is acyclic, so a topological order always exists).
     */
    @SuppressWarnings("unchecked")
    private void orderModelsByInheritance(Map<String, Object> objs) {
        Object modelsObj = objs.get("models");
        if (!(modelsObj instanceof List)) return;
        List<Object> models = (List<Object>) modelsObj;

        Map<String, Object> entryByName = new LinkedHashMap<>();
        Map<String, String> parentByName = new HashMap<>();
        for (Object entry : models) {
            if (!(entry instanceof Map)) return; // unexpected shape: leave as-is
            Object m = ((Map<String, Object>) entry).get("model");
            if (!(m instanceof CodegenModel)) return;
            CodegenModel cm = (CodegenModel) m;
            entryByName.put(cm.classname, entry);
            String parent = cm.parentModel != null ? cm.parentModel.classname : cm.parent;
            if (parent != null) parentByName.put(cm.classname, parent);
        }

        List<Object> ordered = new ArrayList<>(models.size());
        Set<String> placed = new HashSet<>();
        for (String name : entryByName.keySet()) {
            placeModelAfterParent(name, entryByName, parentByName, placed, ordered);
        }

        if (ordered.size() == models.size()) {
            models.clear();
            models.addAll(ordered);
        }
    }

    private void placeModelAfterParent(String name, Map<String, Object> entryByName,
                                       Map<String, String> parentByName, Set<String> placed,
                                       List<Object> ordered) {
        if (name == null || placed.contains(name)) return;
        Object entry = entryByName.get(name);
        if (entry == null) return; // parent isn't a generated model (e.g. a primitive/container)
        placed.add(name); // mark before recursing so a malformed cycle can't loop forever
        String parent = parentByName.get(name);
        if (parent != null) {
            placeModelAfterParent(parent, entryByName, parentByName, placed, ordered);
        }
        ordered.add(entry);
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.CRYSTAL;
    }
}
