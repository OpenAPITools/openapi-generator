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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.templating.mustache.PrefixWithHashLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class CrystalClientCodegen extends DefaultCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CrystalClientCodegen.class);
    private static final String NUMERIC_ENUM_PREFIX = "N";
    protected static int emptyMethodNameCounter = 0;

    protected String shardName;
    protected String moduleName;
    protected String shardVersion = "1.0.0";
    protected String specFolder = "spec";
    protected String srcFolder = "src";
    protected String shardLicense = "unlicense";
    protected String shardHomepage = "https://openapitools.org";
    protected String shardSummary = "A Crystal SDK for the REST API";
    protected String shardDescription = "This shard maps to a REST API";
    protected String shardAuthor = "";
    protected String shardAuthorEmail = "";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public static final String SHARD_NAME = "shardName";
    public static final String SHARD_VERSION = "shardVersion";
    public static final String SHARD_LICENSE = "shardLicense";
    public static final String SHARD_HOMEPAGE = "shardHomepage";
    public static final String SHARD_SUMMARY = "shardSummary";
    public static final String SHARD_DESCRIPTION = "shardDescription";
    public static final String SHARD_AUTHOR = "shardAuthor";
    public static final String SHARD_AUTHOR_EMAIL = "shardAuthorEmail";

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

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        supportsInheritance = true;

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

        // TODO support auto-generated doc
        //modelDocTemplateFiles.put("model_doc.mustache", ".md");
        //apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // reserved word. Ref: https://github.com/crystal-lang/crystal/wiki/Crystal-for-Rubyists#available-keywords
        reservedWords = new HashSet<>(
                Arrays.asList(
                        "abstract", "annotation", "do", "if", "nil?", "select", "union",
                        "alias", "else", "in", "of", "self", "unless",
                        "as", "elsif", "include", "out", "sizeof", "until",
                        "as?", "end", "instance", "sizeof", "pointerof", "struct", "verbatim",
                        "asm", "ensure", "is_a?", "private", "super", "when",
                        "begin", "enum", "lib", "protected", "then", "while",
                        "break", "extend", "macro", "require", "true", "with",
                        "case", "false", "module", "rescue", "type", "yield",
                        "class", "for", "next", "responds_to?", "typeof",
                        "def", "fun", "nil", "return", "uninitialized")
        );

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
        typeMapping.put("date", "Time");
        typeMapping.put("DateTime", "Time");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("set", "Set");
        typeMapping.put("map", "Hash");
        typeMapping.put("object", "Object");
        typeMapping.put("file", "::File");
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");

        instantiationTypes.put("map", "Hash");
        instantiationTypes.put("array", "Array");
        instantiationTypes.put("set", "Set");

        // remove modelPackage and apiPackage added by default
        cliOptions.removeIf(opt -> CodegenConstants.MODEL_PACKAGE.equals(opt.getOpt()) ||
                CodegenConstants.API_PACKAGE.equals(opt.getOpt()));

        cliOptions.add(new CliOption(SHARD_NAME, "shard name (e.g. twitter_client").
                defaultValue("openapi_client"));

        cliOptions.add(new CliOption(SHARD_VERSION, "shard version.").defaultValue("1.0.0"));

        cliOptions.add(new CliOption(SHARD_LICENSE, "shard license.").
                defaultValue("unlicense"));

        cliOptions.add(new CliOption(SHARD_HOMEPAGE, "shard homepage.").
                defaultValue("http://org.openapitools"));

        cliOptions.add(new CliOption(SHARD_DESCRIPTION, "shard description.").
                defaultValue("This shard maps to a REST API"));

        cliOptions.add(new CliOption(SHARD_AUTHOR, "shard author (only one is supported)."));

        cliOptions.add(new CliOption(SHARD_AUTHOR_EMAIL, "shard author email (only one is supported)."));

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).
                defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("CRYSTAL_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable 'CRYSTAL_POST_PROCESS_FILE' (optional) not defined. E.g. to format the source code, please try 'export CRYSTAL_POST_PROCESS_FILE=\"/usr/local/bin/crystal tool format\"' (Linux/Mac)");
        }

        if (additionalProperties.containsKey(SHARD_NAME)) {
            setShardName((String) additionalProperties.get(SHARD_NAME));
        }
        additionalProperties.put(SHARD_NAME, shardName);

        if (additionalProperties.containsKey(SHARD_VERSION)) {
            setShardVersion((String) additionalProperties.get(SHARD_VERSION));
        } else {
            // not set, pass the default value to template
            additionalProperties.put(SHARD_VERSION, shardVersion);
        }

        if (additionalProperties.containsKey(SHARD_LICENSE)) {
            setShardLicense((String) additionalProperties.get(SHARD_LICENSE));
        }

        if (additionalProperties.containsKey(SHARD_HOMEPAGE)) {
            setShardHomepage((String) additionalProperties.get(SHARD_HOMEPAGE));
        }

        if (additionalProperties.containsKey(SHARD_SUMMARY)) {
            setShardSummary((String) additionalProperties.get(SHARD_SUMMARY));
        }

        if (additionalProperties.containsKey(SHARD_DESCRIPTION)) {
            setShardDescription((String) additionalProperties.get(SHARD_DESCRIPTION));
        }

        if (additionalProperties.containsKey(SHARD_AUTHOR)) {
            setShardAuthor((String) additionalProperties.get(SHARD_AUTHOR));
        }

        if (additionalProperties.containsKey(SHARD_AUTHOR_EMAIL)) {
            setShardAuthorEmail((String) additionalProperties.get(SHARD_AUTHOR_EMAIL));
        }

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("shard_name.mustache", srcFolder, shardName + ".cr"));
        String shardFolder = srcFolder + File.separator + shardName;
        supportingFiles.add(new SupportingFile("api_error.mustache", shardFolder, "api_error.cr"));
        supportingFiles.add(new SupportingFile("configuration.mustache", shardFolder, "configuration.cr"));
        supportingFiles.add(new SupportingFile("api_client.mustache", shardFolder, "api_client.cr"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("shard.mustache", "", "shard.yml"));

        // crystal spec files
        supportingFiles.add(new SupportingFile("spec_helper.mustache", specFolder, "spec_helper.cr")
                .doNotOverwrite());

        // add lambda for mustache templates
        additionalProperties.put("lambdaPrefixWithHash", new PrefixWithHashLambda());

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
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
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
    public String toModelName(final String name) {
        String modelName;
        modelName = sanitizeName(name);

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

        // model name starts with number
        if (modelName.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", modelName,
                    camelize("model_" + modelName));
            modelName = "model_" + modelName; // e.g. 200Response => Model200Response (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(modelName);
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(final String name) {
        // replace - with _ e.g. created-at => created_at
        String filename = name;
        if (apiNameSuffix != null && apiNameSuffix.length() > 0) {
            filename = filename + "_" + apiNameSuffix;
        }

        filename = filename.replaceAll("-", "_");

        // e.g. PhoneNumberApi.cr => phone_number_api.cr
        return underscore(filename);
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
        return super.toApiName(name);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("Integer".equals(datatype) || "Float".equals(datatype)) {
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
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
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

    public void setShardName(String shardName) {
        this.shardName = shardName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setShardVersion(String shardVersion) {
        this.shardVersion = shardVersion;
    }

    public void setShardDescription(String shardDescription) {
        this.shardDescription = shardDescription;
    }

    public void setShardSummary(String shardSummary) {
        this.shardSummary = shardSummary;
    }

    public void setShardLicense(String shardLicense) {
        this.shardLicense = shardLicense;
    }

    public void setShardHomepage(String shardHomepage) {
        this.shardHomepage = shardHomepage;
    }

    public void setShardAuthor(String shardAuthor) {
        this.shardAuthor = shardAuthor;
    }

    public void setShardAuthorEmail(String shardAuthorEmail) {
        this.shardAuthorEmail = shardAuthorEmail;
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        final Schema additionalProperties = getAdditionalProperties(schema);

        if (additionalProperties != null) {
            codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        HashMap<String, CodegenModel> modelMaps = new HashMap<>();
        HashMap<String, Integer> processedModelMaps = new HashMap<>();

        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
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
                codegenParameter.example = String.valueOf(values.get(0));
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
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
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
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    private String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        // break infinite recursion. Return, in case a model is already processed in the current context.
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
            String oneOf = constructExampleCode(modelMaps.get(subModel), modelMaps, processedModelMap);
            return oneOf;
        } else {
            processedModelMap.put(model, 1);
        }

        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.requiredVars) {
            propertyExamples.add(codegenProperty.name + ": " + constructExampleCode(codegenProperty, modelMaps, processedModelMap));
        }
        String example = moduleName + "::" + toModelName(model) + ".new";
        if (!propertyExamples.isEmpty()) {
            example += "({" + StringUtils.join(propertyExamples, ", ") + "})";
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
            Schema inner = ((ArraySchema) schema).getItems();
            return getSchemaType(schema) + "(" + getTypeDeclaration(inner) + ")";
        } else if (ModelUtils.isMapSchema(schema)) {
            Schema inner = getAdditionalProperties(schema);
            return getSchemaType(schema) + "(String, " + getTypeDeclaration(inner) + ")";
        }

        return super.getTypeDeclaration(schema);
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
                    return "Time.parse(\"" + String.format(Locale.ROOT, ((java.time.OffsetDateTime) p.getDefault()).atZoneSameInstant(ZoneId.systemDefault()).toString(), "") + "\")";
                } else {
                    return "\"" + escapeText((String) p.getDefault()) + "\"";
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
        if (file == null) {
            return;
        }
        String crystalPostProcessFile = System.getenv("CRYSTAL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(crystalPostProcessFile)) {
            return; // skip if CRYSTAL_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with cr extension
        if ("cr".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = crystalPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    try (InputStreamReader inputStreamReader = new InputStreamReader(p.getErrorStream(), StandardCharsets.UTF_8);
                         BufferedReader br = new BufferedReader(inputStreamReader)) {
                        StringBuilder sb = new StringBuilder();
                        String line;
                        while ((line = br.readLine()) != null) {
                            sb.append(line);
                        }
                        LOGGER.error("Error running the command ({}). Exit value: {}, Error output: {}", command, exitValue, sb);
                    }
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.CRYSTAL; }
}
