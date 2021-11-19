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

import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RubyClientCodegen extends AbstractRubyCodegen {
    public static final String GEM_VERSION = "gemVersion";
    public static final String GEM_LICENSE = "gemLicense";
    public static final String GEM_REQUIRED_RUBY_VERSION = "gemRequiredRubyVersion";
    public static final String GEM_HOMEPAGE = "gemHomepage";
    public static final String GEM_SUMMARY = "gemSummary";
    public static final String GEM_DESCRIPTION = "gemDescription";
    public static final String GEM_AUTHOR = "gemAuthor";
    public static final String GEM_AUTHOR_EMAIL = "gemAuthorEmail";
    public static final String FARADAY = "faraday";
    public static final String TYPHOEUS = "typhoeus";
    private final Logger LOGGER = LoggerFactory.getLogger(RubyClientCodegen.class);
    private static final String NUMERIC_ENUM_PREFIX = "N";
    protected static int emptyMethodNameCounter = 0;
    protected String gemName;
    protected String moduleName;
    protected String gemVersion = "1.0.0";
    protected String specFolder = "spec";
    protected String libFolder = "lib";
    protected String gemLicense = "unlicense";
    protected String gemRequiredRubyVersion = ">= 1.9";
    protected String gemHomepage = "https://openapitools.org";
    protected String gemSummary = "A Ruby SDK for the REST API";
    protected String gemDescription = "This gem maps to a REST API";
    protected String gemAuthor = "";
    protected String gemAuthorEmail = "";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public RubyClientCodegen() {
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

        // clear import mapping (from default generator) as ruby does not use it
        // at the moment
        importMapping.clear();

        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separator + "ruby";
        modelTemplateFiles.put("model.mustache", ".rb");
        apiTemplateFiles.put("api.mustache", ".rb");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "ruby-client";

        modelTestTemplateFiles.put("model_test.mustache", ".rb");
        apiTestTemplateFiles.put("api_test.mustache", ".rb");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // local variable names used in API methods (endpoints)
        for (String word : Arrays.asList(
                "local_var_path", "query_params", "header_params", "_header_accept", "_header_accept_result",
                "_header_content_type", "form_params", "post_body", "auth_names", "send")) {
            reservedWords.add(word.toLowerCase(Locale.ROOT));
        }

        // primitives in ruby lang
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("array");
        languageSpecificPrimitives.add("map");
        languageSpecificPrimitives.add("string");

        // remove modelPackage and apiPackage added by default
        cliOptions.removeIf(opt -> CodegenConstants.MODEL_PACKAGE.equals(opt.getOpt()) ||
                CodegenConstants.API_PACKAGE.equals(opt.getOpt()));

        cliOptions.add(new CliOption(CodegenConstants.GEM_NAME, CodegenConstants.GEM_NAME_DESC).
                defaultValue("openapi_client"));

        cliOptions.add(new CliOption(CodegenConstants.MODULE_NAME, CodegenConstants.MODULE_NAME_DESC).
                defaultValue("OpenAPIClient"));

        cliOptions.add(new CliOption(GEM_VERSION, "gem version.").defaultValue("1.0.0"));

        cliOptions.add(new CliOption(GEM_LICENSE, "gem license. ").
                defaultValue("unlicense"));

        cliOptions.add(new CliOption(GEM_REQUIRED_RUBY_VERSION, "gem required Ruby version. ").
                defaultValue(">= 1.9"));

        cliOptions.add(new CliOption(GEM_HOMEPAGE, "gem homepage. ").
                defaultValue("http://org.openapitools"));

        cliOptions.add(new CliOption(GEM_SUMMARY, "gem summary. ").
                defaultValue("A ruby wrapper for the REST APIs"));

        cliOptions.add(new CliOption(GEM_DESCRIPTION, "gem description. ").
                defaultValue("This gem maps to a REST API"));

        cliOptions.add(new CliOption(GEM_AUTHOR, "gem author (only one is supported)."));

        cliOptions.add(new CliOption(GEM_AUTHOR_EMAIL, "gem author email (only one is supported)."));

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).
                defaultValue(Boolean.TRUE.toString()));

        supportedLibraries.put(FARADAY, "Faraday (https://github.com/lostisland/faraday) (Beta support)");
        supportedLibraries.put(TYPHOEUS, "Typhoeus >= 1.0.1 (https://github.com/typhoeus/typhoeus)");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "HTTP library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set TYPHOEUS as the default
        libraryOption.setDefault(TYPHOEUS);
        cliOptions.add(libraryOption);
        setLibrary(TYPHOEUS);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.GEM_NAME)) {
            setGemName((String) additionalProperties.get(CodegenConstants.GEM_NAME));
        }
        if (additionalProperties.containsKey(CodegenConstants.MODULE_NAME)) {
            setModuleName((String) additionalProperties.get(CodegenConstants.MODULE_NAME));
        }

        if (gemName == null && moduleName == null) {
            setGemName("openapi_client");
            setModuleName(generateModuleName(gemName));
        } else if (gemName == null) {
            setGemName(generateGemName(moduleName));
        } else if (moduleName == null) {
            setModuleName(generateModuleName(gemName));
        }

        additionalProperties.put(CodegenConstants.GEM_NAME, gemName);
        additionalProperties.put(CodegenConstants.MODULE_NAME, moduleName);

        if (additionalProperties.containsKey(GEM_VERSION)) {
            setGemVersion((String) additionalProperties.get(GEM_VERSION));
        } else {
            // not set, pass the default value to template
            additionalProperties.put(GEM_VERSION, gemVersion);
        }

        if (additionalProperties.containsKey(GEM_LICENSE)) {
            setGemLicense((String) additionalProperties.get(GEM_LICENSE));
        }

        if (additionalProperties.containsKey(GEM_REQUIRED_RUBY_VERSION)) {
            setGemRequiredRubyVersion((String) additionalProperties.get(GEM_REQUIRED_RUBY_VERSION));
        }

        if (additionalProperties.containsKey(GEM_HOMEPAGE)) {
            setGemHomepage((String) additionalProperties.get(GEM_HOMEPAGE));
        }

        if (additionalProperties.containsKey(GEM_SUMMARY)) {
            setGemSummary((String) additionalProperties.get(GEM_SUMMARY));
        }

        if (additionalProperties.containsKey(GEM_DESCRIPTION)) {
            setGemDescription((String) additionalProperties.get(GEM_DESCRIPTION));
        }

        if (additionalProperties.containsKey(GEM_AUTHOR)) {
            setGemAuthor((String) additionalProperties.get(GEM_AUTHOR));
        }

        if (additionalProperties.containsKey(GEM_AUTHOR_EMAIL)) {
            setGemAuthorEmail((String) additionalProperties.get(GEM_AUTHOR_EMAIL));
        }

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("gem.mustache", libFolder, gemName + ".rb"));
        String gemFolder = libFolder + File.separator + gemName;
        supportingFiles.add(new SupportingFile("api_error.mustache", gemFolder, "api_error.rb"));
        supportingFiles.add(new SupportingFile("version.mustache", gemFolder, "version.rb"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("Rakefile.mustache", "", "Rakefile"));
        supportingFiles.add(new SupportingFile("Gemfile.mustache", "", "Gemfile"));
        supportingFiles.add(new SupportingFile("rubocop.mustache", "", ".rubocop.yml"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("gemspec.mustache", "", gemName + ".gemspec"));
        supportingFiles.add(new SupportingFile("configuration.mustache", gemFolder, "configuration.rb"));
        supportingFiles.add(new SupportingFile("api_client.mustache", gemFolder, "api_client.rb"));

        if (TYPHOEUS.equals(getLibrary())) {
            // for Typhoeus
        } else if (FARADAY.equals(getLibrary())) {
            // for Faraday
            additionalProperties.put("isFaraday", Boolean.TRUE);
        } else {
            throw new RuntimeException("Invalid HTTP library " + getLibrary() + ". Only faraday, typhoeus are supported.");
        }

        // test files should not be overwritten
        supportingFiles.add(new SupportingFile("rspec.mustache", "", ".rspec")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("spec_helper.mustache", specFolder, "spec_helper.rb")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("configuration_spec.mustache", specFolder, "configuration_spec.rb")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("api_client_spec.mustache", specFolder, "api_client_spec.rb")
                .doNotOverwrite());
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "ruby";
    }

    @Override
    public String getHelp() {
        return "Generates a Ruby client library.";
    }

    /**
     * Generate Ruby module name from the gem name, e.g. use "OpenAPIClient" for "openapi_client".
     *
     * @param gemName Ruby gem name
     *
     * @return Ruby module name
     */
    @SuppressWarnings("static-method")
    public String generateModuleName(String gemName) {
        return camelize(gemName.replaceAll("[^\\w]+", "_"));
    }

    /**
     * Generate Ruby gem name from the module name, e.g. use "openapi_client" for "OpenAPIClient".
     *
     * @param moduleName Ruby module name
     *
     * @return Ruby gem name
     */
    @SuppressWarnings("static-method")
    public String generateGemName(String moduleName) {
        return underscore(moduleName.replaceAll("[^\\w]+", ""));
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + modelPackage.replace("/", File.separator);
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

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
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

    public void setGemDescription(String gemDescription) {
        this.gemDescription = gemDescription;
    }

    public void setGemSummary(String gemSummary) {
        this.gemSummary = gemSummary;
    }

    public void setGemLicense(String gemLicense) {
        this.gemLicense = gemLicense;
    }

    public void setGemRequiredRubyVersion(String gemRequiredRubyVersion) {
        this.gemRequiredRubyVersion = gemRequiredRubyVersion;
    }

    public void setGemHomepage(String gemHomepage) {
        this.gemHomepage = gemHomepage;
    }

    public void setGemAuthor(String gemAuthor) {
        this.gemAuthor = gemAuthor;
    }

    public void setGemAuthorEmail(String gemAuthorEmail) {
        this.gemAuthorEmail = gemAuthorEmail;
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
        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        HashMap<String, Integer> processedModelMaps = new HashMap<String, Integer>();

        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            for (CodegenParameter p : op.allParams) {
                p.vendorExtensions.put("x-ruby-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.requiredParams) {
                p.vendorExtensions.put("x-ruby-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.optionalParams) {
                p.vendorExtensions.put("x-ruby-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.bodyParams) {
                p.vendorExtensions.put("x-ruby-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
            for (CodegenParameter p : op.pathParams) {
                p.vendorExtensions.put("x-ruby-example", constructExampleCode(p, modelMaps, processedModelMaps));
            }
            processedModelMaps.clear();
        }

        return objs;
    }

    private String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, HashMap<String, Integer> processedModelMap) {
        if (codegenParameter.isArray) { // array
            return "[" + constructExampleCode(codegenParameter.items, modelMaps, processedModelMap) + "]";
        } else if (codegenParameter.isMap) {
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
            if (!StringUtils.isEmpty(codegenProperty.example) && !"null".equals(codegenProperty.example)) {
                String value = codegenProperty.example;
                value = value.replaceAll(",", ", ");
                value = value.replaceAll(":", ": ");
                return value;
            }
            return "[" + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "]";
        } else if (codegenProperty.isMap) {
            return "{ key: " + constructExampleCode(codegenProperty.items, modelMaps, processedModelMap) + "}";
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
            if (modelMaps.containsKey(subModel)) {
                // oneOf models
                return constructExampleCode(modelMaps.get(subModel), modelMaps, processedModelMap);
            } else {
                // TODO oneOf primitive type not supported at the moment
                LOGGER.warn("oneOf example value not supported at the moment.");
                return "nil";
            }
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
}
