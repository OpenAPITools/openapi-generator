/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.common.base.CaseFormat;
import com.samskivert.mustache.Mustache.Compiler;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.CookieParameter;
import io.swagger.v3.oas.models.parameters.HeaderParameter;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.PathParameter;
import io.swagger.v3.oas.models.parameters.QueryParameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.OAuthFlow;
import io.swagger.v3.oas.models.security.OAuthFlows;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultCodegen.class);

    protected String inputSpec;
    protected String outputFolder = "";
    protected Set<String> defaultIncludes = new HashSet<String>();
    protected Map<String, String> typeMapping = new HashMap<String, String>();
    protected Map<String, String> instantiationTypes = new HashMap<String, String>();
    protected Set<String> reservedWords = new HashSet<String>();
    protected Set<String> languageSpecificPrimitives = new HashSet<String>();
    protected Map<String, String> importMapping = new HashMap<String, String>();
    protected String modelPackage = "", apiPackage = "", fileSuffix;
    protected String modelNamePrefix = "", modelNameSuffix = "";
    protected String testPackage = "";
    protected Map<String, String> apiTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> apiTestTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelTestTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> apiDocTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelDocTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> reservedWordsMappings = new HashMap<String, String>();
    protected String templateDir;
    protected String embeddedTemplateDir;
    protected String commonTemplateDir = "_common";
    protected Map<String, Object> additionalProperties = new HashMap<String, Object>();
    protected Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    protected List<SupportingFile> supportingFiles = new ArrayList<SupportingFile>();
    protected List<CliOption> cliOptions = new ArrayList<CliOption>();
    protected boolean skipOverwrite;
    protected boolean removeOperationIdPrefix;
    protected boolean supportsInheritance;
    protected boolean supportsMixins;
    protected Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();
    protected String library;
    protected Boolean sortParamsByRequiredFlag = true;
    protected Boolean ensureUniqueParams = true;
    protected Boolean allowUnicodeIdentifiers = false;
    protected String gitUserId, gitRepoId, releaseNote;
    protected String httpUserAgent;
    protected Boolean hideGenerationTimestamp = true;
    // How to encode special characters like $
    // They are translated to words like "Dollar" and prefixed with '
    // Then translated back during JSON encoding and decoding
    protected Map<String, String> specialCharReplacements = new HashMap<String, String>();
    // When a model is an alias for a simple type
    protected Map<String, String> typeAliases = null;
    protected Boolean prependFormOrBodyParameters = false;

    protected String ignoreFilePathOverride;

    public List<CliOption> cliOptions() {
        return cliOptions;
    }

    public void processOpts() {
        if (additionalProperties.containsKey(CodegenConstants.TEMPLATE_DIR)) {
            this.setTemplateDir((String) additionalProperties.get(CodegenConstants.TEMPLATE_DIR));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            this.setModelPackage((String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            this.setApiPackage((String) additionalProperties.get(CodegenConstants.API_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            setHideGenerationTimestamp(convertPropertyToBooleanAndWriteBack(CodegenConstants.HIDE_GENERATION_TIMESTAMP));
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, hideGenerationTimestamp);
        }

        if (additionalProperties.containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
            this.setSortParamsByRequiredFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS)) {
            this.setPrependFormOrBodyParameters(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ENSURE_UNIQUE_PARAMS)) {
            this.setEnsureUniqueParams(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ENSURE_UNIQUE_PARAMS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS)) {
            this.setAllowUnicodeIdentifiers(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_NAME_PREFIX)) {
            this.setModelNamePrefix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_PREFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_NAME_SUFFIX)) {
            this.setModelNameSuffix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_SUFFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.REMOVE_OPERATION_ID_PREFIX)) {
            this.setRemoveOperationIdPrefix(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.REMOVE_OPERATION_ID_PREFIX).toString()));
        }
    }

    // override with any special post-processing for all models
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        if (supportsInheritance) {
            // Index all CodegenModels by model name.
            Map<String, CodegenModel> allModels = new HashMap<String, CodegenModel>();
            for (Entry<String, Object> entry : objs.entrySet()) {
                String modelName = toModelName(entry.getKey());
                Map<String, Object> inner = (Map<String, Object>) entry.getValue();
                List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
                for (Map<String, Object> mo : models) {
                    CodegenModel cm = (CodegenModel) mo.get("model");
                    allModels.put(modelName, cm);
                }
            }
            // Fix up all parent and interface CodegenModel references.
            for (CodegenModel cm : allModels.values()) {
                if (cm.getParent() != null) {
                    cm.setParentModel(allModels.get(cm.getParent()));
                }
                if (cm.getInterfaces() != null && !cm.getInterfaces().isEmpty()) {
                    cm.setInterfaceModels(new ArrayList<CodegenModel>(cm.getInterfaces().size()));
                    for (String intf : cm.getInterfaces()) {
                        CodegenModel intfModel = allModels.get(intf);
                        if (intfModel != null) {
                            cm.getInterfaceModels().add(intfModel);
                        }
                    }
                }
            }
            // Let parent know about all its children
            for (String name : allModels.keySet()) {
                CodegenModel cm = allModels.get(name);
                CodegenModel parent = allModels.get(cm.getParent());
                // if a discriminator exists on the parent, don't add this child to the inheritance heirarchy
                // TODO Determine what to do if the parent discriminator name == the grandparent discriminator name
                while (parent != null) {
                    if (parent.getChildren() == null) {
                        parent.setChildren(new ArrayList<CodegenModel>());
                    }
                    parent.getChildren().add(cm);
                    if (parent.getDiscriminator() == null) {
                        parent = allModels.get(parent.parent);
                    } else {
                        parent = null;
                    }
                }
            }
        }
        return objs;
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return objs;
    }

    /**
     * post process enum defined in model's properties
     *
     * @param objs Map of models
     * @return maps of models with better enum support
     */
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // for enum model
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                Map<String, Object> allowableValues = cm.allowableValues;
                List<Object> values = (List<Object>) allowableValues.get("values");
                List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
                String commonPrefix = findCommonPrefixOfVars(values);
                int truncateIdx = commonPrefix.length();
                for (Object value : values) {
                    Map<String, String> enumVar = new HashMap<String, String>();
                    String enumName;
                    if (truncateIdx == 0) {
                        enumName = value.toString();
                    } else {
                        enumName = value.toString().substring(truncateIdx);
                        if ("".equals(enumName)) {
                            enumName = value.toString();
                        }
                    }
                    enumVar.put("name", toEnumVarName(enumName, cm.dataType));
                    enumVar.put("value", toEnumValue(value.toString(), cm.dataType));
                    enumVars.add(enumVar);
                }
                cm.allowableValues.put("enumVars", enumVars);
            }

            // update codegen property enum with proper naming convention
            // and handling of numbers, special characters
            for (CodegenProperty var : cm.vars) {
                updateCodegenPropertyEnum(var);
            }

        }
        return objs;
    }

    /**
     * Returns the common prefix of variables for enum naming if
     * two or more variables are present
     *
     * @param vars List of variable names
     * @return the common prefix for naming
     */
    public String findCommonPrefixOfVars(List<Object> vars) {
        if (vars.size() > 1) {
            try {
                String[] listStr = vars.toArray(new String[vars.size()]);
                String prefix = StringUtils.getCommonPrefix(listStr);
                // exclude trailing characters that should be part of a valid variable
                // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
                return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
            } catch (ArrayStoreException e) {
                // do nothing, just return default value
            }
        }
        return "";
    }

    /**
     * Return the enum default value in the language specified format
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the default value for the enum
     */
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "." + value;
    }

    /**
     * Return the enum value in the language specified format
     * e.g. status becomes "status"
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized value for enum
     */
    public String toEnumValue(String value, String datatype) {
        if ("number".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = value.replaceAll("\\W+", "_").toUpperCase();
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        return objs;
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        return objs;
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        return objs;
    }

    // override to post-process any model properties
    @SuppressWarnings("unused")
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
    }

    // override to post-process any parameters
    @SuppressWarnings("unused")
    public void postProcessParameter(CodegenParameter parameter) {
    }

    //override with any special handling of the entire swagger spec
    @SuppressWarnings("unused")
    public void preprocessOpenAPI(OpenAPI openAPI) {
    }

    // override with any special handling of the entire swagger spec
    @SuppressWarnings("unused")
    public void processOpenAPI(OpenAPI openAPI) {
    }

    // override with any special handling of the JMustache compiler
    @SuppressWarnings("unused")
    public Compiler processCompiler(Compiler compiler) {
        return compiler;
    }

    // override with any special text escaping logic
    @SuppressWarnings("static-method")
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        // remove \t, \n, \r
        // replace \ with \\
        // replace " with \"
        // outter unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t\\n\\r]", " ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));
    }

    /**
     * override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with unsafe characters removed or escaped
     */
    public String escapeUnsafeCharacters(String input) {
        LOGGER.warn("escapeUnsafeCharacters should be overridden in the code generator with proper logic to escape " +
                "unsafe characters");
        // doing nothing by default and code generator should implement
        // the logic to prevent code injection
        // later we'll make this method abstract to make sure
        // code generator implements this method
        return input;
    }

    /**
     * Escape single and/or double quote to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    public String escapeQuotationMark(String input) {
        LOGGER.warn("escapeQuotationMark should be overridden in the code generator with proper logic to escape " +
                "single/double quote");
        return input.replace("\"", "\\\"");
    }

    public Set<String> defaultIncludes() {
        return defaultIncludes;
    }

    public Map<String, String> typeMapping() {
        return typeMapping;
    }

    public Map<String, String> instantiationTypes() {
        return instantiationTypes;
    }

    public Set<String> reservedWords() {
        return reservedWords;
    }

    public Set<String> languageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public Map<String, String> importMapping() {
        return importMapping;
    }

    public String testPackage() {
        return testPackage;
    }

    public String modelPackage() {
        return modelPackage;
    }

    public String apiPackage() {
        return apiPackage;
    }

    public String fileSuffix() {
        return fileSuffix;
    }

    public String templateDir() {
        return templateDir;
    }

    public String embeddedTemplateDir() {
        if (embeddedTemplateDir != null) {
            return embeddedTemplateDir;
        } else {
            return templateDir;
        }
    }

    public String getCommonTemplateDir() {
        return this.commonTemplateDir;
    }

    public void setCommonTemplateDir(String commonTemplateDir) {
        this.commonTemplateDir = commonTemplateDir;
    }

    public Map<String, String> apiDocTemplateFiles() {
        return apiDocTemplateFiles;
    }

    public Map<String, String> modelDocTemplateFiles() {
        return modelDocTemplateFiles;
    }

    public Map<String, String> reservedWordsMappings() {
        return reservedWordsMappings;
    }

    public Map<String, String> apiTestTemplateFiles() {
        return apiTestTemplateFiles;
    }

    public Map<String, String> modelTestTemplateFiles() {
        return modelTestTemplateFiles;
    }

    public Map<String, String> apiTemplateFiles() {
        return apiTemplateFiles;
    }

    public Map<String, String> modelTemplateFiles() {
        return modelTemplateFiles;
    }

    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', '/');
    }

    public String modelFileFolder() {
        return outputFolder + "/" + modelPackage().replace('.', '/');
    }

    public String apiTestFileFolder() {
        return outputFolder + "/" + testPackage().replace('.', '/');
    }

    public String modelTestFileFolder() {
        return outputFolder + "/" + testPackage().replace('.', '/');
    }

    public String apiDocFileFolder() {
        return outputFolder;
    }

    public String modelDocFileFolder() {
        return outputFolder;
    }

    public Map<String, Object> additionalProperties() {
        return additionalProperties;
    }

    public Map<String, Object> vendorExtensions() {
        return vendorExtensions;
    }

    public List<SupportingFile> supportingFiles() {
        return supportingFiles;
    }

    public String outputFolder() {
        return outputFolder;
    }

    public void setOutputDir(String dir) {
        this.outputFolder = dir;
    }

    public String getOutputDir() {
        return outputFolder();
    }

    public String getInputSpec() {
        return inputSpec;
    }

    public void setInputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
    }

    public void setTemplateDir(String templateDir) {
        this.templateDir = templateDir;
    }

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public void setModelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
    }

    public void setModelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public void setSortParamsByRequiredFlag(Boolean sortParamsByRequiredFlag) {
        this.sortParamsByRequiredFlag = sortParamsByRequiredFlag;
    }

    public void setPrependFormOrBodyParameters(Boolean prependFormOrBodyParameters) {
        this.prependFormOrBodyParameters = prependFormOrBodyParameters;
    }

    public void setEnsureUniqueParams(Boolean ensureUniqueParams) {
        this.ensureUniqueParams = ensureUniqueParams;
    }

    public void setAllowUnicodeIdentifiers(Boolean allowUnicodeIdentifiers) {
        this.allowUnicodeIdentifiers = allowUnicodeIdentifiers;
    }

    /**
     * Return the regular expression/JSON schema pattern (http://json-schema.org/latest/json-schema-validation.html#anchor33)
     *
     * @param pattern the pattern (regular expression)
     * @return properly-escaped pattern
     */
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(escapeText(pattern));
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Return the file name of the Api Documentation
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Test";
    }

    /**
     * Return the variable name in the Api
     *
     * @param name the varible name of the Api
     * @return the snake-cased variable name
     */
    public String toApiVarName(String name) {
        return snakeCase(name);
    }

    /**
     * Return the capitalized file name of the model
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelFilename(String name) {
        return initialCaps(name);
    }

    /**
     * Return the capitalized file name of the model test
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelTestFilename(String name) {
        return initialCaps(name) + "Test";
    }

    /**
     * Return the capitalized file name of the model documentation
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelDocFilename(String name) {
        return initialCaps(name);
    }

    /**
     * Return the operation ID (method name)
     *
     * @param operationId operation ID
     * @return the sanitized method name
     */
    @SuppressWarnings("static-method")
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        return operationId;
    }

    /**
     * Return the variable name by removing invalid characters and proper escaping if
     * it's a reserved word.
     *
     * @param name the variable name
     * @return the sanitized variable name
     */
    public String toVarName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else {
            return name;
        }
    }

    /**
     * Return the parameter name by removing invalid characters and proper escaping if
     * it's a reserved word.
     *
     * @param name Codegen property object
     * @return the sanitized parameter name
     */
    public String toParamName(String name) {
        name = removeNonNameElementToCamelCase(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        }
        return name;
    }

    /**
     * Return the parameter name of array of model
     *
     * @param name name of the array model
     * @return the sanitized parameter name
     */
    public String toArrayModelParamName(String name) {
        return toParamName(name);
    }

    /**
     * Return the Enum name (e.g. StatusEnum given 'status')
     *
     * @param property Codegen property
     * @return the Enum name
     */
    @SuppressWarnings("static-method")
    public String toEnumName(CodegenProperty property) {
        return StringUtils.capitalize(property.name) + "Enum";
    }

    /**
     * Return the escaped name of the reserved word
     *
     * @param name the name to be escaped
     * @return the escaped reserved word
     * <p>
     * throws Runtime exception as reserved word is not allowed (default behavior)
     */
    @SuppressWarnings("static-method")
    public String escapeReservedWord(String name) {
        throw new RuntimeException("reserved word " + name + " not allowed");
    }

    /**
     * Return the fully-qualified "Model" name for import
     *
     * @param name the name of the "Model"
     * @return the fully-qualified "Model" name for import
     */
    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "." + name;
        }
    }

    /**
     * Return the fully-qualified "Api" name for import
     *
     * @param name the name of the "Api"
     * @return the fully-qualified "Api" name for import
     */
    public String toApiImport(String name) {
        return apiPackage() + "." + name;
    }

    /**
     * Default constructor.
     * This method will map between OAS type and language-specified type, as well as mapping
     * between OAS type and the corresponding import statement for the language. This will
     * also add some language specified CLI options, if any.
     * returns string presentation of the example path (it's a constructor)
     */
    public DefaultCodegen() {
        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "int",
                        "long",
                        "short",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Void",
                        "Integer",
                        "Long",
                        "Float")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("array", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("List", "List");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("long", "Long");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "String");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "Integer");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("binary", "File");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "UUID");
        //typeMapping.put("BigDecimal", "BigDecimal"); //TODO need the mapping?


        instantiationTypes = new HashMap<String, String>();

        reservedWords = new HashSet<String>();

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "java.util.Map");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Array", "java.util.List");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("List", "java.util.*");
        importMapping.put("Set", "java.util.*");
        importMapping.put("DateTime", "org.joda.time.*");
        importMapping.put("LocalDateTime", "org.joda.time.*");
        importMapping.put("LocalDate", "org.joda.time.*");
        importMapping.put("LocalTime", "org.joda.time.*");

        // we've used the .openapi-generator-ignore approach as
        // suppportingFiles can be cleared by code generator that extends
        // the default codegen, leaving the commented code below for
        // future reference
        //supportingFiles.add(new GlobalSupportingFile("LICENSE", "LICENSE"));

        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENSURE_UNIQUE_PARAMS, CodegenConstants
                .ENSURE_UNIQUE_PARAMS_DESC).defaultValue(Boolean.TRUE.toString()));
        // name formatting options
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, CodegenConstants
                .ALLOW_UNICODE_IDENTIFIERS_DESC).defaultValue(Boolean.FALSE.toString()));
        // option to change the order of form/body parameter
        cliOptions.add(CliOption.newBoolean(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS,
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS_DESC).defaultValue(Boolean.FALSE.toString()));

        // initialize special character mapping
        initalizeSpecialCharacterMapping();
    }

    /**
     * Initialize special character mapping
     */
    protected void initalizeSpecialCharacterMapping() {
        // Initialize special characters
        specialCharReplacements.put("$", "Dollar");
        specialCharReplacements.put("^", "Caret");
        specialCharReplacements.put("|", "Pipe");
        specialCharReplacements.put("=", "Equal");
        specialCharReplacements.put("*", "Star");
        specialCharReplacements.put("-", "Minus");
        specialCharReplacements.put("&", "Ampersand");
        specialCharReplacements.put("%", "Percent");
        specialCharReplacements.put("#", "Hash");
        specialCharReplacements.put("@", "At");
        specialCharReplacements.put("!", "Exclamation");
        specialCharReplacements.put("+", "Plus");
        specialCharReplacements.put(":", "Colon");
        specialCharReplacements.put(">", "Greater_Than");
        specialCharReplacements.put("<", "Less_Than");
        specialCharReplacements.put(".", "Period");
        specialCharReplacements.put("_", "Underscore");
        specialCharReplacements.put("?", "Question_Mark");
        specialCharReplacements.put(",", "Comma");
        specialCharReplacements.put("'", "Quote");
        specialCharReplacements.put("\"", "Double_Quote");
        specialCharReplacements.put("/", "Slash");
        specialCharReplacements.put("\\", "Back_Slash");
        specialCharReplacements.put("(", "Left_Parenthesis");
        specialCharReplacements.put(")", "Right_Parenthesis");
        specialCharReplacements.put("{", "Left_Curly_Bracket");
        specialCharReplacements.put("}", "Right_Curly_Bracket");
        specialCharReplacements.put("[", "Left_Square_Bracket");
        specialCharReplacements.put("]", "Right_Square_Bracket");
        specialCharReplacements.put("~", "Tilde");
        specialCharReplacements.put("`", "Backtick");

        specialCharReplacements.put("<=", "Less_Than_Or_Equal_To");
        specialCharReplacements.put(">=", "Greater_Than_Or_Equal_To");
        specialCharReplacements.put("!=", "Not_Equal");
    }

    /**
     * Return the symbol name of a symbol
     *
     * @param input Symbol (e.g. $)
     * @return Symbol name (e.g. Dollar)
     */
    protected String getSymbolName(String input) {
        return specialCharReplacements.get(input);
    }

    /**
     * Return the example path
     *
     * @param path      the path of the operation
     * @param operation OAS operation object
     * @return string presentation of the example path
     */
    @SuppressWarnings("static-method")
    public String generateExamplePath(String path, Operation operation) {
        StringBuilder sb = new StringBuilder();
        sb.append(path);

        if (operation.getParameters() != null) {
            int count = 0;

            for (Parameter param : operation.getParameters()) {
                if (param instanceof QueryParameter) {
                    StringBuilder paramPart = new StringBuilder();
                    QueryParameter qp = (QueryParameter) param;

                    if (count == 0) {
                        paramPart.append("?");
                    } else {
                        paramPart.append(",");
                    }
                    count += 1;
                    if (!param.getRequired()) {
                        paramPart.append("[");
                    }
                    paramPart.append(param.getName()).append("=");
                    paramPart.append("{");

                    // TODO support for multi, tsv?
                    if (qp.getStyle() != null) {
                        paramPart.append(param.getName()).append("1");
                        if (Parameter.StyleEnum.FORM.equals(qp.getStyle())) {
                            if (qp.getExplode() != null && qp.getExplode()) {
                                paramPart.append(",");
                            } else {
                                paramPart.append("&").append(param.getName()).append("=");
                                paramPart.append(param.getName()).append("2");
                            }
                        } else if (Parameter.StyleEnum.PIPEDELIMITED.equals(qp.getStyle())) {
                            paramPart.append("|");
                        } else if (Parameter.StyleEnum.SPACEDELIMITED.equals(qp.getStyle())) {
                            paramPart.append("%20");
                        } else {
                            LOGGER.warn("query parameter '" + param.getName() + "style not support: " + qp.getStyle());
                        }
                    } else {
                        paramPart.append(param.getName());
                    }

                    paramPart.append("}");
                    if (!param.getRequired()) {
                        paramPart.append("]");
                    }
                    sb.append(paramPart.toString());
                }
            }
        }

        return sb.toString();
    }

    /**
     * Return the instantiation type of the property, especially for map and array
     *
     * @param schema property schema
     * @return string presentation of the instantiation type of the property
     */
    public String toInstantiationType(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            Schema additionalProperties = (Schema) schema.getAdditionalProperties();
            String type = additionalProperties.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties + "\n" //
                        + "\tIn Property: " + schema);
            }
            String inner = getSchemaType(additionalProperties);
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arraySchema = (ArraySchema) schema;
            String inner = getSchemaType(arraySchema.getItems());
            return instantiationTypes.get("array") + "<" + inner + ">";
        } else {
            return null;
        }
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     */
    public void setParameterExampleValue(CodegenParameter codegenParameter) {

        // set the example value
        // if not specified in x-example, generate a default value
        // TODO need to revise how to obtain the example value
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        } else if (Boolean.TRUE.equals(codegenParameter.isBoolean)) {
            codegenParameter.example = "true";
        } else if (Boolean.TRUE.equals(codegenParameter.isLong)) {
            codegenParameter.example = "789";
        } else if (Boolean.TRUE.equals(codegenParameter.isInteger)) {
            codegenParameter.example = "56";
        } else if (Boolean.TRUE.equals(codegenParameter.isFloat)) {
            codegenParameter.example = "3.4";
        } else if (Boolean.TRUE.equals(codegenParameter.isDouble)) {
            codegenParameter.example = "1.2";
        } else if (Boolean.TRUE.equals(codegenParameter.isNumber)) {
            codegenParameter.example = "8.14";
        } else if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            codegenParameter.example = "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            codegenParameter.example = "BYTE_ARRAY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) {
            codegenParameter.example = "/path/to/file.txt";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) {
            codegenParameter.example = "2013-10-20";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) {
            codegenParameter.example = "2013-10-20T19:20:30+01:00";
        } else if (Boolean.TRUE.equals(codegenParameter.isUuid)) {
            codegenParameter.example = "38400000-8cf0-11bd-b23e-10b96e4ef00d";
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.example = codegenParameter.paramName + "_example";
        }

    }

    /**
     * Return the example value of the property
     *
     * @param schema Property schema
     * @return string presentation of the example value of the property
     */
    public String toExampleValue(Schema schema) {
        if (schema.getExample() != null) {
            return schema.getExample().toString();
        }

        if (ModelUtils.isBooleanSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "null";
        } else if (ModelUtils.isNumberSchema(schema)) {
            return "null";
        } else if (ModelUtils.isIntegerSchema(schema)) {
            return "null";
        } else if (ModelUtils.isStringSchema(schema)) {
            return "null";
        } else if (ModelUtils.isObjectSchema(schema)) {
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * Return the default value of the property
     *
     * @param schema Property schema
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        }

        if (ModelUtils.isBooleanSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "null";
        } else if (ModelUtils.isNumberSchema(schema)) {
            return "null";
        } else if (ModelUtils.isIntegerSchema(schema)) {
            return "null";
        } else if (ModelUtils.isStringSchema(schema)) {
            return "null";
        } else if (ModelUtils.isObjectSchema(schema)) {
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * Return the property initialized from a data object
     * Useful for initialization with a plain object in Javascript
     *
     * @param name   Name of the property object
     * @param schema Property schema
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValueWithParam(String name, Schema schema) {
        return " = data." + name + ";";
    }

    /**
     * returns the OpenAPI type for the property. Use getAlias to handle $ref of primitive type
     *
     * @param schema property schema
     * @return string presentation of the type
     **/
    @SuppressWarnings("static-method")
    public String getSchemaType(Schema schema) {
        // TODO better logic to handle compose schema
        if (schema instanceof ComposedSchema) { // composed schema
            ComposedSchema cs = (ComposedSchema) schema;
            if(cs.getAllOf() != null) {
                for (Schema s : cs.getAllOf()) {
                    if (s != null) {
                        // using the first schema defined in allOf
                        schema = s;
                        break;
                    }
                }
            }
        }

        if (StringUtils.isNotBlank(schema.get$ref())) { // reference to another definition/schema
            // get the schema/model name from $ref
            String schemaName = ModelUtils.getSimpleRef(schema.get$ref());
            if (StringUtils.isNotEmpty(schemaName)) {
                return getAlias(schemaName);
            } else {
                LOGGER.warn("Error obtaining the datatype from ref:" + schema.get$ref() + ". Default to 'object'");
                return "object";
            }
        } else { // primitive type or model
            return getAlias(getPrimitiveType(schema));
        }
    }

    /**
     * Return the OAI type (e.g. integer, long, etc) corresponding to a schema.
     * <pre>$ref</pre> is not taken into account by this method.
     *
     * @param schema
     * @return type
     */
    private static String getPrimitiveType(Schema schema) {
        if (schema == null) {
            throw new RuntimeException("schema cannnot be null in getPrimitiveType");
        } else if (ModelUtils.isStringSchema(schema) && "number".equals(schema.getFormat())) {
            // special handle of type: string, format: number
            return "BigDecimal";
        } else if (ModelUtils.isByteArraySchema(schema)) {
            return "ByteArray";
        } else if (ModelUtils.isFileSchema(schema)) {
            return "file";
        } else if (ModelUtils.isBinarySchema(schema)) {
            return SchemaTypeUtil.BINARY_FORMAT;
        } else if (ModelUtils.isBooleanSchema(schema)) {
            return SchemaTypeUtil.BOOLEAN_TYPE;
        } else if (ModelUtils.isDateSchema(schema)) {
            return SchemaTypeUtil.DATE_FORMAT;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "DateTime";
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (schema.getFormat() == null) { // no format defined
                return "number";
            } else if (ModelUtils.isFloatSchema(schema)) {
                return SchemaTypeUtil.FLOAT_FORMAT;
            } else if (ModelUtils.isDoubleSchema(schema)) {
                return SchemaTypeUtil.DOUBLE_FORMAT;
            } else {
                LOGGER.warn("Unknown `format` detected for " + schema.getName() + ": " + schema.getFormat());
            }
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (ModelUtils.isLongSchema(schema)) {
                return "long";
            } else {
                return schema.getType(); // integer
            }
        } else if (ModelUtils.isMapSchema(schema)) {
            return "map";
        } else if (ModelUtils.isArraySchema(schema)) {
            return "array";
        } else if (ModelUtils.isUUIDSchema(schema)) {
            return "UUID";
        } else if (ModelUtils.isStringSchema(schema)) {
            return "string";
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) { // having property implies it's a model
            return "object";
        } else if (StringUtils.isNotEmpty(schema.getType())) {
            LOGGER.warn("Unknown type found in the schema: " + schema.getType());
            return schema.getType();
        }

        return "object";
    }

    /**
     * Return the snake-case of the string
     *
     * @param name string to be snake-cased
     * @return snake-cased string
     */
    @SuppressWarnings("static-method")
    public String snakeCase(String name) {
        return (name.length() > 0) ? (Character.toLowerCase(name.charAt(0)) + name.substring(1)) : "";
    }

    /**
     * Capitalize the string
     *
     * @param name string to be capitalized
     * @return capitalized string
     */
    @SuppressWarnings("static-method")
    public String initialCaps(String name) {
        return StringUtils.capitalize(name);
    }

    /**
     * Output the type declaration of a given name
     *
     * @param name name
     * @return a string presentation of the type
     */
    @SuppressWarnings("static-method")
    public String getTypeDeclaration(String name) {
        return name;
    }

    /**
     * Output the type declaration of the property
     *
     * @param schema property schema
     * @return a string presentation of the property type
     */
    public String getTypeDeclaration(Schema schema) {
        String oasType = getSchemaType(schema);
        if (typeMapping.containsKey(oasType)) {
            return typeMapping.get(oasType);
        }
        return oasType;
    }

    /**
     * Determine the type alias for the given type if it exists. This feature
     * was original developed for Java because the language does not have a aliasing
     * mechanism of its own but later extends to handle other languages
     *
     * @param name The type name.
     * @return The alias of the given type, if it exists. If there is no alias
     * for this type, then returns the input type name.
     */
    public String getAlias(String name) {
        if (typeAliases != null && typeAliases.containsKey(name)) {
            return typeAliases.get(name);
        }
        return name;
    }

    /**
     * Output the Getter name for boolean property, e.g. getActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toBooleanGetter(String name) {
        return "get" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the Getter name, e.g. getSize
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toGetter(String name) {
        return "get" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the Getter name, e.g. getSize
     *
     * @param name the name of the property
     * @return setter name based on naming convention
     */
    public String toSetter(String name) {
        return "set" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the API (class) name (capitalized) ending with "Api"
     * Return DefaultApi if name is empty
     *
     * @param name the name of the Api
     * @return capitalized Api name ending with "Api"
     */
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return initialCaps(name) + "Api";
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    public String toModelName(final String name) {
        return initialCaps(modelNamePrefix + name + modelNameSuffix);
    }

    /**
     * Convert OAS Model object to Codegen Model object
     *
     * @param name           the name of the model
     * @param schema         OAS Model object
     * @param allDefinitions a map of all OAS models from the spec
     * @return Codegen Model object
     */
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allDefinitions) {
        if (typeAliases == null) {
            // Only do this once during first call
            typeAliases = getAllAliases(allDefinitions);
        }

        CodegenModel m = CodegenModelFactory.newInstance(CodegenModelType.MODEL);

        if (reservedWords.contains(name)) {
            m.name = escapeReservedWord(name);
        } else {
            m.name = name;
        }
        m.title = escapeText(schema.getTitle());
        m.description = escapeText(schema.getDescription());
        m.unescapedDescription = schema.getDescription();
        m.classname = toModelName(name);
        m.classVarName = toVarName(name);
        m.classFilename = toModelFilename(name);
        m.modelJson = Json.pretty(schema);
        m.externalDocumentation = schema.getExternalDocs();
        if (schema.getExtensions() != null && !schema.getExtensions().isEmpty()) {
            m.getVendorExtensions().putAll(schema.getExtensions());
        }
        m.isAlias = typeAliases.containsKey(name);
        m.discriminator = schema.getDiscriminator();

        if (schema.getXml() != null) {
            m.xmlPrefix = schema.getXml().getPrefix();
            m.xmlNamespace = schema.getXml().getNamespace();
            m.xmlName = schema.getXml().getName();
        }

        if (ModelUtils.isArraySchema(schema)) {
            m.isArrayModel = true;
            m.arrayModelType = fromProperty(name, schema).complexType;
            addParentContainer(m, name, schema);
        } else if (schema instanceof ComposedSchema) {
            final ComposedSchema composed = (ComposedSchema) schema;
            Map<String, Schema> properties = new LinkedHashMap<String, Schema>();
            List<String> required = new ArrayList<String>();
            Map<String, Schema> allProperties;
            List<String> allRequired;

            if (supportsInheritance || supportsMixins) {
                allProperties = new LinkedHashMap<String, Schema>();
                allRequired = new ArrayList<String>();
                m.allVars = new ArrayList<CodegenProperty>();
                if (composed.getAllOf() != null) {
                    int modelImplCnt = 0; // only one inline object allowed in a ComposedModel
                    for (Schema innerModel : composed.getAllOf()) {
                        if (m.discriminator == null) {
                            m.discriminator = schema.getDiscriminator();
                        }
                        if (innerModel.getXml() != null) {
                            m.xmlPrefix = innerModel.getXml().getPrefix();
                            m.xmlNamespace = innerModel.getXml().getNamespace();
                            m.xmlName = innerModel.getXml().getName();
                        }
                        if (modelImplCnt++ > 1) {
                            LOGGER.warn("More than one inline schema specified in allOf:. Only the first one is recognized. All others are ignored.");
                            break; // only one ModelImpl with discriminator allowed in allOf
                        }
                    }
                }
            } else {
                allProperties = null;
                allRequired = null;
            }
            // parent model
            final String parentName = getParentName(composed, allDefinitions);
            final Schema parent = StringUtils.isBlank(parentName) || allDefinitions == null ? null : allDefinitions.get(parentName);

            List<Schema> interfaces = getInterfaces(composed);

            // interfaces (intermediate models)
            if (interfaces != null) {
                if (m.interfaces == null)
                    m.interfaces = new ArrayList<String>();

                for (Schema interfaceSchema : interfaces) {
                    if (StringUtils.isBlank(interfaceSchema.get$ref())) {
                        continue;
                    }
                    Schema refSchema = null;
                    String ref = ModelUtils.getSimpleRef(interfaceSchema.get$ref());
                    if (allDefinitions != null) {
                        refSchema = allDefinitions.get(ref);
                    }
                    final String modelName = toModelName(ref);
                    m.interfaces.add(modelName);
                    addImport(m, modelName);
                    if (allDefinitions != null && refSchema != null) {
                        if (!supportsMixins && !supportsInheritance) {
                            addProperties(properties, required, refSchema, allDefinitions);
                        }
                        if (supportsInheritance) {
                            addProperties(allProperties, allRequired, refSchema, allDefinitions);
                        }
                    }
                }
            }

            if (parent != null) {
                m.parentSchema = parentName;
                m.parent = toModelName(parentName);
                addImport(m, m.parent);
                if (allDefinitions != null && !allDefinitions.isEmpty()) {
                    if (supportsInheritance) {
                        addProperties(allProperties, allRequired, parent, allDefinitions);
                    } else {
                        addProperties(properties, required, parent, allDefinitions);
                    }
                }
            }

            // child model (properties owned by the model itself)
            Schema child = null;
            if (composed.getAllOf() != null && !composed.getAllOf().isEmpty()) {
                for (Schema component : composed.getAllOf()) {
                    if (component.get$ref() == null) {
                        child = component;
                    }
                }
            }
            if (child != null) {
                addProperties(properties, required, child, allDefinitions);
                if (supportsInheritance) {
                    addProperties(allProperties, allRequired, child, allDefinitions);
                }
            }
            addVars(m, properties, required, allProperties, allRequired);
            // TODO
            //} else if (schema instanceof RefModel) {
        } else {
            m.dataType = getSchemaType(schema);
            if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
                m.isEnum = true;
                // comment out below as allowableValues is not set in post processing model enum
                m.allowableValues = new HashMap<String, Object>();
                m.allowableValues.put("values", schema.getEnum());
            }
            if (ModelUtils.isMapSchema(schema)) {
                addAdditionPropertiesToCodeGenModel(m, schema);
            }
            addVars(m, schema.getProperties(), schema.getRequired());
        }

        if (m.vars != null) {
            for (CodegenProperty prop : m.vars) {
                postProcessModelProperty(m, prop);
            }
        }
        LOGGER.debug("debugging fromModel return: " + m);

        return m;
    }

    /**
     * Recursively look for a discriminator in the interface tree
     *
     * @param schema         composed schema
     * @param allDefinitions all schema defintion
     * @return true if it's a discriminator
     */
    private boolean isDiscriminatorInInterfaceTree(ComposedSchema schema, Map<String, Schema> allDefinitions) {
        if (schema == null || allDefinitions == null || allDefinitions.isEmpty()) {
            return false;
        }
        if (schema.getDiscriminator() != null) {
            return true;
        }
        final List<Schema> interfaces = getInterfaces(schema);
        if (interfaces == null) {
            return false;
        }
        for (Schema interfaceSchema : interfaces) {
            if (interfaceSchema.getDiscriminator() != null) {
                return true;
            }
            // TODO revise the logic below
            if (interfaceSchema instanceof ComposedSchema) {
                return isDiscriminatorInInterfaceTree((ComposedSchema) interfaceSchema, allDefinitions);
            }
        }
        return false;
    }

    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        addParentContainer(codegenModel, codegenModel.name, schema);
    }

    protected void addProperties(Map<String, Schema> properties, List<String> required, Schema schema, Map<String, Schema> allSchemas) {
        if (schema instanceof ComposedSchema) {
            ComposedSchema composedSchema = (ComposedSchema) schema;
            if (composedSchema.getAllOf() == null) {
                return;
            }

            for (Schema component : composedSchema.getAllOf()) {
                addProperties(properties, required, component, allSchemas);
            }
            return;
        }

        if (StringUtils.isNotBlank(schema.get$ref())) {
            Schema interfaceSchema = allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref()));
            addProperties(properties, required, interfaceSchema, allSchemas);
            return;
        }
        if (schema.getProperties() != null) {
            properties.putAll(schema.getProperties());
        }
        if (schema.getRequired() != null) {
            required.addAll(schema.getRequired());
        }
    }

    /**
     * Camelize the method name of the getter and setter
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }
        return camelize(toVarName(name));
    }

    /**
     * Convert OAS Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    public CodegenProperty fromProperty(String name, Schema p) {
        if (p == null) {
            LOGGER.error("Unexpected missing property for name " + name);
            return null;
        }
        LOGGER.debug("debugging fromProperty for " + name + " : " + p);

        CodegenProperty property = CodegenModelFactory.newInstance(CodegenModelType.PROPERTY);
        property.name = toVarName(name);
        property.baseName = name;
        property.nameInCamelCase = camelize(property.name, false);
        property.nameInSnakeCase = CaseFormat.UPPER_CAMEL.to(CaseFormat.UPPER_UNDERSCORE, property.nameInCamelCase);
        property.description = escapeText(p.getDescription());
        property.unescapedDescription = p.getDescription();
        property.title = p.getTitle();
        property.getter = toGetter(name);
        property.setter = toSetter(name);
        String example = toExampleValue(p);
        if (!"null".equals(example)) {
            property.example = example;
        }
        property.defaultValue = toDefaultValue(p);
        property.defaultValueWithParam = toDefaultValueWithParam(name, p);
        property.jsonSchema = Json.pretty(p);
        if (p.getReadOnly() != null) {
            property.isReadOnly = p.getReadOnly();
        }
        if (p.getXml() != null) {
            if (p.getXml().getAttribute() != null) {
                property.isXmlAttribute = p.getXml().getAttribute();
            }
            property.xmlPrefix = p.getXml().getPrefix();
            property.xmlName = p.getXml().getName();
            property.xmlNamespace = p.getXml().getNamespace();
        }
        if (p.getExtensions() != null && !p.getExtensions().isEmpty()) {
            property.getVendorExtensions().putAll(p.getExtensions());
        }

        String type = getSchemaType(p);
        if (ModelUtils.isIntegerSchema(p)) { // integer type
            property.isNumeric = Boolean.TRUE;
            if (SchemaTypeUtil.INTEGER64_FORMAT.equals(p.getFormat())) { // int64/long format
                property.isLong = Boolean.TRUE;
            } else { // int32 format
                property.isInteger = Boolean.TRUE;
            }

            if (p.getMinimum() != null) {
                property.minimum = String.valueOf(p.getMinimum().longValue());
            }
            if (p.getMaximum() != null) {
                property.maximum = String.valueOf(p.getMaximum().longValue());
            }
            if (p.getExclusiveMinimum() != null) {
                property.exclusiveMinimum = p.getExclusiveMinimum();
            }
            if (p.getExclusiveMaximum() != null) {
                property.exclusiveMaximum = p.getExclusiveMaximum();
            }

            // check if any validation rule defined
            // exclusive* are noop without corresponding min/max
            if (property.minimum != null || property.maximum != null)
                property.hasValidation = true;

            // legacy support
            Map<String, Object> allowableValues = new HashMap<String, Object>();

            if (p.getEnum() != null) {
                List<Object> _enum = p.getEnum();
                property._enum = new ArrayList<String>();
                for (Object i : _enum) {
                    property._enum.add(String.valueOf(i));
                }
                property.isEnum = true;
                allowableValues.put("values", _enum);
            }

            if (allowableValues.size() > 0) {
                property.allowableValues = allowableValues;
            }
        } else if (ModelUtils.isBooleanSchema(p)) { // boolean type
            property.isBoolean = true;
            property.getter = toBooleanGetter(name);
        } else if (ModelUtils.isDateSchema(p)) { // date format
            property.isString = false; // for backward compatibility with 2.x
            property.isDate = true;
            if (p.getEnum() != null) {
                List<String> _enum = p.getEnum();
                property._enum = new ArrayList<String>();
                for (String i : _enum) {
                    property._enum.add(i);
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        } else if (ModelUtils.isDateTimeSchema(p)) { // date-time format
            property.isString = false; // for backward compatibility with 2.x
            property.isDateTime = true;
            if (p.getEnum() != null) {
                List<String> _enum = p.getEnum();
                property._enum = new ArrayList<String>();
                for (String i : _enum) {
                    property._enum.add(i);
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (ModelUtils.isByteArraySchema(p)) {
                property.isByteArray = true;
            } else if (ModelUtils.isBinarySchema(p)) {
                property.isBinary = true;
                property.isFile = true; // file = binary in OAS3
            } else if (ModelUtils.isFileSchema(p)) {
                property.isFile = true;
            } else if (ModelUtils.isUUIDSchema(p)) {
                // keep isString to true to make it backward compatible
                property.isString = true;
                property.isUuid = true;
            } else {
                property.isString = true;
            }

            property.maxLength = p.getMaxLength();
            property.minLength = p.getMinLength();
            property.pattern = toRegularExpression(p.getPattern());

            // check if any validation rule defined
            if (property.pattern != null || property.minLength != null || property.maxLength != null)
                property.hasValidation = true;

            if (p.getEnum() != null) {
                List<String> _enum = p.getEnum();
                property._enum = _enum;
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            property.isNumeric = Boolean.TRUE;
            if (ModelUtils.isFloatSchema(p)) { // float
                property.isFloat = Boolean.TRUE;
            } else if (ModelUtils.isDoubleSchema(p)) { // double
                property.isDouble = Boolean.TRUE;
            } else { // type is number and without format
                property.isNumber = Boolean.TRUE;
            }

            if (p.getMinimum() != null) {
                property.minimum = String.valueOf(p.getMinimum());
            }
            if (p.getMaximum() != null) {
                property.maximum = String.valueOf(p.getMaximum());
            }
            if (p.getExclusiveMinimum() != null) {
                property.exclusiveMinimum = p.getExclusiveMinimum();
            }
            if (p.getExclusiveMaximum() != null) {
                property.exclusiveMaximum = p.getExclusiveMaximum();
            }

            // check if any validation rule defined
            // exclusive* are noop without corresponding min/max
            if (property.minimum != null || property.maximum != null)
                property.hasValidation = true;

            if (p.getEnum() != null && !p.getEnum().isEmpty()) {
                List<Object> _enum = p.getEnum();
                property._enum = new ArrayList<String>();
                for (Object i : _enum) {
                    property._enum.add(String.valueOf(i));
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }

        property.dataType = getTypeDeclaration(p);
        property.dataFormat = p.getFormat();
        property.baseType = getSchemaType(p);

        // this can cause issues for clients which don't support enums
        if (property.isEnum) {
            property.datatypeWithEnum = toEnumName(property);
            property.enumName = toEnumName(property);
        } else {
            property.datatypeWithEnum = property.dataType;
        }

        if (ModelUtils.isArraySchema(p)) {
            property.isContainer = true;
            property.isListContainer = true;
            property.containerType = "array";
            property.baseType = getSchemaType(p);
            if (p.getXml() != null) {
                property.isXmlWrapped = p.getXml().getWrapped() == null ? false : p.getXml().getWrapped();
                property.xmlPrefix = p.getXml().getPrefix();
                property.xmlNamespace = p.getXml().getNamespace();
                property.xmlName = p.getXml().getName();
            }

            // handle inner property
            property.maxItems = p.getMaxItems();
            property.minItems = p.getMinItems();
            String itemName = null;
            if (p.getExtensions() != null && p.getExtensions().get("x-item-name") != null) {
                itemName = p.getExtensions().get("x-item-name").toString();
            }
            if (itemName == null) {
                itemName = property.name;
            }
            CodegenProperty cp = fromProperty(itemName, ((ArraySchema) p).getItems());
            updatePropertyForArray(property, cp);
        } else if (ModelUtils.isMapSchema(p)) {
            property.isContainer = true;
            property.isMapContainer = true;
            property.containerType = "map";
            property.baseType = getSchemaType(p);
            property.minItems = p.getMinProperties();
            property.maxItems = p.getMaxProperties();

            // handle inner property
            CodegenProperty cp = fromProperty("inner", (Schema) p.getAdditionalProperties());
            updatePropertyForMap(property, cp);
        } else { // model
            // TODO revise the logic below
            //if (StringUtils.isNotBlank(p.get$ref())) {
            //    property.baseType = getSimpleRef(p.get$ref());
            //}
            // --END of revision

            setNonArrayMapProperty(property, type);
        }

        LOGGER.debug("debugging from property return: " + property);
        return property;
    }

    /**
     * Update property for array(list) container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    protected void updatePropertyForArray(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid array property " + Json.pretty(property));
            return;
        }
        property.dataFormat = innerProperty.dataFormat;
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        property.mostInnerItems = getMostInnerItems(innerProperty);
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            property.isEnum = true;
            // update datatypeWithEnum and default value for array
            // e.g. List<string> => List<StatusEnum>
            updateDataTypeWithEnumForArray(property);
            // set allowable values to enum values (including array/map of enum)
            property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    /**
     * Update property for map container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    protected void updatePropertyForMap(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid map property " + Json.pretty(property));
            return;
        }
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        property.mostInnerItems = getMostInnerItems(innerProperty);
        property.dataFormat = innerProperty.dataFormat;
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            property.isEnum = true;
            // update datatypeWithEnum and default value for map
            // e.g. Dictionary<string, string> => Dictionary<string, StatusEnum>
            updateDataTypeWithEnumForMap(property);
            // set allowable values to enum values (including array/map of enum)
            property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    /**
     * Update property for map container
     *
     * @param property Codegen property
     * @return True if the inner most type is enum
     */
    protected Boolean isPropertyInnerMostEnum(CodegenProperty property) {
        CodegenProperty currentProperty = getMostInnerItems(property);

        return currentProperty == null ? false : currentProperty.isEnum;
    }

    protected CodegenProperty getMostInnerItems(CodegenProperty property) {
        CodegenProperty currentProperty = property;
        while (currentProperty != null && (Boolean.TRUE.equals(currentProperty.isMapContainer)
                || Boolean.TRUE.equals(currentProperty.isListContainer))) {
            currentProperty = currentProperty.items;
        }
        return currentProperty;
    }

    protected Map<String, Object> getInnerEnumAllowableValues(CodegenProperty property) {
        CodegenProperty currentProperty = getMostInnerItems(property);

        return currentProperty == null ? new HashMap<String, Object>() : currentProperty.allowableValues;
    }


    /**
     * Update datatypeWithEnum for array container
     *
     * @param property Codegen property
     */
    protected void updateDataTypeWithEnumForArray(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(baseItem.baseType, toEnumName(baseItem));

            // naming the enum with respect to the language enum naming convention
            // e.g. remove [], {} from array/map of enum
            property.enumName = toEnumName(property);

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(baseItem.baseType, toEnumName(baseItem));
            }

            updateCodegenPropertyEnum(property);
        }
    }

    /**
     * Update datatypeWithEnum for map container
     *
     * @param property Codegen property
     */
    protected void updateDataTypeWithEnumForMap(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }

        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(", " + baseItem.baseType, ", " + toEnumName(baseItem));

            // naming the enum with respect to the language enum naming convention
            // e.g. remove [], {} from array/map of enum
            property.enumName = toEnumName(property);

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(", " + property.items.baseType, ", " + toEnumName(property.items));
            }
        }
    }

    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        property.isNotContainer = true;
        if (languageSpecificPrimitives().contains(type)) {
            property.isPrimitiveType = true;
        } else {
            property.complexType = property.baseType;
        }
    }

    /**
     * Override with any special handling of response codes
     *
     * @param responses OAS Operation's responses
     * @return default method response or <tt>null</tt> if not found
     */
    protected ApiResponse findMethodResponse(ApiResponses responses) {
        String code = null;
        for (String responseCode : responses.keySet()) {
            if (responseCode.startsWith("2") || responseCode.equals("default")) {
                if (code == null || code.compareTo(responseCode) > 0) {
                    code = responseCode;
                }
            }
        }
        if (code == null) {
            return null;
        }
        return responses.get(code);
    }

    /**
     * Convert OAS Operation object to Codegen Operation object (without providing a OAS object)
     *
     * @param path       the path of the operation
     * @param httpMethod HTTP method
     * @param operation  OAS operation object
     * @param schemas    a map of OAS models
     * @return Codegen Operation object
     */
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Schema> schemas) {
        return fromOperation(path, httpMethod, operation, schemas, null);
    }

    /**
     * Convert OAS Operation object to Codegen Operation object
     *
     * @param path       the path of the operation
     * @param httpMethod HTTP method
     * @param operation  OAS operation object
     * @param schemas    a map of OAS models
     * @param openAPI    a OAS object representing the spec
     * @return Codegen Operation object
     */
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          Map<String, Schema> schemas,
                                          OpenAPI openAPI) {
        LOGGER.debug("fromOperation => operation: " + operation);
        CodegenOperation op = CodegenModelFactory.newInstance(CodegenModelType.OPERATION);
        Set<String> imports = new HashSet<String>();
        if (operation.getExtensions() != null && !operation.getExtensions().isEmpty()) {
            op.vendorExtensions.putAll(operation.getExtensions());
        }

        if (operation == null)
            throw new RuntimeException("operation cannnot be null in fromOperation");

        // store the original operationId for plug-in
        op.operationIdOriginal = operation.getOperationId();

        String operationId = getOrGenerateOperationId(operation, path, httpMethod);
        // remove prefix in operationId
        if (removeOperationIdPrefix) {
            int offset = operationId.indexOf('_');
            if (offset > -1) {
                operationId = operationId.substring(offset + 1);
            }
        }
        operationId = removeNonNameElementToCamelCase(operationId);
        op.path = path;
        op.operationId = toOperationId(operationId);
        op.summary = escapeText(operation.getSummary());
        op.unescapedNotes = operation.getDescription();
        op.notes = escapeText(operation.getDescription());
        op.hasConsumes = false;
        op.hasProduces = false;
        if (operation.getDeprecated() != null) {
            op.isDeprecated = operation.getDeprecated();
        }

        addConsumesInfo(openAPI, operation, op);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());
            for (String key : operation.getResponses().keySet()) {
                ApiResponse response = operation.getResponses().get(key);
                addProducesInfo(openAPI, response, op);
                CodegenResponse r = fromResponse(key, response);
                r.hasMore = true;
                if (r.baseType != null &&
                        !defaultIncludes.contains(r.baseType) &&
                        !languageSpecificPrimitives.contains(r.baseType)) {
                    imports.add(r.baseType);
                }
                r.isDefault = response == methodResponse;
                op.responses.add(r);
                if (Boolean.TRUE.equals(r.isBinary) && Boolean.TRUE.equals(r.isDefault)) {
                    op.isResponseBinary = Boolean.TRUE;
                }
                if (Boolean.TRUE.equals(r.isFile) && Boolean.TRUE.equals(r.isDefault)) {
                    op.isResponseFile = Boolean.TRUE;
                }
            }
            op.responses.get(op.responses.size() - 1).hasMore = false;

            if (methodResponse != null) {
                final Schema responseSchema = ModelUtils.getSchemaFromResponse(methodResponse);
                if (responseSchema != null) {
                    CodegenProperty cm = fromProperty("response", responseSchema);

                    if (ModelUtils.isArraySchema(responseSchema)) {
                        ArraySchema as = (ArraySchema) responseSchema;
                        CodegenProperty innerProperty = fromProperty("response", as.getItems());
                        op.returnBaseType = innerProperty.baseType;
                    } else if (ModelUtils.isMapSchema(responseSchema)) {
                        CodegenProperty innerProperty = fromProperty("response", (Schema) responseSchema.getAdditionalProperties());
                        op.returnBaseType = innerProperty.baseType;
                    } else {
                        if (cm.complexType != null) {
                            op.returnBaseType = cm.complexType;
                        } else {
                            op.returnBaseType = cm.baseType;
                        }
                    }

                    // generate examples
                    op.examples = new ExampleGenerator(schemas, openAPI).generateFromResponseSchema(responseSchema, getProducesInfo(openAPI, operation));
                    op.defaultResponse = toDefaultValue(responseSchema);
                    op.returnType = cm.dataType;
                    op.hasReference = schemas != null && schemas.containsKey(op.returnBaseType);

                    // lookup discriminator
                    if (schemas != null) {
                        Schema schema = schemas.get(op.returnBaseType);
                        if (schema != null) {
                            CodegenModel cmod = fromModel(op.returnBaseType, schema, schemas);
                            op.discriminator = cmod.discriminator;
                        }
                    }

                    if (cm.isContainer) {
                        op.returnContainer = cm.containerType;
                        if ("map".equals(cm.containerType)) {
                            op.isMapContainer = true;
                        } else if ("list".equalsIgnoreCase(cm.containerType)) {
                            op.isListContainer = true;
                        } else if ("array".equalsIgnoreCase(cm.containerType)) {
                            op.isListContainer = true;
                        }
                    } else {
                        op.returnSimpleType = true;
                    }
                    if (languageSpecificPrimitives().contains(op.returnBaseType) || op.returnBaseType == null) {
                        op.returnTypeIsPrimitive = true;
                    }
                }
                addHeaders(methodResponse, op.responseHeaders);
            }
        }

        List<Parameter> parameters = operation.getParameters();
        List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> requiredParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> optionalParams = new ArrayList<CodegenParameter>();

        CodegenParameter bodyParam = null;
        RequestBody requestBody = operation.getRequestBody();
        if (requestBody != null) {
            if ("application/x-www-form-urlencoded".equalsIgnoreCase(getContentType(requestBody)) ||
                    "multipart/form-data".equalsIgnoreCase(getContentType(requestBody))) {
                // process form parameters
                formParams = fromRequestBodyToFormParameters(requestBody, schemas, imports);
                for (CodegenParameter cp : formParams) {
                    postProcessParameter(cp);
                }
                // add form parameters to the beginning of all parameter list
                if (prependFormOrBodyParameters) {
                    for (CodegenParameter cp : formParams) {
                        allParams.add(cp.copy());
                    }
                }
            } else {
                // process body parameter
                requestBody = ModelUtils.getReferencedRequestBody(openAPI, requestBody);

                String bodyParameterName = "";
                if (op.vendorExtensions != null && op.vendorExtensions.containsKey("x-codegen-request-body-name")) {
                    bodyParameterName = (String) op.vendorExtensions.get("x-codegen-request-body-name");
                }
                bodyParam = fromRequestBody(requestBody, schemas, imports, bodyParameterName);
                bodyParam.description = requestBody.getDescription();
                postProcessParameter(bodyParam);

                bodyParams.add(bodyParam);

                if (prependFormOrBodyParameters) {
                    allParams.add(bodyParam);
                }

                // add example
                if (schemas != null) {
                    op.requestBodyExamples = new ExampleGenerator(schemas, openAPI).generate(null, new ArrayList<String>(getConsumesInfo(openAPI, operation)), bodyParam.baseType);
                }
            }
        }

        if (parameters != null) {
            for (Parameter param : parameters) {
                if (StringUtils.isNotBlank(param.get$ref())) {
                    param = getParameterFromRef(param.get$ref(), openAPI);
                }

                CodegenParameter p = fromParameter(param, imports);

                allParams.add(p);

                if (param instanceof QueryParameter || "query".equalsIgnoreCase(param.getIn())) {
                    queryParams.add(p.copy());
                } else if (param instanceof PathParameter || "path".equalsIgnoreCase(param.getIn())) {
                    pathParams.add(p.copy());
                } else if (param instanceof HeaderParameter || "header".equalsIgnoreCase(param.getIn())) {
                    headerParams.add(p.copy());
                } else if (param instanceof CookieParameter || "cookie".equalsIgnoreCase(param.getIn())) {
                    cookieParams.add(p.copy());
                } else {
                    LOGGER.warn("Unknown parameter type " + p.baseType + " for " + p.baseName);
                }

            }
        }

        // add form/body parameter (if any) to the end of all parameter list
        if (!prependFormOrBodyParameters) {
            for (CodegenParameter cp : formParams) {
                allParams.add(cp.copy());
            }

            for (CodegenParameter cp : bodyParams) {
                allParams.add(cp.copy());
            }
        }

        // ensure unique parameter name
        for (CodegenParameter cp : allParams) {
            if (ensureUniqueParams) {
                if (isParameterNameUnique(cp, allParams)) {
                    continue;
                } else {
                    cp.paramName = generateNextName(cp.paramName);
                }
            }
        }

        // create optional, required parameters
        for (CodegenParameter cp : allParams) {
            if (cp.required) { //required parameters
                requiredParams.add(cp.copy());
            } else { // optional parameters
                optionalParams.add(cp.copy());
                op.hasOptionalParams = true;
            }
        }

        // add imports to operation import tag
        for (String i : imports) {
            if (needToImport(i)) {
                op.imports.add(i);
            }
        }

        op.bodyParam = bodyParam;
        op.httpMethod = httpMethod.toUpperCase();

        // move "required" parameters in front of "optional" parameters
        if (sortParamsByRequiredFlag) {
            Collections.sort(allParams, new Comparator<CodegenParameter>() {
                @Override
                public int compare(CodegenParameter one, CodegenParameter another) {
                    if (one.required == another.required) return 0;
                    else if (one.required) return -1;
                    else return 1;
                }
            });
        }

        op.allParams = addHasMore(allParams);
        op.bodyParams = addHasMore(bodyParams);
        op.pathParams = addHasMore(pathParams);
        op.queryParams = addHasMore(queryParams);
        op.headerParams = addHasMore(headerParams);
        op.cookieParams = addHasMore(cookieParams);
        op.formParams = addHasMore(formParams);
        op.requiredParams = addHasMore(requiredParams);
        op.optionalParams = addHasMore(optionalParams);
        op.externalDocs = operation.getExternalDocs();
        // legacy support
        op.nickname = op.operationId;

        if (op.allParams.size() > 0) {
            op.hasParams = true;
        }
        op.hasRequiredParams = op.requiredParams.size() > 0;

        // set Restful Flag
        op.isRestfulShow = op.isRestfulShow();
        op.isRestfulIndex = op.isRestfulIndex();
        op.isRestfulCreate = op.isRestfulCreate();
        op.isRestfulUpdate = op.isRestfulUpdate();
        op.isRestfulDestroy = op.isRestfulDestroy();
        op.isRestful = op.isRestful();

        return op;
    }

    public boolean isParameterNameUnique(CodegenParameter p, List<CodegenParameter> parameters) {
        for (CodegenParameter parameter : parameters) {
            if (System.identityHashCode(p) == System.identityHashCode(parameter)) {
                continue; // skip itself
            }

            if (p.paramName.equals(parameter.paramName)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Convert OAS Response object to Codegen Response object
     *
     * @param responseCode HTTP response code
     * @param response     OAS Response object
     * @return Codegen Response object
     */
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        CodegenResponse r = CodegenModelFactory.newInstance(CodegenModelType.RESPONSE);
        if ("default".equals(responseCode)) {
            r.code = "0";
        } else {
            r.code = responseCode;
        }
        final Schema responseSchema = ModelUtils.getSchemaFromResponse(response);
        r.schema = responseSchema;
        r.message = escapeText(response.getDescription());
        // TODO need to revise and test examples in responses
        // ApiResponse does not support examples at the moment
        //r.examples = toExamples(response.getExamples());
        r.jsonSchema = Json.pretty(response);
        if (response.getExtensions() != null && !response.getExtensions().isEmpty()) {
            r.vendorExtensions.putAll(response.getExtensions());
        }
        addHeaders(response, r.headers);
        r.hasHeaders = !r.headers.isEmpty();

        if (r.schema != null) {
            CodegenProperty cp = fromProperty("response", responseSchema);

            if (ModelUtils.isArraySchema(responseSchema)) {
                ArraySchema as = (ArraySchema) responseSchema;
                CodegenProperty innerProperty = fromProperty("response", as.getItems());
                CodegenProperty innerCp = innerProperty;
                while (innerCp != null) {
                    r.baseType = innerCp.baseType;
                    innerCp = innerCp.items;
                }
            } else {
                if (cp.complexType != null) {
                    r.baseType = cp.complexType;
                } else {
                    r.baseType = cp.baseType;
                }
            }

            r.dataType = cp.dataType;

            if (Boolean.TRUE.equals(cp.isString) && Boolean.TRUE.equals(cp.isUuid)) {
                r.isUuid = true;
            } else if (Boolean.TRUE.equals(cp.isByteArray)) {
                r.isByteArray = true;
            } else if (Boolean.TRUE.equals(cp.isString)) {
                r.isString = true;
            } else if (Boolean.TRUE.equals(cp.isBoolean)) {
                r.isBoolean = true;
            } else if (Boolean.TRUE.equals(cp.isLong)) {
                r.isLong = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isInteger)) {
                r.isInteger = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isNumber)) {
                r.isNumber = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isDouble)) {
                r.isDouble = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isFloat)) {
                r.isFloat = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isBinary)) {
                r.isFile = true; // file = binary in OAS3
                r.isBinary = true;
            } else if (Boolean.TRUE.equals(cp.isFile)) {
                r.isFile = true;
            } else if (Boolean.TRUE.equals(cp.isDate)) {
                r.isDate = true;
            } else if (Boolean.TRUE.equals(cp.isDateTime)) {
                r.isDateTime = true;
            } else {
                LOGGER.debug("Property type is not primitive: " + cp.dataType);
            }

            if (cp.isContainer) {
                r.simpleType = false;
                r.containerType = cp.containerType;
                r.isMapContainer = "map".equals(cp.containerType);
                r.isListContainer = "list".equalsIgnoreCase(cp.containerType) || "array".equalsIgnoreCase(cp.containerType);
            } else {
                r.simpleType = true;
            }

            r.primitiveType = (r.baseType == null || languageSpecificPrimitives().contains(r.baseType));
        }

        if (r.baseType == null) {
            r.isMapContainer = false;
            r.isListContainer = false;
            r.primitiveType = true;
            r.simpleType = true;
        }

        return r;
    }

    /**
     * Convert OAS Parameter object to Codegen Parameter object
     *
     * @param parameter OAS parameter object
     * @param imports   set of imports for library/package/module
     * @return Codegen Parameter object
     */
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegenParameter.baseName = parameter.getName();
        codegenParameter.description = escapeText(parameter.getDescription());
        codegenParameter.unescapedDescription = parameter.getDescription();
        if (parameter.getRequired() != null) {
            codegenParameter.required = parameter.getRequired();
        }
        codegenParameter.jsonSchema = Json.pretty(parameter);

        if (System.getProperty("debugParser") != null) {
            LOGGER.info("working on Parameter " + parameter.getName());
            LOGGER.info("JSON schema: " + codegenParameter.jsonSchema);
        }

        if (parameter.getExtensions() != null && !parameter.getExtensions().isEmpty()) {
            codegenParameter.vendorExtensions.putAll(parameter.getExtensions());
        }

        if (parameter.getSchema() != null) {
            Schema parameterSchema = parameter.getSchema();
            if (parameterSchema == null) {
                LOGGER.warn("warning!  Schema not found for parameter \"" + parameter.getName() + "\", using String");
                parameterSchema = new StringSchema().description("//TODO automatically added by openapi-generator due to missing type definition.");
            }
            // set default value
            if (parameterSchema.getDefault() != null) {
                codegenParameter.defaultValue = toDefaultValue(parameterSchema);
            }
            // TDOO revise collectionFormat
            String collectionFormat = null;
            if (ModelUtils.isArraySchema(parameterSchema)) { // for array parameter
                final ArraySchema arraySchema = (ArraySchema) parameterSchema;
                Schema inner = arraySchema.getItems();
                if (inner == null) {
                    LOGGER.warn("warning! No inner type supplied for array parameter \"" + parameter.getName() + "\", using String");
                    inner = new StringSchema().description("//TODO automatically added by openapi-generator due to missing iner type definition in the spec");
                    arraySchema.setItems(inner);
                }

                collectionFormat = getCollectionFormat(parameter);
                // default to csv:
                collectionFormat = StringUtils.isEmpty(collectionFormat) ? "csv" : collectionFormat;
                CodegenProperty codegenProperty = fromProperty("inner", inner);
                codegenParameter.items = codegenProperty;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.baseType = codegenProperty.dataType;
                codegenParameter.isContainer = true;
                codegenParameter.isListContainer = true;


                // recursively add import
                while (codegenProperty != null) {
                    imports.add(codegenProperty.baseType);
                    codegenProperty = codegenProperty.items;
                }

            } else if (ModelUtils.isMapSchema(parameterSchema)) { // for map parameter
                CodegenProperty codegenProperty = fromProperty("inner", (Schema) parameterSchema.getAdditionalProperties());
                codegenParameter.items = codegenProperty;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.baseType = codegenProperty.dataType;
                codegenParameter.isContainer = true;
                codegenParameter.isMapContainer = true;

                // recursively add import
                while (codegenProperty != null) {
                    imports.add(codegenProperty.baseType);
                    codegenProperty = codegenProperty.items;
                }
            }
/* TODO revise the logic below
            } else {
                Map<PropertyId, Object> args = new HashMap<PropertyId, Object>();
                String format = qp.getFormat();
                args.put(PropertyId.ENUM, qp.getEnum());
                property = PropertyBuilder.build(type, format, args);
            }
*/

            CodegenProperty codegenProperty = fromProperty(parameter.getName(), parameterSchema);
            // TODO revise below which seems not working
            //if (parameterSchema.getRequired() != null && !parameterSchema.getRequired().isEmpty() && parameterSchema.getRequired().contains(codegenProperty.baseName)) {
            codegenProperty.required = Boolean.TRUE.equals(parameter.getRequired()) ? true : false;
            //}
            //codegenProperty.required = true;

            // set boolean flag (e.g. isString)
            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);


            String parameterDataType = this.getParameterDataType(parameter, parameterSchema);
            if (parameterDataType != null) {
                codegenParameter.dataType = parameterDataType;
            } else {
                codegenParameter.dataType = codegenProperty.dataType;
            }
            codegenParameter.dataFormat = codegenProperty.dataFormat;
            codegenParameter.required = codegenProperty.required;

            if (codegenProperty.isEnum) {
                codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
                codegenParameter.enumName = codegenProperty.enumName;
            }

            // enum
            updateCodegenPropertyEnum(codegenProperty);
            codegenParameter.isEnum = codegenProperty.isEnum;
            codegenParameter._enum = codegenProperty._enum;
            codegenParameter.allowableValues = codegenProperty.allowableValues;

            if (codegenProperty.items != null && codegenProperty.items.isEnum) {
                codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
                codegenParameter.enumName = codegenProperty.enumName;
                codegenParameter.items = codegenProperty.items;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
            }

            codegenParameter.collectionFormat = collectionFormat;
            if ("multi".equals(collectionFormat)) {
                codegenParameter.isCollectionFormatMulti = true;
            }
            codegenParameter.paramName = toParamName(parameter.getName());

            // import
            if (codegenProperty.complexType != null) {
                imports.add(codegenProperty.complexType);
            }

            // validation
            // handle maximum, minimum properly for int/long by removing the trailing ".0"
            if (ModelUtils.isIntegerSchema(parameterSchema)) {
                codegenParameter.maximum = parameterSchema.getMaximum() == null ? null : String.valueOf(parameterSchema.getMaximum().longValue());
                codegenParameter.minimum = parameterSchema.getMinimum() == null ? null : String.valueOf(parameterSchema.getMinimum().longValue());
            } else {
                codegenParameter.maximum = parameterSchema.getMaximum() == null ? null : String.valueOf(parameterSchema.getMaximum());
                codegenParameter.minimum = parameterSchema.getMinimum() == null ? null : String.valueOf(parameterSchema.getMinimum());
            }

            codegenParameter.exclusiveMaximum = parameterSchema.getExclusiveMaximum() == null ? false : parameterSchema.getExclusiveMaximum();
            codegenParameter.exclusiveMinimum = parameterSchema.getExclusiveMinimum() == null ? false : parameterSchema.getExclusiveMinimum();
            codegenParameter.maxLength = parameterSchema.getMaxLength();
            codegenParameter.minLength = parameterSchema.getMinLength();
            codegenParameter.pattern = toRegularExpression(parameterSchema.getPattern());
            codegenParameter.maxItems = parameterSchema.getMaxItems();
            codegenParameter.minItems = parameterSchema.getMinItems();
            codegenParameter.uniqueItems = parameterSchema.getUniqueItems() == null ? false : parameterSchema.getUniqueItems();
            codegenParameter.multipleOf = parameterSchema.getMultipleOf();

            // exclusive* are noop without corresponding min/max
            if (codegenParameter.maximum != null || codegenParameter.minimum != null ||
                    codegenParameter.maxLength != null || codegenParameter.minLength != null ||
                    codegenParameter.maxItems != null || codegenParameter.minItems != null ||
                    codegenParameter.pattern != null) {
                codegenParameter.hasValidation = true;
            }

        } else {
            LOGGER.error("ERROR! Not handling  " + parameter + " as Body Parameter at the moment");
             /* TODO need to revise the logic below to handle body parameter
            if (!(parameter instanceof BodyParameter)) {
                LOGGER.error("Cannot use Parameter " + parameter + " as Body Parameter");
            }

            BodyParameter bp = (BodyParameter) param;
            Model model = bp.getSchema();

            if (model instanceof ModelImpl) {
                ModelImpl impl = (ModelImpl) model;
                CodegenModel cm = fromModel(bp.getName(), impl);
                if (!cm.emptyVars) {
                    codegen.dataType = getTypeDeclaration(cm.classname);
                    imports.add(p.dataType);
                } else {
                    Property prop = PropertyBuilder.build(impl.getType(), impl.getFormat(), null);
                    prop.setRequired(bp.getRequired());
                    CodegenProperty cp = fromProperty("property", prop);
                    if (cp != null) {
                        p.baseType = cp.baseType;
                        p.dataType = cp.datatype;
                        p.isPrimitiveType = cp.isPrimitiveType;
                        p.isBinary = isDataTypeBinary(cp.datatype);
                        p.isFile = isDataTypeFile(cp.datatype);
                        if (cp.complexType != null) {
                            imports.add(cp.complexType);
                        }
                    }

                    // set boolean flag (e.g. isString)
                    setParameterBooleanFlagWithCodegenProperty(p, cp);
                }
            } else if (model instanceof ArrayModel) {
                // to use the built-in model parsing, we unwrap the ArrayModel
                // and get a single property from it
                ArrayModel impl = (ArrayModel) model;
                // get the single property
                ArrayProperty ap = new ArrayProperty().items(impl.getItems());
                ap.setRequired(param.getRequired());
                CodegenProperty cp = fromProperty("inner", ap);
                if (cp.complexType != null) {
                    imports.add(cp.complexType);
                }
                imports.add(cp.baseType);

                // recursively add import
                CodegenProperty innerCp = cp;
                while(innerCp != null) {
                    if(innerCp.complexType != null) {
                        imports.add(innerCp.complexType);
                    }
                    innerCp = innerCp.items;
                }

                p.items = cp;
                p.dataType = cp.datatype;
                p.baseType = cp.complexType;
                p.isPrimitiveType = cp.isPrimitiveType;
                p.isContainer = true;
                p.isListContainer = true;

                // set boolean flag (e.g. isString)
                setParameterBooleanFlagWithCodegenProperty(p, cp);
            } else {
                Model sub = bp.getSchema();
                if (sub instanceof RefModel) {
                    String name = ((RefModel) sub).getSimpleRef();
                    name = getAlias(name);
                    if (typeMapping.containsKey(name)) {
                        name = typeMapping.get(name);
                        p.baseType = name;
                    } else {
                        name = toModelName(name);
                        p.baseType = name;
                        if (defaultIncludes.contains(name)) {
                            imports.add(name);
                        }
                        imports.add(name);
                        name = getTypeDeclaration(name);
                    }
                    p.dataType = name;
                }
            }
            p.paramName = toParamName(bp.getName());
        */
        }

        if (parameter instanceof QueryParameter || "query".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isQueryParam = true;
        } else if (parameter instanceof PathParameter || "path".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.required = true;
            codegenParameter.isPathParam = true;
        } else if (parameter instanceof HeaderParameter || "header".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isHeaderParam = true;
        } else if (parameter instanceof CookieParameter || "cookie".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isCookieParam = true;
        } else {
            LOGGER.warn("Unknown parameter type: " + parameter.getName());
        }

        // set the parameter excample value
        // should be overridden by lang codegen
        setParameterExampleValue(codegenParameter);

        postProcessParameter(codegenParameter);
        LOGGER.debug("debugging codegenParameter return: " + codegenParameter);
        return codegenParameter;
    }

    /**
     * Returns the data type of a parameter.
     * Returns null by default to use the CodegenProperty.datatype value
     *
     * @param parameter Parameter
     * @param schema    Schema
     * @return data type
     */
    protected String getParameterDataType(Parameter parameter, Schema schema) {
        return null;
    }

    // TODO revise below as it should be replaced by ModelUtils.isByteArraySchema(parameterSchema)
    public boolean isDataTypeBinary(String dataType) {
        if (dataType != null) {
            return dataType.toLowerCase().startsWith("byte");
        } else {
            return false;
        }
    }

    // TODO revise below as it should be replaced by ModelUtils.isFileSchema(parameterSchema)
    public boolean isDataTypeFile(String dataType) {
        if (dataType != null) {
            return dataType.toLowerCase().equals("file");
        } else {
            return false;
        }
    }

    /**
     * Convert map of OAS SecurityScheme objects to a list of Codegen Security objects
     *
     * @param securitySchemeMap a map of OAS SecuritySchemeDefinition object
     * @return a list of Codegen Security objects
     */
    @SuppressWarnings("static-method")
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        if (securitySchemeMap == null) {
            return Collections.emptyList();
        }

        List<CodegenSecurity> codegenSecurities = new ArrayList<CodegenSecurity>(securitySchemeMap.size());
        for (String key : securitySchemeMap.keySet()) {
            final SecurityScheme securityScheme = securitySchemeMap.get(key);

            CodegenSecurity cs = CodegenModelFactory.newInstance(CodegenModelType.SECURITY);
            cs.name = key;
            cs.type = securityScheme.getType().toString();
            cs.isCode = cs.isPassword = cs.isApplication = cs.isImplicit = false;

            if (SecurityScheme.Type.APIKEY.equals(securityScheme.getType())) {
                cs.isBasic = cs.isOAuth = false;
                cs.isApiKey = true;
                cs.keyParamName = securityScheme.getName();
                cs.isKeyInHeader = securityScheme.getIn() == SecurityScheme.In.HEADER;
                cs.isKeyInQuery = !cs.isKeyInHeader;
            } else if (SecurityScheme.Type.HTTP.equals(securityScheme.getType())) {
                cs.isKeyInHeader = cs.isKeyInQuery = cs.isApiKey = cs.isOAuth = false;
                cs.isBasic = true;
            } else if (SecurityScheme.Type.OAUTH2.equals(securityScheme.getType())) {
                cs.isKeyInHeader = cs.isKeyInQuery = cs.isApiKey = cs.isBasic = false;
                cs.isOAuth = true;
                final OAuthFlows flows = securityScheme.getFlows();
                if (securityScheme.getFlows() == null) {
                    throw new RuntimeException("missing oauth flow in " + cs.name);
                }
                if (flows.getPassword() != null) {
                    setOauth2Info(cs, flows.getPassword());
                    cs.isPassword = true;
                    cs.flow = "password";
                } else if (flows.getImplicit() != null) {
                    setOauth2Info(cs, flows.getImplicit());
                    cs.isImplicit = true;
                    cs.flow = "implicit";
                } else if (flows.getClientCredentials() != null) {
                    setOauth2Info(cs, flows.getClientCredentials());
                    cs.isApplication = true;
                    cs.flow = "application";
                } else if (flows.getAuthorizationCode() != null) {
                    setOauth2Info(cs, flows.getAuthorizationCode());
                    cs.isCode = true;
                    cs.flow = "accessCode";
                } else {
                    throw new RuntimeException("Could not identify any oauth2 flow in " + cs.name);
                }
            }

            codegenSecurities.add(cs);
        }

        // sort auth methods to maintain the same order
        Collections.sort(codegenSecurities, new Comparator<CodegenSecurity>() {
            @Override
            public int compare(CodegenSecurity one, CodegenSecurity another) {
                return ObjectUtils.compare(one.name, another.name);
            }
        });
        // set 'hasMore'
        Iterator<CodegenSecurity> it = codegenSecurities.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            security.hasMore = it.hasNext();
        }

        return codegenSecurities;
    }

    protected void setReservedWordsLowerCase(List<String> words) {
        reservedWords = new HashSet<String>();
        for (String word : words) {
            reservedWords.add(word.toLowerCase());
        }
    }

    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word.toLowerCase());
    }

    /**
     * Get operationId from the operation object, and if it's blank, generate a new one from the given parameters.
     *
     * @param operation  the operation object
     * @param path       the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        String operationId = operation.getOperationId();
        if (StringUtils.isBlank(operationId)) {
            String tmpPath = path;
            tmpPath = tmpPath.replaceAll("\\{", "");
            tmpPath = tmpPath.replaceAll("\\}", "");
            String[] parts = (tmpPath + "/" + httpMethod).split("/");
            StringBuilder builder = new StringBuilder();
            if ("/".equals(tmpPath)) {
                // must be root tmpPath
                builder.append("root");
            }
            for (String part : parts) {
                if (part.length() > 0) {
                    if (builder.toString().length() == 0) {
                        part = Character.toLowerCase(part.charAt(0)) + part.substring(1);
                    } else {
                        part = initialCaps(part);
                    }
                    builder.append(part);
                }
            }
            operationId = sanitizeName(builder.toString());
            LOGGER.warn("Empty operationId found for path: " + httpMethod + " " + path + ". Renamed to auto-generated operationId: " + operationId);
        }
        return operationId;
    }

    /**
     * Check the type to see if it needs import the library/module/package
     *
     * @param type name of the type
     * @return true if the library/module/package of the corresponding type needs to be imported
     */
    protected boolean needToImport(String type) {
        return StringUtils.isNotBlank(type) && !defaultIncludes.contains(type)
                && !languageSpecificPrimitives.contains(type);
    }

    @SuppressWarnings("static-method")
    protected List<Map<String, Object>> toExamples(Map<String, Object> examples) {
        if (examples == null) {
            return null;
        }

        final List<Map<String, Object>> output = new ArrayList<Map<String, Object>>(examples.size());
        for (Map.Entry<String, Object> entry : examples.entrySet()) {
            final Map<String, Object> kv = new HashMap<String, Object>();
            kv.put("contentType", entry.getKey());
            kv.put("example", entry.getValue());
            output.add(kv);
        }
        return output;
    }

    /**
     * Add headers to codegen property
     *
     * @param response   API response
     * @param properties list of codegen property
     */
    private void addHeaders(ApiResponse response, List<CodegenProperty> properties) {
        if (response.getHeaders() != null) {
            for (Map.Entry<String, Header> headers : response.getHeaders().entrySet()) {
                CodegenProperty cp = fromProperty(headers.getKey(), headers.getValue().getSchema());
                cp.setDescription(escapeText(headers.getValue().getDescription()));
                cp.setUnescapedDescription(headers.getValue().getDescription());
                properties.add(cp);
            }
        }
    }

    private static List<CodegenParameter> addHasMore(List<CodegenParameter> objs) {
        if (objs != null) {
            for (int i = 0; i < objs.size(); i++) {
                if (i > 0) {
                    objs.get(i).secondaryParam = true;
                }
                if (i < objs.size() - 1) {
                    objs.get(i).hasMore = true;
                }
            }
        }
        return objs;
    }

    private static Map<String, Object> addHasMore(Map<String, Object> objs) {
        if (objs != null) {
            for (int i = 0; i < objs.size() - 1; i++) {
                if (i > 0) {
                    objs.put("secondaryParam", true);
                }
                if (i < objs.size() - 1) {
                    objs.put("hasMore", true);
                }
            }
        }
        return objs;
    }

    /**
     * Add operation to group
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    OAS Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @SuppressWarnings("static-method")
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }
        // check for operationId uniqueness
        String uniqueName = co.operationId;
        int counter = 0;
        for (CodegenOperation op : opList) {
            if (uniqueName.equals(op.operationId)) {
                uniqueName = co.operationId + "_" + counter;
                counter++;
            }
        }
        if (!co.operationId.equals(uniqueName)) {
            LOGGER.warn("generated unique operationId `" + uniqueName + "`");
        }
        co.operationId = uniqueName;
        co.operationIdLowerCase = uniqueName.toLowerCase();
        co.operationIdCamelCase = DefaultCodegen.camelize(uniqueName);
        co.operationIdSnakeCase = DefaultCodegen.underscore(uniqueName);
        opList.add(co);
        co.baseName = tag;
    }


    private void addParentContainer(CodegenModel model, String name, Schema schema) {
        final CodegenProperty property = fromProperty(name, schema);
        addImport(model, property.complexType);
        model.parent = toInstantiationType(schema);
        final String containerType = property.containerType;
        final String instantiationType = instantiationTypes.get(containerType);
        if (instantiationType != null) {
            addImport(model, instantiationType);
        }

        final String mappedType = typeMapping.get(containerType);
        if (mappedType != null) {
            addImport(model, mappedType);
        }
    }

    /**
     * Underscore the given word.
     * Copied from Twitter elephant bird
     * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
     *
     * @param word The word
     * @return The underscored version of the word
     */
    public static String underscore(String word) {
        String firstPattern = "([A-Z]+)([A-Z][a-z])";
        String secondPattern = "([a-z\\d])([A-Z])";
        String replacementPattern = "$1_$2";
        // Replace package separator with slash.
        word = word.replaceAll("\\.", "/"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // Replace $ with two underscores for inner classes.
        word = word.replaceAll("\\$", "__");
        // Replace capital letter with _ plus lowercase letter.
        word = word.replaceAll(firstPattern, replacementPattern);
        word = word.replaceAll(secondPattern, replacementPattern);
        word = word.replace('-', '_');
        // replace space with underscore
        word = word.replace(' ', '_');
        word = word.toLowerCase();
        return word;
    }

    /**
     * Dashize the given word.
     *
     * @param word The word
     * @return The dashized version of the word, e.g. "my-name"
     */
    @SuppressWarnings("static-method")
    protected String dashize(String word) {
        return underscore(word).replaceAll("[_ ]", "-");
    }

    /**
     * Generate the next name for the given name, i.e. append "2" to the base name if not ending with a number,
     * otherwise increase the number by 1. For example:
     * status    => status2
     * status2   => status3
     * myName100 => myName101
     *
     * @param name The base name
     * @return The next name for the base name
     */
    private static String generateNextName(String name) {
        Pattern pattern = Pattern.compile("\\d+\\z");
        Matcher matcher = pattern.matcher(name);
        if (matcher.find()) {
            String numStr = matcher.group();
            int num = Integer.parseInt(numStr) + 1;
            return name.substring(0, name.length() - numStr.length()) + num;
        } else {
            return name + "2";
        }
    }

    protected void addImport(CodegenModel m, String type) {
        if (type != null && needToImport(type)) {
            m.imports.add(type);
        }
    }

    private void addVars(CodegenModel model, Map<String, Schema> properties, List<String> required) {
        addVars(model, properties, required, null, null);
    }

    private void addVars(CodegenModel m, Map<String, Schema> properties, List<String> required,
                         Map<String, Schema> allProperties, List<String> allRequired) {

        m.hasRequired = false;
        if (properties != null && !properties.isEmpty()) {
            m.hasVars = true;
            m.hasEnums = false;

            Set<String> mandatory = required == null ? Collections.<String>emptySet()
                    : new TreeSet<String>(required);
            addVars(m, m.vars, properties, mandatory);
            m.allMandatory = m.mandatory = mandatory;
        } else {
            m.emptyVars = true;
            m.hasVars = false;
            m.hasEnums = false;
        }

        if (allProperties != null) {
            Set<String> allMandatory = allRequired == null ? Collections.<String>emptySet()
                    : new TreeSet<String>(allRequired);
            addVars(m, m.allVars, allProperties, allMandatory);
            m.allMandatory = allMandatory;
        }
    }

    private void addVars(CodegenModel m, List<CodegenProperty> vars, Map<String, Schema> properties, Set<String> mandatory) {
        // convert set to list so that we can access the next entry in the loop
        List<Map.Entry<String, Schema>> propertyList = new ArrayList<Map.Entry<String, Schema>>(properties.entrySet());
        final int totalCount = propertyList.size();
        for (int i = 0; i < totalCount; i++) {
            Map.Entry<String, Schema> entry = propertyList.get(i);

            final String key = entry.getKey();
            final Schema prop = entry.getValue();

            if (prop == null) {
                LOGGER.warn("null property for " + key);
            } else {
                final CodegenProperty cp = fromProperty(key, prop);
                cp.required = mandatory.contains(key);
                m.hasRequired = m.hasRequired || cp.required;
                m.hasOptional = m.hasOptional || !cp.required;
                if (cp.isEnum) {
                    // FIXME: if supporting inheritance, when called a second time for allProperties it is possible for
                    // m.hasEnums to be set incorrectly if allProperties has enumerations but properties does not.
                    m.hasEnums = true;
                }

                // set model's hasOnlyReadOnly to false if the property is read-only
                if (!Boolean.TRUE.equals(cp.isReadOnly)) {
                    m.hasOnlyReadOnly = false;
                }

                if (i + 1 != totalCount) {
                    cp.hasMore = true;
                    // check the next entry to see if it's read only
                    if (!Boolean.TRUE.equals(propertyList.get(i + 1).getValue().getReadOnly())) {
                        cp.hasMoreNonReadOnly = true; // next entry is not ready only
                    }
                }

                if (cp.isContainer) {
                    addImport(m, typeMapping.get("array"));
                }

                addImport(m, cp.baseType);
                CodegenProperty innerCp = cp;
                while (innerCp != null) {
                    addImport(m, innerCp.complexType);
                    innerCp = innerCp.items;
                }
                vars.add(cp);

                // if required, add to the list "requiredVars"
                if (Boolean.TRUE.equals(cp.required)) {
                    m.requiredVars.add(cp);
                } else { // else add to the list "optionalVars" for optional property
                    m.optionalVars.add(cp);
                }

                // if readonly, add to readOnlyVars (list of properties)
                if (Boolean.TRUE.equals(cp.isReadOnly)) {
                    m.readOnlyVars.add(cp);
                } else { // else add to readWriteVars (list of properties)
                    // FIXME: readWriteVars can contain duplicated properties. Debug/breakpoint here while running C# generator (Dog and Cat models)
                    m.readWriteVars.add(cp);
                }
            }
        }
    }

    /**
     * Determine all of the types in the model definitions (schemas) that are aliases of
     * simple types.
     *
     * @param schemas The complete set of model definitions (schemas).
     * @return A mapping from model name to type alias
     */
    private static Map<String, String> getAllAliases(Map<String, Schema> schemas) {
        if (schemas == null || schemas.isEmpty()) {
            return new HashMap<>();
        }

        Map<String, String> aliases = new HashMap<>();
        for (Map.Entry<String, Schema> entry : schemas.entrySet()) {
            String oasName = entry.getKey();
            Schema schema = entry.getValue();
            String schemaType = getPrimitiveType(schema);
            if (schemaType != null && !schemaType.equals("object") && !schemaType.equals("array") && schema.getEnum() == null) {
                aliases.put(oasName, schemaType);
            }
        }

        return aliases;
    }

    /**
     * Remove characters not suitable for variable or method name from the input and camelize it
     *
     * @param name string to be camelize
     * @return camelized string
     */
    @SuppressWarnings("static-method")
    public String removeNonNameElementToCamelCase(String name) {
        return removeNonNameElementToCamelCase(name, "[-_:;#]");
    }

    /**
     * Remove characters that is not good to be included in method name from the input and camelize it
     *
     * @param name                  string to be camelize
     * @param nonNameElementPattern a regex pattern of the characters that is not good to be included in name
     * @return camelized string
     */
    protected String removeNonNameElementToCamelCase(final String name, final String nonNameElementPattern) {
        String result = Arrays.stream(name.split(nonNameElementPattern))
                .map(StringUtils::capitalize)
                .collect(Collectors.joining(""));
        if (result.length() > 0) {
            result = result.substring(0, 1).toLowerCase() + result.substring(1);
        }
        return result;
    }

    /**
     * Camelize name (parameter, property, method, etc) with upper case for first letter
     * copied from Twitter elephant bird
     * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
     *
     * @param word string to be camelize
     * @return camelized string
     */
    public static String camelize(String word) {
        return camelize(word, false);
    }

    /**
     * Camelize name (parameter, property, method, etc)
     *
     * @param word                 string to be camelize
     * @param lowercaseFirstLetter lower case for first letter if set to true
     * @return camelized string
     */
    public static String camelize(String word, boolean lowercaseFirstLetter) {
        // Replace all slashes with dots (package separator)
        Pattern p = Pattern.compile("\\/(.?)");
        Matcher m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("." + m.group(1)/*.toUpperCase()*/); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
            m = p.matcher(word);
        }

        // case out dots
        String[] parts = word.split("\\.");
        StringBuilder f = new StringBuilder();
        for (String z : parts) {
            if (z.length() > 0) {
                f.append(Character.toUpperCase(z.charAt(0))).append(z.substring(1));
            }
        }
        word = f.toString();

        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("" + Character.toUpperCase(m.group(1).charAt(0)) + m.group(1).substring(1)/*.toUpperCase()*/);
            m = p.matcher(word);
        }

        // Uppercase the class name.
        p = Pattern.compile("(\\.?)(\\w)([^\\.]*)$");
        m = p.matcher(word);
        if (m.find()) {
            String rep = m.group(1) + m.group(2).toUpperCase() + m.group(3);
            rep = rep.replaceAll("\\$", "\\\\\\$");
            word = m.replaceAll(rep);
        }

        // Remove all underscores (underscore_case to camelCase)
        p = Pattern.compile("(_)(.)");
        m = p.matcher(word);
        while (m.find()) {
            String original = m.group(2);
            String upperCase = original.toUpperCase();
            if (original.equals(upperCase)) {
                word = word.replaceFirst("_", "");
            } else {
                word = m.replaceFirst(upperCase);
            }
            m = p.matcher(word);
        }

        // Remove all hyphens (hyphen-case to camelCase)
        p = Pattern.compile("(-)(.)");
        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst(m.group(2).toUpperCase());
            m = p.matcher(word);
        }

        if (lowercaseFirstLetter && word.length() > 0) {
            int i = 0;
            char charAt = word.charAt(i);
            while (i + 1 < word.length() && !((charAt >= 'a' && charAt <= 'z') || (charAt >= 'A' && charAt <= 'Z'))) {
                i = i + 1;
                charAt = word.charAt(i);
            }
            i = i + 1;
            word = word.substring(0, i).toLowerCase() + word.substring(i);
        }

        // remove all underscore
        word = word.replaceAll("_", "");

        return word;
    }

    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return apiFileFolder() + File.separator + toApiFilename(tag) + suffix;
    }

    /**
     * Return the full path and API documentation file
     *
     * @param templateName template name
     * @param tag          tag
     * @return the API documentation file name with full path
     */
    public String apiDocFilename(String templateName, String tag) {
        String suffix = apiDocTemplateFiles().get(templateName);
        return apiDocFileFolder() + File.separator + toApiDocFilename(tag) + suffix;
    }

    /**
     * Return the full path and API test file
     *
     * @param templateName template name
     * @param tag          tag
     * @return the API test file name with full path
     */
    public String apiTestFilename(String templateName, String tag) {
        String suffix = apiTestTemplateFiles().get(templateName);
        return apiTestFileFolder() + File.separator + toApiTestFilename(tag) + suffix;
    }

    public boolean shouldOverwrite(String filename) {
        return !(skipOverwrite && new File(filename).exists());
    }

    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    public void setSkipOverwrite(boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
    }

    public boolean isRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public void setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
    }

    public boolean isHideGenerationTimestamp() {
        return hideGenerationTimestamp;
    }

    public void setHideGenerationTimestamp(boolean hideGenerationTimestamp) {
        this.hideGenerationTimestamp = hideGenerationTimestamp;
    }

    /**
     * All library templates supported.
     * (key: library name, value: library description)
     *
     * @return the supported libraries
     */
    public Map<String, String> supportedLibraries() {
        return supportedLibraries;
    }

    /**
     * Set library template (sub-template).
     *
     * @param library Library template
     */
    public void setLibrary(String library) {
        if (library != null && !supportedLibraries.containsKey(library)) {
            StringBuilder sb = new StringBuilder("Unknown library: " + library + "\nAvailable libraries:");
            if (supportedLibraries.size() == 0) {
                sb.append("\n  ").append("NONE");
            } else {
                for (String lib : supportedLibraries.keySet()) {
                    sb.append("\n  ").append(lib);
                }
            }
            throw new RuntimeException(sb.toString());
        }
        this.library = library;
    }

    /**
     * Library template (sub-template).
     *
     * @return Library template
     */
    public String getLibrary() {
        return library;
    }

    /**
     * Set Git user ID.
     *
     * @param gitUserId Git user ID
     */
    public void setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
    }

    /**
     * Git user ID
     *
     * @return Git user ID
     */
    public String getGitUserId() {
        return gitUserId;
    }

    /**
     * Set Git repo ID.
     *
     * @param gitRepoId Git repo ID
     */
    public void setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
    }

    /**
     * Git repo ID
     *
     * @return Git repo ID
     */
    public String getGitRepoId() {
        return gitRepoId;
    }

    /**
     * Set release note.
     *
     * @param releaseNote Release note
     */
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    /**
     * Release note
     *
     * @return Release note
     */
    public String getReleaseNote() {
        return releaseNote;
    }

    /**
     * Set HTTP user agent.
     *
     * @param httpUserAgent HTTP user agent
     */
    public void setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
    }

    /**
     * HTTP user agent
     *
     * @return HTTP user agent
     */
    public String getHttpUserAgent() {
        return httpUserAgent;
    }

    @SuppressWarnings("static-method")
    protected CliOption buildLibraryCliOption(Map<String, String> supportedLibraries) {
        StringBuilder sb = new StringBuilder("library template (sub-template) to use:");
        for (String lib : supportedLibraries.keySet()) {
            sb.append("\n").append(lib).append(" - ").append(supportedLibraries.get(lib));
        }
        return new CliOption("library", sb.toString());
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    @SuppressWarnings("static-method")
    public String sanitizeName(String name) {
        return sanitizeName(name, "\\W");
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name            string to be sanitize
     * @param removeCharRegEx a regex containing all char that will be removed
     * @return sanitized string
     */
    public String sanitizeName(String name, String removeCharRegEx) {
        // NOTE: performance wise, we should have written with 2 replaceAll to replace desired
        // character with _ or empty character. Below aims to spell out different cases we've
        // encountered so far and hopefully make it easier for others to add more special
        // cases in the future.

        // better error handling when map/array type is invalid
        if (name == null) {
            LOGGER.error("String to be sanitized is null. Default to ERROR_UNKNOWN");
            return "ERROR_UNKNOWN";
        }

        // if the name is just '$', map it to 'value' for the time being.
        if ("$".equals(name)) {
            return "value";
        }

        // input[] => input
        name = name.replaceAll("\\[\\]", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // input[a][b] => input_a_b
        name = name.replaceAll("\\[", "_");
        name = name.replaceAll("\\]", "");

        // input(a)(b) => input_a_b
        name = name.replaceAll("\\(", "_");
        name = name.replaceAll("\\)", "");

        // input.name => input_name
        name = name.replaceAll("\\.", "_");

        // input-name => input_name
        name = name.replaceAll("-", "_");

        // input name and age => input_name_and_age
        name = name.replaceAll(" ", "_");

        // remove everything else other than word, number and _
        // $php_variable => php_variable
        if (allowUnicodeIdentifiers) { //could be converted to a single line with ?: operator
            name = Pattern.compile(removeCharRegEx, Pattern.UNICODE_CHARACTER_CLASS).matcher(name).replaceAll("");
        } else {
            name = name.replaceAll(removeCharRegEx, "");
        }

        return name;
    }

    /**
     * Sanitize tag
     *
     * @param tag Tag
     * @return Sanitized tag
     */
    public String sanitizeTag(String tag) {
        tag = camelize(sanitizeName(tag));

        // tag starts with numbers
        if (tag.matches("^\\d.*")) {
            tag = "Class" + tag;
        }

        return tag;
    }

    /**
     * Only write if the file doesn't exist
     *
     * @param outputFolder   Output folder
     * @param supportingFile Supporting file
     */
    public void writeOptional(String outputFolder, SupportingFile supportingFile) {
        String folder = "";

        if (outputFolder != null && !"".equals(outputFolder)) {
            folder += outputFolder + File.separator;
        }
        folder += supportingFile.folder;
        if (!"".equals(folder)) {
            folder += File.separator + supportingFile.destinationFilename;
        } else {
            folder = supportingFile.destinationFilename;
        }
        if (!new File(folder).exists()) {
            supportingFiles.add(supportingFile);
        } else {
            LOGGER.info("Skipped overwriting " + supportingFile.destinationFilename + " as the file already exists in " + folder);
        }
    }

    /**
     * Set CodegenParameter boolean flag using CodegenProperty.
     *
     * @param parameter Codegen Parameter
     * @param property  Codegen property
     */
    public void setParameterBooleanFlagWithCodegenProperty(CodegenParameter parameter, CodegenProperty property) {
        if (parameter == null) {
            LOGGER.error("Codegen Parameter cannot be null.");
            return;
        }

        if (property == null) {
            LOGGER.error("Codegen Property cannot be null.");
            return;
        }

        if (Boolean.TRUE.equals(property.isUuid) && Boolean.TRUE.equals(property.isString)) {
            parameter.isUuid = true;
        } else if (Boolean.TRUE.equals(property.isByteArray)) {
            parameter.isByteArray = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isBinary)) {
            parameter.isBinary = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isString)) {
            parameter.isString = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isBoolean)) {
            parameter.isBoolean = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isLong)) {
            parameter.isLong = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isInteger)) {
            parameter.isInteger = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDouble)) {
            parameter.isDouble = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isFloat)) {
            parameter.isFloat = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isNumber)) {
            parameter.isNumber = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDate)) {
            parameter.isDate = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDateTime)) {
            parameter.isDateTime = true;
            parameter.isPrimitiveType = true;
        } else {
            LOGGER.debug("Property type is not primitive: " + property.dataType);
        }

        if (Boolean.TRUE.equals(property.isFile)) {
            parameter.isFile = true;
        }
    }


    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        Map<String, Object> allowableValues = var.allowableValues;

        // handle array
        if (var.items != null) {
            allowableValues = var.items.allowableValues;
        }

        if (allowableValues == null) {
            return;
        }

        List<Object> values = (List<Object>) allowableValues.get("values");
        if (values == null) {
            return;
        }

        // put "enumVars" map into `allowableValues", including `name` and `value`
        List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
        String commonPrefix = findCommonPrefixOfVars(values);
        int truncateIdx = commonPrefix.length();
        for (Object value : values) {
            Map<String, String> enumVar = new HashMap<String, String>();
            String enumName;
            if (truncateIdx == 0) {
                enumName = value.toString();
            } else {
                enumName = value.toString().substring(truncateIdx);
                if ("".equals(enumName)) {
                    enumName = value.toString();
                }
            }
            enumVar.put("name", toEnumVarName(enumName, var.dataType));
            enumVar.put("value", toEnumValue(value.toString(), var.dataType));
            enumVars.add(enumVar);
        }
        allowableValues.put("enumVars", enumVars);

        // handle default value for enum, e.g. available => StatusEnum.AVAILABLE
        if (var.defaultValue != null) {
            String enumName = null;
            for (Map<String, String> enumVar : enumVars) {
                if (toEnumValue(var.defaultValue, var.dataType).equals(enumVar.get("value"))) {
                    enumName = enumVar.get("name");
                    break;
                }
            }
            if (enumName != null) {
                var.defaultValue = toEnumDefaultValue(enumName, var.datatypeWithEnum);
            }
        }
    }

    /**
     * If the pattern misses the delimiter, add "/" to the beginning and end
     * Otherwise, return the original pattern
     *
     * @param pattern the pattern (regular expression)
     * @return the pattern with delimiter
     */
    public String addRegularExpressionDelimiter(String pattern) {
        if (StringUtils.isEmpty(pattern)) {
            return pattern;
        }

        if (!pattern.matches("^/.*")) {
            return "/" + pattern.replaceAll("/", "\\\\/") + "/";
        }

        return pattern;
    }

    /**
     * reads propertyKey from additionalProperties, converts it to a boolean and
     * writes it back to additionalProperties to be usable as a boolean in
     * mustache files.
     *
     * @param propertyKey property key
     * @return property value as boolean
     */
    public boolean convertPropertyToBooleanAndWriteBack(String propertyKey) {
        boolean booleanValue = false;
        if (additionalProperties.containsKey(propertyKey)) {
            booleanValue = convertPropertyToBoolean(propertyKey);
            // write back as boolean
            writePropertyBack(propertyKey, booleanValue);
        }

        return booleanValue;
    }

    /**
     * Provides an override location, if any is specified, for the .openapi-generator-ignore.
     * <p>
     * This is originally intended for the first generation only.
     *
     * @return a string of the full path to an override ignore file.
     */
    public String getIgnoreFilePathOverride() {
        return ignoreFilePathOverride;
    }

    /**
     * Sets an override location for the '.openapi-generator-ignore' location for the first code generation.
     *
     * @param ignoreFileOverride The full path to an ignore file
     */
    public void setIgnoreFilePathOverride(final String ignoreFileOverride) {
        this.ignoreFilePathOverride = ignoreFileOverride;
    }

    public boolean convertPropertyToBoolean(String propertyKey) {
        boolean booleanValue = false;
        if (additionalProperties.containsKey(propertyKey)) {
            booleanValue = Boolean.valueOf(additionalProperties.get(propertyKey).toString());
        }

        return booleanValue;
    }

    public void writePropertyBack(String propertyKey, boolean value) {
        additionalProperties.put(propertyKey, value);
    }

    protected String getContentType(RequestBody requestBody) {
        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            return null;
        }
        return new ArrayList<>(requestBody.getContent().keySet()).get(0);
    }

    protected Parameter getParameterFromRef(String ref, OpenAPI openAPI) {
        String parameterName = ref.substring(ref.lastIndexOf('/') + 1);
        Map<String, Parameter> parameterMap = openAPI.getComponents().getParameters();
        return parameterMap.get(parameterName);
    }

    private void setOauth2Info(CodegenSecurity codegenSecurity, OAuthFlow flow) {
        codegenSecurity.authorizationUrl = flow.getAuthorizationUrl();
        codegenSecurity.tokenUrl = flow.getTokenUrl();

        if (flow.getScopes() != null && !flow.getScopes().isEmpty()) {
            List<Map<String, Object>> scopes = new ArrayList<Map<String, Object>>();
            int count = 0, numScopes = flow.getScopes().size();
            for (Map.Entry<String, String> scopeEntry : flow.getScopes().entrySet()) {
                Map<String, Object> scope = new HashMap<String, Object>();
                scope.put("scope", scopeEntry.getKey());
                scope.put("description", escapeText(scopeEntry.getValue()));

                count += 1;
                if (count < numScopes) {
                    scope.put("hasMore", "true");
                } else {
                    scope.put("hasMore", null);
                }

                scopes.add(scope);
            }
            codegenSecurity.scopes = scopes;
        }
    }

    private List<Schema> getInterfaces(ComposedSchema composed) {
        List<Schema> interfaces;
        if (composed.getAllOf() != null && !composed.getAllOf().isEmpty()) {
            return composed.getAllOf();
        } else if (composed.getAnyOf() != null && !composed.getAnyOf().isEmpty()) {
            return composed.getAnyOf();
        } else if (composed.getOneOf() != null && !composed.getOneOf().isEmpty()) {
            return composed.getOneOf();
        } else {
            return null;
        }
    }

    private void addConsumesInfo(OpenAPI openAPI, Operation operation, CodegenOperation codegenOperation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            return;
        }

        Set<String> consumes = requestBody.getContent().keySet();
        List<Map<String, String>> mediaTypeList = new ArrayList<>();
        int count = 0;
        for (String key : consumes) {
            Map<String, String> mediaType = new HashMap<>();
            if ("*/*".equals(key)) {
                // skip as it implies `consumes` in OAS2 is not defined
                continue;
            } else {
                mediaType.put("mediaType", escapeText(escapeQuotationMark(key)));
            }

            count += 1;
            if (count < consumes.size()) {
                mediaType.put("hasMore", "true");
            } else {
                mediaType.put("hasMore", null);
            }

            mediaTypeList.add(mediaType);
        }

        if (!mediaTypeList.isEmpty()) {
            codegenOperation.consumes = mediaTypeList;
            codegenOperation.hasConsumes = true;
        }
    }

    public static Set<String> getConsumesInfo(OpenAPI openAPI, Operation operation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());

        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            return Collections.emptySet(); // return emtpy set
        }
        return requestBody.getContent().keySet();
    }

    public boolean hasFormParameter(OpenAPI openAPI, Operation operation) {
        Set<String> consumesInfo = getConsumesInfo(openAPI, operation);

        if (consumesInfo == null || consumesInfo.isEmpty()) {
            return false;
        }

        for (String consume : consumesInfo) {
            if ("application/x-www-form-urlencoded".equalsIgnoreCase(consume) || "multipart/form-data".equalsIgnoreCase(consume)) {
                return true;
            }
        }

        return false;
    }

    public boolean hasBodyParameter(OpenAPI openAPI, Operation operation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
        if (requestBody == null) {
            return false;
        }

        Schema schema = ModelUtils.getSchemaFromRequestBody(requestBody);
        return ModelUtils.getReferencedSchema(openAPI, schema) != null;
    }

    private void addProducesInfo(OpenAPI openAPI, ApiResponse inputResponse, CodegenOperation codegenOperation) {
        ApiResponse response = ModelUtils.getReferencedApiResponse(openAPI, inputResponse);
        if (response == null || response.getContent() == null || response.getContent().isEmpty()) {
            return;
        }

        Set<String> produces = response.getContent().keySet();
        if (codegenOperation.produces == null) {
            codegenOperation.produces = new ArrayList<>();
        }

        Set<String> existingMediaTypes = new HashSet<>();
        for (Map<String, String> mediaType: codegenOperation.produces) {
            existingMediaTypes.add(mediaType.get("mediaType"));
        }

        int count = 0;
        for (String key : produces) {
            // escape quotation to avoid code injection, "*/*" is a special case, do nothing
            String encodedKey = "*/*".equals(key)? key : escapeText(escapeQuotationMark(key));
            //Only unique media types should be added to "produces"
            if (!existingMediaTypes.contains(encodedKey)) {
                Map<String, String> mediaType = new HashMap<String, String>();
                mediaType.put("mediaType", encodedKey);

                count += 1;
                if (count < produces.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }

                codegenOperation.produces.add(mediaType);
                codegenOperation.hasProduces = Boolean.TRUE;
            }
        }
    }

    /**
     * returns the list of MIME types the APIs can produce
     *
     * @param openAPI current specification instance
     * @param operation Operation
     * @return a set of MIME types
     */
    public static Set<String> getProducesInfo(OpenAPI openAPI, Operation operation) {
        if (operation.getResponses() == null || operation.getResponses().isEmpty()) {
            return null;
        }

        Set<String> produces = new TreeSet<String>();

        for (ApiResponse r : operation.getResponses().values()) {
            ApiResponse response = ModelUtils.getReferencedApiResponse(openAPI, r);
            if (response.getContent() != null) {
                produces.addAll(response.getContent().keySet());
            }
        }

        return produces;
    }

    protected Schema detectParent(ComposedSchema composedSchema, Map<String, Schema> allSchemas) {
        if (composedSchema.getAllOf() != null && !composedSchema.getAllOf().isEmpty()) {
            Schema schema = composedSchema.getAllOf().get(0);
            String ref = schema.get$ref();
            if (StringUtils.isBlank(ref)) {
                return null;
            }
            ref = ModelUtils.getSimpleRef(ref);
            return allSchemas.get(ref);
        }
        return null;
    }

    protected String getParentName(ComposedSchema composedSchema, Map<String, Schema> allSchemas) {
        if (composedSchema.getAllOf() != null && !composedSchema.getAllOf().isEmpty()) {
            Schema schema = composedSchema.getAllOf().get(0);
            String ref = schema.get$ref();
            if (StringUtils.isBlank(ref)) {
                return null;
            }
            return ModelUtils.getSimpleRef(ref);
        }
        return null;
    }

    protected String getCollectionFormat(Parameter parameter) {
        if (Parameter.StyleEnum.FORM.equals(parameter.getStyle())) {
            // Ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#style-values
            if (Boolean.TRUE.equals(parameter.getExplode())) { // explode is true (default)
                return "multi";
            } else {
                return "csv";
            }
        } else if (Parameter.StyleEnum.SIMPLE.equals(parameter.getStyle())) {
            return "csv";
        } else if (Parameter.StyleEnum.PIPEDELIMITED.equals(parameter.getStyle())) {
            return "pipe";
        } else if (Parameter.StyleEnum.SPACEDELIMITED.equals(parameter.getStyle())) {
            return "space";
        } else {
            return null;
        }
    }

    // TODO do we still need the methdo below?
    protected static boolean hasSchemaProperties(Schema schema) {
        final Object additionalProperties = schema.getAdditionalProperties();
        return additionalProperties != null && additionalProperties instanceof Schema;
    }

    public CodegenType getTag() {
        return null;
    }

    public String getName() {
        return null;
    }

    public String getHelp() {
        return null;
    }

    public List<CodegenParameter> fromRequestBodyToFormParameters(RequestBody body, Map<String, Schema> schemas, Set<String> imports) {
        List<CodegenParameter> parameters = new ArrayList<CodegenParameter>();
        LOGGER.debug("debugging fromRequestBodyToFormParameters= " + body);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        if (StringUtils.isNotBlank(schema.get$ref())) {
            schema = schemas.get(ModelUtils.getSimpleRef(schema.get$ref()));
        }
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            Map<String, Schema> properties = schema.getProperties();
            for (Map.Entry<String, Schema> entry : properties.entrySet()) {
                CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
                // key => property name
                // value => property schema
                String collectionFormat = null;
                Schema s = entry.getValue();
                // array of schema
                if (ModelUtils.isArraySchema(s)) {
                    final ArraySchema arraySchema = (ArraySchema) s;
                    Schema inner = arraySchema.getItems();
                    if (inner == null) {
                        LOGGER.warn("warning! No inner type supplied for array parameter \"" + s.getName() + "\", using String");
                        inner = new StringSchema().description("//TODO automatically added by openapi-generator due to missing iner type definition in the spec");
                        arraySchema.setItems(inner);
                    }

                    codegenParameter = fromFormProperty(entry.getKey(), inner, imports);
                    CodegenProperty codegenProperty = fromProperty("inner", inner);
                    codegenParameter.items = codegenProperty;
                    codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                    codegenParameter.baseType = codegenProperty.dataType;
                    codegenParameter.isPrimitiveType = false;
                    codegenParameter.isContainer = true;
                    codegenParameter.isListContainer = true;
                    codegenParameter.description = s.getDescription();
                    codegenParameter.dataType = getTypeDeclaration(s);
                    if (codegenParameter.baseType != null && codegenParameter.enumName != null){
                        codegenParameter.datatypeWithEnum = codegenParameter.dataType.replace(codegenParameter.baseType, codegenParameter.enumName);
                    }
                    else {
                        LOGGER.warn("Could not compute datatypeWithEnum from " + codegenParameter.baseType + ", " + codegenParameter.enumName);
                    }
                    //TODO fix collectformat for form parameters
                    //collectionFormat = getCollectionFormat(s);
                    // default to csv:
                    codegenParameter.collectionFormat = StringUtils.isEmpty(collectionFormat) ? "csv" : collectionFormat;

                    // recursively add import
                    while (codegenProperty != null) {
                        imports.add(codegenProperty.baseType);
                        codegenProperty = codegenProperty.items;
                    }

                } else if (ModelUtils.isMapSchema(s)) {
                    LOGGER.error("Map of form parameters not supported. Please report the issue to https://github.com/openapitools/openapi-generator if you need help.");
                    continue;
                } else {
                    codegenParameter = fromFormProperty(entry.getKey(), entry.getValue(), imports);
                }

                // Set 'required' flag defined in the schema element
                if (!codegenParameter.required && schema.getRequired() != null) {
                    codegenParameter.required = schema.getRequired().contains(entry.getKey());
                }

                parameters.add(codegenParameter);
            }
        }

        return parameters;
    }

    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);

        LOGGER.debug("Debugging fromFormProperty: " + name);
        CodegenProperty codegenProperty = fromProperty(name, propertySchema);

        codegenParameter.isFormParam = Boolean.TRUE;
        codegenParameter.baseName = codegenProperty.baseName;
        codegenParameter.paramName = toParamName((codegenParameter.baseName));
        codegenParameter.baseType = codegenProperty.baseType;
        codegenParameter.dataType = codegenProperty.dataType;
        codegenParameter.dataFormat = codegenProperty.dataFormat;
        codegenParameter.description = escapeText(codegenProperty.description);
        codegenParameter.unescapedDescription = codegenProperty.getDescription();
        codegenParameter.jsonSchema = Json.pretty(propertySchema);
        codegenParameter.defaultValue = codegenProperty.getDefaultValue();


        if (codegenProperty.getVendorExtensions() != null && !codegenProperty.getVendorExtensions().isEmpty()) {
            codegenParameter.vendorExtensions = codegenProperty.getVendorExtensions();
        }
        if (propertySchema.getRequired() != null && !propertySchema.getRequired().isEmpty() && propertySchema.getRequired().contains(codegenProperty.baseName)) {
            codegenParameter.required = Boolean.TRUE;
        }

        // non-array/map
        updateCodegenPropertyEnum(codegenProperty);
        codegenParameter.isEnum = codegenProperty.isEnum;
        codegenParameter._enum = codegenProperty._enum;
        codegenParameter.allowableValues = codegenProperty.allowableValues;

        if (codegenProperty.isEnum) {
            codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
            codegenParameter.enumName = codegenProperty.enumName;
        }

        if (codegenProperty.items != null && codegenProperty.items.isEnum) {
            codegenParameter.items = codegenProperty.items;
            codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
        }

        // import
        if (codegenProperty.complexType != null) {
            imports.add(codegenProperty.complexType);
        }

        // validation
        // handle maximum, minimum properly for int/long by removing the trailing ".0"
        if (ModelUtils.isIntegerSchema(propertySchema)) {
            codegenParameter.maximum = propertySchema.getMaximum() == null ? null : String.valueOf(propertySchema.getMaximum().longValue());
            codegenParameter.minimum = propertySchema.getMinimum() == null ? null : String.valueOf(propertySchema.getMinimum().longValue());
        } else {
            codegenParameter.maximum = propertySchema.getMaximum() == null ? null : String.valueOf(propertySchema.getMaximum());
            codegenParameter.minimum = propertySchema.getMinimum() == null ? null : String.valueOf(propertySchema.getMinimum());
        }

        codegenParameter.exclusiveMaximum = propertySchema.getExclusiveMaximum() == null ? false : propertySchema.getExclusiveMaximum();
        codegenParameter.exclusiveMinimum = propertySchema.getExclusiveMinimum() == null ? false : propertySchema.getExclusiveMinimum();
        codegenParameter.maxLength = propertySchema.getMaxLength();
        codegenParameter.minLength = propertySchema.getMinLength();
        codegenParameter.pattern = toRegularExpression(propertySchema.getPattern());
        codegenParameter.maxItems = propertySchema.getMaxItems();
        codegenParameter.minItems = propertySchema.getMinItems();
        codegenParameter.uniqueItems = propertySchema.getUniqueItems() == null ? false : propertySchema.getUniqueItems();
        codegenParameter.multipleOf = propertySchema.getMultipleOf();

        // exclusive* are noop without corresponding min/max
        if (codegenParameter.maximum != null || codegenParameter.minimum != null ||
                codegenParameter.maxLength != null || codegenParameter.minLength != null ||
                codegenParameter.maxItems != null || codegenParameter.minItems != null ||
                codegenParameter.pattern != null) {
            codegenParameter.hasValidation = true;
        }

        setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
        setParameterExampleValue(codegenParameter);

        //TODO collectionFormat for form parameter not yet supported
        //codegenParameter.collectionFormat = getCollectionFormat(propertySchema);
        return codegenParameter;
    }

    public CodegenParameter fromRequestBody(RequestBody body, Map<String, Schema> schemas, Set<String> imports, String bodyParameterName) {
        if (body == null) {
            LOGGER.error("body in fromRequestBody cannot be null!");
        }
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegenParameter.baseName = "UNKNOWN_BASE_NAME";
        codegenParameter.paramName = "UNKNOWN_PARAM_NAME";
        codegenParameter.description = body.getDescription();
        codegenParameter.required = body.getRequired() != null ? body.getRequired() : Boolean.FALSE;
        codegenParameter.isBodyParam = Boolean.TRUE;

        String name = null;
        LOGGER.debug("Request body = " + body);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        if (StringUtils.isNotBlank(schema.get$ref())) {
            name = ModelUtils.getSimpleRef(schema.get$ref());
            schema = schemas.get(name);
        }

        if (ModelUtils.isMapSchema(schema)) {
            Schema inner = (Schema) schema.getAdditionalProperties();
            if (inner == null) {
                inner = new StringSchema().description("//TODO automatically added by openapi-generator");
                schema.setAdditionalProperties(inner);
            }
            CodegenProperty codegenProperty = fromProperty("property", schema);
            // only support 1-dimension map only
            imports.add(codegenProperty.baseType);

            if (StringUtils.isEmpty(bodyParameterName)) {
                codegenParameter.baseName = "request_body";
            } else {
                codegenParameter.baseName = bodyParameterName;
            }
            codegenParameter.paramName = toParamName(codegenParameter.baseName);
            codegenParameter.items = codegenProperty.items;
            codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
            codegenParameter.dataType = getTypeDeclaration(schema);
            codegenParameter.baseType = getSchemaType(inner);
            codegenParameter.isContainer = Boolean.TRUE;
            codegenParameter.isMapContainer = Boolean.TRUE;

            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
        } else if (ModelUtils.isArraySchema(schema)) {
            final ArraySchema arraySchema = (ArraySchema) schema;
            Schema inner = arraySchema.getItems();
            if (inner == null) {
                inner = new StringSchema().description("//TODO automatically added by openapi-generator");
                arraySchema.setItems(inner);
            }
            CodegenProperty codegenProperty = fromProperty("property", schema);
            imports.add(codegenProperty.baseType);
            CodegenProperty innerCp = codegenProperty;
            CodegenProperty mostInnerItem = innerCp;
            // loop through multidimensional array to add proper import
            // also find the most inner item
            while (innerCp != null) {
                if (innerCp.complexType != null) {
                    imports.add(innerCp.complexType);
                }
                mostInnerItem = innerCp;
                innerCp = innerCp.items;
            }

            if (StringUtils.isEmpty(bodyParameterName)) {
                codegenParameter.baseName = mostInnerItem.complexType;
            } else {
                codegenParameter.baseName = bodyParameterName;
            }
            codegenParameter.paramName = toArrayModelParamName(codegenParameter.baseName);
            codegenParameter.items = codegenProperty.items;
            codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
            codegenParameter.dataType = getTypeDeclaration(arraySchema);
            codegenParameter.baseType = getSchemaType(arraySchema);
            codegenParameter.isContainer = Boolean.TRUE;
            codegenParameter.isListContainer = Boolean.TRUE;

            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);

            while (codegenProperty != null) {
                imports.add(codegenProperty.baseType);
                codegenProperty = codegenProperty.items;
            }

        } else if (ModelUtils.isObjectSchema(schema)) {
            CodegenModel codegenModel = null;
            if (StringUtils.isNotBlank(name)) {
                schema.setName(name);
                codegenModel = fromModel(name, schema, schemas);
            }

            if (codegenModel != null && !codegenModel.emptyVars) {
                if (StringUtils.isEmpty(bodyParameterName)) {
                    codegenParameter.baseName = codegenModel.classname;
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
                codegenParameter.baseType = codegenModel.classname;
                codegenParameter.dataType = getTypeDeclaration(codegenModel.classname);
                codegenParameter.description = codegenModel.description;
                imports.add(codegenParameter.baseType);
            } else {
                CodegenProperty codegenProperty = fromProperty("property", schema);
                if (schema.getAdditionalProperties() != null) {// http body is map
                    LOGGER.error("Map should be supported. Please report to openapi-generator github repo about the issue.");
                } else if (codegenProperty != null) {
                    LOGGER.warn("The folowing schema has undefined (null) baseType. " +
                            "It could be due to form parameter defined in OpenAPI v2 spec with incorrect consumes. " +
                            "A correct 'consumes' for form parameters should be " +
                            "'application/x-www-form-urlencoded' or 'multipart/form-data'");
                    LOGGER.warn("schema: " + schema);
                    LOGGER.warn("Defaulting baseType to UNKNOWN_BASE_TYPE");
                    codegenProperty.baseType = "UNKNOWN_BASE_TYPE";

                    codegenParameter.baseName = codegenProperty.baseType;
                    codegenParameter.baseType = codegenProperty.baseType;
                    codegenParameter.dataType = codegenProperty.dataType;
                    codegenParameter.description = codegenProperty.description;
                    codegenParameter.paramName = toParamName(codegenProperty.baseType);

                    if (codegenProperty.complexType != null) {
                        imports.add(codegenProperty.complexType);
                    }
                }
                setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
            }

        } else {
            // HTTP request body is primitive type (e.g. integer, string, etc)
            CodegenProperty codegenProperty = fromProperty("PRIMITIVE_REQUEST_BODY", schema);
            if (codegenProperty != null) {
                if (StringUtils.isEmpty(bodyParameterName)) {
                    codegenParameter.baseName = "body";  // default to body
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.isPrimitiveType = true;
                codegenParameter.baseType = codegenProperty.baseType;
                codegenParameter.dataType = codegenProperty.dataType;
                codegenParameter.description = codegenProperty.description;
                codegenParameter.paramName = toParamName(codegenParameter.baseName);

                if (codegenProperty.complexType != null) {
                    imports.add(codegenProperty.complexType);
                }

            }
            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
        }

        // set the parameter's example value
        // should be overridden by lang codegen
        setParameterExampleValue(codegenParameter);

        return codegenParameter;
    }

    protected void addOption(String key, String description) {
        addOption(key, description, null);
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    protected void addSwitch(String key, String description, Boolean defaultValue) {
        CliOption option = CliOption.newBoolean(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue.toString());
        cliOptions.add(option);
    }

    /**
     * generates OpenAPI specification file in JSON format
     *
     * @param objs map of object
     */
    public void generateJSONSpecFile(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        if (openAPI != null) {
            try {
                objs.put("openapi-json", Json.pretty().writeValueAsString(openAPI).replace("\r\n", "\n"));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
    }

    /**
     * generates OpenAPI specification file in YAML format
     *
     * @param objs map of object
     */
    public void generateYAMLSpecFile(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        if (openAPI != null) {
            try {
                objs.put("openapi-yaml", Yaml.mapper().writeValueAsString(openAPI));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
    }

}
