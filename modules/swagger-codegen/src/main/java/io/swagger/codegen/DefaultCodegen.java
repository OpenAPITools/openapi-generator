package io.swagger.codegen;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.samskivert.mustache.Mustache.Compiler;

import io.swagger.codegen.examples.ExampleGenerator;
import io.swagger.models.ArrayModel;
import io.swagger.models.ComposedModel;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.Operation;
import io.swagger.models.RefModel;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.auth.ApiKeyAuthDefinition;
import io.swagger.models.auth.BasicAuthDefinition;
import io.swagger.models.auth.In;
import io.swagger.models.auth.OAuth2Definition;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.models.parameters.BodyParameter;
import io.swagger.models.parameters.CookieParameter;
import io.swagger.models.parameters.FormParameter;
import io.swagger.models.parameters.HeaderParameter;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.parameters.PathParameter;
import io.swagger.models.parameters.QueryParameter;
import io.swagger.models.parameters.SerializableParameter;
import io.swagger.models.properties.AbstractNumericProperty;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BaseIntegerProperty;
import io.swagger.models.properties.BinaryProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.ByteArrayProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.PropertyBuilder;
import io.swagger.models.properties.PropertyBuilder.PropertyId;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import io.swagger.models.properties.UUIDProperty;
import io.swagger.util.Json;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
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
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DefaultCodegen {
    protected static final Logger LOGGER = LoggerFactory.getLogger(DefaultCodegen.class);
    
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

        if (additionalProperties.containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
            this.setSortParamsByRequiredFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ENSURE_UNIQUE_PARAMS)) {
            this.setEnsureUniqueParams(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ENSURE_UNIQUE_PARAMS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS)) {
            this.setAllowUnicodeIdentifiers(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS).toString()));
        }

        if(additionalProperties.containsKey(CodegenConstants.MODEL_NAME_PREFIX)){
            this.setModelNamePrefix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_PREFIX));
        }

        if(additionalProperties.containsKey(CodegenConstants.MODEL_NAME_SUFFIX)){
            this.setModelNameSuffix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_SUFFIX));
        }
    }

    // override with any special post-processing for all models
    @SuppressWarnings({ "static-method", "unchecked" })
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
                if (cm.parent != null) {
                    cm.parentModel = allModels.get(cm.parent);
                }
                if (cm.interfaces != null && !cm.interfaces.isEmpty()) {
                    cm.interfaceModels = new ArrayList<CodegenModel>(cm.interfaces.size());
                    for (String intf : cm.interfaces) {
                        CodegenModel intfModel = allModels.get(intf);
                        if (intfModel != null) {
                            cm.interfaceModels.add(intfModel);
                        }
                    }
                }
            }
            // Let parent know about all its children
            for (String name : allModels.keySet()) {
                CodegenModel cm = allModels.get(name);
                CodegenModel parent = allModels.get(cm.parent);
                // if a discriminator exists on the parent, don't add this child to the inheritance heirarchy
                // TODO Determine what to do if the parent discriminator name == the grandparent discriminator name
                while (parent != null) {
                    if (parent.children == null) {
                       parent.children = new ArrayList<CodegenModel>();
                    }
                    parent.children.add(cm);
                    if (parent.discriminator == null) {
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
     * Returns the common prefix of variables for enum naming
     * 
     * @param vars List of variable names
     * @return the common prefix for naming
     */
    public String findCommonPrefixOfVars(List<Object> vars) {
        try {
            String[] listStr = vars.toArray(new String[vars.size()]);
            String prefix = StringUtils.getCommonPrefix(listStr);
            // exclude trailing characters that should be part of a valid variable
            // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
            return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
        } catch (ArrayStoreException e) {
            return "";
        }
    }
    
    /**
     * Return the enum default value in the language specified format
     * 
     * @param value enum variable name
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
     * @param value enum variable name
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
     * @param value enum variable name
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
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        return objs;
    }

    // override to post-process any model properties
    @SuppressWarnings("unused")
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property){
    }

    // override to post-process any parameters
    @SuppressWarnings("unused")
    public void postProcessParameter(CodegenParameter parameter){
    }

    //override with any special handling of the entire swagger spec
    @SuppressWarnings("unused")
    public void preprocessSwagger(Swagger swagger) {
    }

    // override with any special handling of the entire swagger spec
    @SuppressWarnings("unused")
    public void processSwagger(Swagger swagger) {
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
                        .replaceAll("[\\t\\n\\r]"," ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));
    }

    /**
     * override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection
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

    public void setModelNamePrefix(String modelNamePrefix){
        this.modelNamePrefix = modelNamePrefix;
    }

    public void setModelNameSuffix(String modelNameSuffix){
        this.modelNameSuffix = modelNameSuffix;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public void setSortParamsByRequiredFlag(Boolean sortParamsByRequiredFlag) {
        this.sortParamsByRequiredFlag = sortParamsByRequiredFlag;
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
        return escapeText(addRegularExpressionDelimiter(pattern));
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
     *
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
     * This method will map between Swagger type and language-specified type, as well as mapping
     * between Swagger type and the corresponding import statement for the language. This will
     * also add some language specified CLI options, if any.
     *
     *
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
        typeMapping.put("binary", "byte[]");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "UUID");


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

        // we've used the .swagger-codegen-ignore approach as 
        // suppportingFiles can be cleared by code generator that extends
        // the default codegen, leaving the commented code below for 
        // future reference
        //supportingFiles.add(new GlobalSupportingFile("LICENSE", "LICENSE"));

        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENSURE_UNIQUE_PARAMS, CodegenConstants
                .ENSURE_UNIQUE_PARAMS_DESC).defaultValue(Boolean.TRUE.toString()));

        //name formatting options
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, CodegenConstants
                .ALLOW_UNICODE_IDENTIFIERS_DESC).defaultValue(Boolean.FALSE.toString()));

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
     * @param path the path of the operation
     * @param operation Swagger operation object
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
                    if (qp.getCollectionFormat() != null) {
                        paramPart.append(param.getName()).append("1");
                        if ("csv".equals(qp.getCollectionFormat())) {
                            paramPart.append(",");
                        } else if ("pipes".equals(qp.getCollectionFormat())) {
                            paramPart.append("|");
                        } else if ("tsv".equals(qp.getCollectionFormat())) {
                            paramPart.append("\t");
                        } else if ("multi".equals(qp.getCollectionFormat())) {
                            paramPart.append("&").append(param.getName()).append("=");
                            paramPart.append(param.getName()).append("2");
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
     * @param p Swagger property object
     * @return string presentation of the instantiation type of the property
     */
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            Property additionalProperties2 = ap.getAdditionalProperties();
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties2 + "\n" //
                      + "\tIn Property: " + p);
            }
            String inner = getSwaggerType(additionalProperties2);
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return instantiationTypes.get("array") + "<" + inner + ">";
        } else {
            return null;
        }
    }

    /**
     * Return the example value of the parameter. 
     *
     * @param p Swagger property object
     */
    public void setParameterExampleValue(CodegenParameter p) {

    }

    /**
     * Return the example value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the example value of the property
     */
    public String toExampleValue(Property p) {
        if(p.getExample() != null) {
            return p.getExample().toString();
        }
        if (p instanceof StringProperty) {
            return "null";
        } else if (p instanceof BooleanProperty) {
            return "null";
        } else if (p instanceof DateProperty) {
            return "null";
        } else if (p instanceof DateTimeProperty) {
            return "null";
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
            return "null";
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
            return "null";
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
            return "null";
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getExample() != null) {
                return dp.getExample().toString();
            }
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * Return the default value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "null";
        } else if (p instanceof BooleanProperty) {
            return "null";
        } else if (p instanceof DateProperty) {
            return "null";
        } else if (p instanceof DateTimeProperty) {
            return "null";
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * Return the property initialized from a data object
     * Useful for initialization with a plain object in Javascript
     *
     * @param name Name of the property object
     * @param p Swagger property object
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValueWithParam(String name, Property p) {
        return " = data." + name + ";";
    }

    /**
     * returns the swagger type for the property
     * @param p Swagger property object
     * @return string presentation of the type
     **/
    @SuppressWarnings("static-method")
    public String getSwaggerType(Property p) {
        String datatype = null;
        if (p instanceof StringProperty && "number".equals(p.getFormat())) {
            datatype = "BigDecimal";
        } else if (p instanceof StringProperty) {
            datatype = "string";
        } else if (p instanceof ByteArrayProperty) {
            datatype = "ByteArray";
        } else if (p instanceof BinaryProperty) {
            datatype = "binary";
        } else if (p instanceof FileProperty) {
            datatype = "file";
        } else if (p instanceof BooleanProperty) {
            datatype = "boolean";
        } else if (p instanceof DateProperty) {
            datatype = "date";
        } else if (p instanceof DateTimeProperty) {
            datatype = "DateTime";
        } else if (p instanceof DoubleProperty) {
            datatype = "double";
        } else if (p instanceof FloatProperty) {
            datatype = "float";
        } else if (p instanceof IntegerProperty) {
            datatype = "integer";
        } else if (p instanceof LongProperty) {
            datatype = "long";
        } else if (p instanceof MapProperty) {
            datatype = "map";
        } else if (p instanceof DecimalProperty) {
            datatype = "number";
        } else if ( p instanceof UUIDProperty) {
            datatype = "UUID";
        } else if (p instanceof RefProperty) {
            try {
                RefProperty r = (RefProperty) p;
                datatype = r.get$ref();
                if (datatype.indexOf("#/definitions/") == 0) {
                    datatype = datatype.substring("#/definitions/".length());
                }
            } catch (Exception e) {
                LOGGER.warn("Error obtaining the datatype from RefProperty:" + p + ". Datatype default to Object");
                datatype = "Object";
                LOGGER.error(e.getMessage(), e);
            }
        } else {
            if (p != null) {
                datatype = p.getType();
            }
        }
        return datatype;
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
     * @param p Swagger Property object
     * @return a string presentation of the property type
     */
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        return swaggerType;
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
     * Convert Swagger Model object to Codegen Model object without providing all model definitions
     *
     * @param name the name of the model
     * @param model Swagger Model object
     * @return Codegen Model object
     */
    public CodegenModel fromModel(String name, Model model) {
        return fromModel(name, model, null);
    }

    /**
     * Convert Swagger Model object to Codegen Model object
     *
     * @param name the name of the model
     * @param model Swagger Model object
     * @param allDefinitions a map of all Swagger models from the spec
     * @return Codegen Model object
     */
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel m = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        if (reservedWords.contains(name)) {
            m.name = escapeReservedWord(name);
        } else {
            m.name = name;
        }
        m.title = escapeText(model.getTitle());
        m.description = escapeText(model.getDescription());
        m.unescapedDescription = model.getDescription();
        m.classname = toModelName(name);
        m.classVarName = toVarName(name);
        m.classFilename = toModelFilename(name);
        m.modelJson = Json.pretty(model);
        m.externalDocs = model.getExternalDocs();
        m.vendorExtensions = model.getVendorExtensions();

        if (model instanceof ModelImpl) {
            m.discriminator = ((ModelImpl) model).getDiscriminator();
        }

        if (model instanceof ArrayModel) {
            ArrayModel am = (ArrayModel) model;
            ArrayProperty arrayProperty = new ArrayProperty(am.getItems());
            m.isArrayModel = true;
            m.arrayModelType = fromProperty(name, arrayProperty).complexType;
            addParentContainer(m, name, arrayProperty);
        } else if (model instanceof RefModel) {
            // TODO
        } else if (model instanceof ComposedModel) {
            final ComposedModel composed = (ComposedModel) model;
            Map<String, Property> properties = new LinkedHashMap<String, Property>();
            List<String> required = new ArrayList<String>();
            Map<String, Property> allProperties;
            List<String> allRequired;
            if (supportsInheritance || supportsMixins) {
                allProperties = new LinkedHashMap<String, Property>();
                allRequired = new ArrayList<String>();
                m.allVars = new ArrayList<CodegenProperty>();
                int modelImplCnt = 0; // only one inline object allowed in a ComposedModel
                for (Model innerModel: ((ComposedModel)model).getAllOf()) {
                    if (innerModel instanceof ModelImpl) {
                        if (m.discriminator == null) {
                            m.discriminator = ((ModelImpl) innerModel).getDiscriminator();
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
            RefModel parent = (RefModel) composed.getParent();

            // interfaces (intermediate models)
            if (composed.getInterfaces() != null) {
                if (m.interfaces == null)
                    m.interfaces = new ArrayList<String>();
                for (RefModel _interface : composed.getInterfaces()) {
                    Model interfaceModel = null;
                    if (allDefinitions != null) {
                        interfaceModel = allDefinitions.get(_interface.getSimpleRef());
                    }
                    // set first interface with discriminator found as parent
                    if (parent == null
                            && ((interfaceModel instanceof ModelImpl && ((ModelImpl) interfaceModel).getDiscriminator() != null)
                            || (interfaceModel instanceof ComposedModel && isDiscriminatorInInterfaceTree((ComposedModel) interfaceModel, allDefinitions)))) {
                        parent = _interface;
                    } else {
                        final String interfaceRef = toModelName(_interface.getSimpleRef());
                        m.interfaces.add(interfaceRef);
                        addImport(m, interfaceRef);
                        if (allDefinitions != null) {
                            if (!supportsMixins) {
                                addProperties(properties, required, interfaceModel, allDefinitions);
                            }
                            if (supportsInheritance) {
                                addProperties(allProperties, allRequired, interfaceModel, allDefinitions);
                            }
                        }
                    }
                }
            }

            if (parent != null) {
                final String parentRef = parent.getSimpleRef();
                m.parentSchema = parentRef;
                m.parent = toModelName(parent.getSimpleRef());
                addImport(m, m.parent);
                if (allDefinitions != null) {
                    final Model parentModel = allDefinitions.get(m.parentSchema);
                    if (supportsInheritance) {
                        addProperties(allProperties, allRequired, parentModel, allDefinitions);
                    } else {
                        addProperties(properties, required, parentModel, allDefinitions);
                    }
                }
            }

            // child model (properties owned by the model itself)
            Model child = composed.getChild();
            if (child != null && child instanceof RefModel && allDefinitions != null) {
                final String childRef = ((RefModel) child).getSimpleRef();
                child = allDefinitions.get(childRef);
            }
            if (child != null && child instanceof ModelImpl) {
                addProperties(properties, required, child, allDefinitions);
                if (supportsInheritance) {
                    addProperties(allProperties, allRequired, child, allDefinitions);
                }
            }
            addVars(m, properties, required, allProperties, allRequired);
        } else {
            ModelImpl impl = (ModelImpl) model;
            if (impl.getType() != null) {
                Property p = PropertyBuilder.build(impl.getType(), impl.getFormat(), null);
                m.dataType = getSwaggerType(p);
            }
            if(impl.getEnum() != null && impl.getEnum().size() > 0) {
                m.isEnum = true;
                // comment out below as allowableValues is not set in post processing model enum
                m.allowableValues = new HashMap<String, Object>();
                m.allowableValues.put("values", impl.getEnum());
            }
            if (impl.getAdditionalProperties() != null) {
                addAdditionPropertiesToCodeGenModel(m, impl);
            }
            addVars(m, impl.getProperties(), impl.getRequired());
        }

        if (m.vars != null) {
            for(CodegenProperty prop : m.vars) {
                postProcessModelProperty(m, prop);
            }
        }
        return m;
    }

    /**
     * Recursively look for a discriminator in the interface tree
     */
    private boolean isDiscriminatorInInterfaceTree(ComposedModel model, Map<String, Model> allDefinitions) {
        if (model == null || allDefinitions == null)
            return false;

        Model child = model.getChild();
        if (child instanceof ModelImpl && ((ModelImpl) child).getDiscriminator() != null) {
            return true;
        }
        for (RefModel _interface : model.getInterfaces()) {
            Model interfaceModel = allDefinitions.get(_interface.getSimpleRef());
            if (interfaceModel instanceof ModelImpl && ((ModelImpl) interfaceModel).getDiscriminator() != null) {
                return true;
            }
            if (interfaceModel instanceof ComposedModel) {

                return isDiscriminatorInInterfaceTree((ComposedModel) interfaceModel, allDefinitions);
            }
        }
        return false;
    }

    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, ModelImpl swaggerModel) {
        MapProperty mapProperty = new MapProperty(swaggerModel.getAdditionalProperties());
        addParentContainer(codegenModel, codegenModel.name, mapProperty);
    }

    protected void addProperties(Map<String, Property> properties,
                                 List<String> required, Model model,
                                 Map<String, Model> allDefinitions) {

        if (model instanceof ModelImpl) {
            ModelImpl mi = (ModelImpl) model;
            if (mi.getProperties() != null) {
                properties.putAll(mi.getProperties());
            }
            if (mi.getRequired() != null) {
                required.addAll(mi.getRequired());
            }
        } else if (model instanceof RefModel) {
            String interfaceRef = ((RefModel) model).getSimpleRef();
            Model interfaceModel = allDefinitions.get(interfaceRef);
            addProperties(properties, required, interfaceModel, allDefinitions);
        } else if (model instanceof ComposedModel) {
            for (Model component :((ComposedModel) model).getAllOf()) {
                addProperties(properties, required, component, allDefinitions);
            }
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
     * Convert Swagger Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p Swagger property object
     * @return Codegen Property object
     */
    public CodegenProperty fromProperty(String name, Property p) {
        if (p == null) {
            LOGGER.error("unexpected missing property for name " + name);
            return null;
        }

        CodegenProperty property = CodegenModelFactory.newInstance(CodegenModelType.PROPERTY);
        property.name = toVarName(name);
        property.baseName = name;
        property.nameInCamelCase = camelize(property.name, false);
        property.description = escapeText(p.getDescription());
        property.unescapedDescription = p.getDescription();
        property.title = p.getTitle();
        property.getter = "get" + getterAndSetterCapitalize(name);
        property.setter = "set" + getterAndSetterCapitalize(name);
        String example = toExampleValue(p);
        if(!"null".equals(example)) {
            property.example = example;
        }
        property.defaultValue = toDefaultValue(p);
        property.defaultValueWithParam = toDefaultValueWithParam(name, p);
        property.jsonSchema = Json.pretty(p);
        if (p.getReadOnly() != null) {
            property.isReadOnly = p.getReadOnly();
        }
        property.vendorExtensions = p.getVendorExtensions();

        String type = getSwaggerType(p);
        if (p instanceof AbstractNumericProperty) {
            AbstractNumericProperty np = (AbstractNumericProperty) p;
            if (np.getMinimum() != null) {
               if (p instanceof BaseIntegerProperty) { // int, long
                 property.minimum = String.valueOf(np.getMinimum().longValue());
               } else { // double, decimal
                 property.minimum = String.valueOf(np.getMinimum());
               }
            }
            if (np.getMaximum() != null) {
               if (p instanceof BaseIntegerProperty) { // int, long
                  property.maximum = String.valueOf(np.getMaximum().longValue());
               } else { // double, decimal
                  property.maximum = String.valueOf(np.getMaximum());
               }
            }

            if (np.getExclusiveMinimum() != null) {
                property.exclusiveMinimum = np.getExclusiveMinimum();
            }
            if (np.getExclusiveMaximum() != null) {
                property.exclusiveMaximum = np.getExclusiveMaximum();
            }

            // check if any validation rule defined
            // exclusive* are noop without corresponding min/max
            if (property.minimum != null || property.maximum != null)
                property.hasValidation = true;

            // legacy support
            Map<String, Object> allowableValues = new HashMap<String, Object>();
            if (np.getMinimum() != null) {
                allowableValues.put("min", np.getMinimum());
            }
            if (np.getMaximum() != null) {
                allowableValues.put("max", np.getMaximum());
            }
            if(allowableValues.size() > 0) {
              property.allowableValues = allowableValues;
            }
        }

        if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            property.maxLength = sp.getMaxLength();
            property.minLength = sp.getMinLength();
            property.pattern = toRegularExpression(sp.getPattern());

            // check if any validation rule defined
            if (property.pattern != null || property.minLength != null || property.maxLength != null)
                property.hasValidation = true;

            property.isString = true;
            if (sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = _enum;
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }

        // type is integer and without format
        if (p instanceof BaseIntegerProperty && !(p instanceof IntegerProperty) && !(p instanceof LongProperty)) {
            BaseIntegerProperty sp = (BaseIntegerProperty) p;
            property.isInteger = true;
            /*if (sp.getEnum() != null) {
                List<Integer> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Integer i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }*/
        }
        if (p instanceof IntegerProperty) {
            IntegerProperty sp = (IntegerProperty) p;
            property.isInteger = true;
            if (sp.getEnum() != null) {
                List<Integer> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Integer i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if (p instanceof LongProperty) {
            LongProperty sp = (LongProperty) p;
            property.isLong = true;
            if (sp.getEnum() != null) {
                List<Long> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Long i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if (p instanceof BooleanProperty) {
            property.isBoolean = true;
        }
        if (p instanceof BinaryProperty) {
            property.isBinary = true;
        }
        if (p instanceof FileProperty) {
            property.isFile = true;
        }
        if (p instanceof UUIDProperty) {
            property.isString = true;
        }
        if (p instanceof ByteArrayProperty) {
            property.isByteArray = true;
        }
        // type is number and without format
        if (p instanceof DecimalProperty && !(p instanceof DoubleProperty) && !(p instanceof FloatProperty)) {
            DecimalProperty sp = (DecimalProperty) p;
            property.isFloat = true;
            /*if (sp.getEnum() != null) {
                List<Double> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Double i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }*/
        }
        if (p instanceof DoubleProperty) {
            DoubleProperty sp = (DoubleProperty) p;
            property.isDouble = true;
            if (sp.getEnum() != null) {
                List<Double> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Double i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if (p instanceof FloatProperty) {
            FloatProperty sp = (FloatProperty) p;
            property.isFloat = true;
            if (sp.getEnum() != null) {
                List<Float> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(Float i : _enum) {
                  property._enum.add(i.toString());
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }

        if (p instanceof DateProperty) {
            DateProperty sp = (DateProperty) p;
            property.isDate = true;
            if (sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(String i : _enum) {
                  property._enum.add(i);
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        if (p instanceof DateTimeProperty) {
            DateTimeProperty sp = (DateTimeProperty) p;
            property.isDateTime = true;
            if (sp.getEnum() != null) {
                List<String> _enum = sp.getEnum();
                property._enum = new ArrayList<String>();
                for(String i : _enum) {
                  property._enum.add(i);
                }
                property.isEnum = true;

                // legacy support
                Map<String, Object> allowableValues = new HashMap<String, Object>();
                allowableValues.put("values", _enum);
                property.allowableValues = allowableValues;
            }
        }
        property.datatype = getTypeDeclaration(p);
        property.dataFormat = p.getFormat();

        // this can cause issues for clients which don't support enums
        if (property.isEnum) {
            property.datatypeWithEnum = toEnumName(property);
            property.enumName = toEnumName(property);
        } else {
            property.datatypeWithEnum = property.datatype;
        }

        property.baseType = getSwaggerType(p);

          if (p instanceof ArrayProperty) {
            property.isContainer = true;
            property.isListContainer = true;
            property.containerType = "array";
            property.baseType = getSwaggerType(p);
            // handle inner property
            ArrayProperty ap = (ArrayProperty) p;
            property.maxItems = ap.getMaxItems();
            property.minItems = ap.getMinItems();
            String itemName = (String) p.getVendorExtensions().get("x-item-name");
            if (itemName == null) {
                itemName = property.name;
            }
            CodegenProperty cp = fromProperty(itemName, ap.getItems());
            updatePropertyForArray(property, cp);
          } else if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;

            property.isContainer = true;
            property.isMapContainer = true;
            property.containerType = "map";
            property.baseType = getSwaggerType(p);
            property.minItems = ap.getMinProperties();
            property.maxItems = ap.getMaxProperties();

            // handle inner property
            CodegenProperty cp = fromProperty("inner", ap.getAdditionalProperties());
            updatePropertyForMap(property, cp);
        } else {
            setNonArrayMapProperty(property, type);
        }
        return property;
    }

    /**
     * Update property for array(list) container
     * @param property Codegen property
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
     * @param property Codegen property
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
     * @param property Codegen property
     * @return True if the inner most type is enum
     */
    protected Boolean isPropertyInnerMostEnum(CodegenProperty property) {
        CodegenProperty currentProperty = property;
        while (currentProperty != null && (Boolean.TRUE.equals(currentProperty.isMapContainer)
                    || Boolean.TRUE.equals(currentProperty.isListContainer))) {
            currentProperty = currentProperty.items;
        }

        return currentProperty == null ? false : currentProperty.isEnum;
    }

    protected Map<String, Object> getInnerEnumAllowableValues(CodegenProperty property) {
        CodegenProperty currentProperty = property;
        while (currentProperty != null && (Boolean.TRUE.equals(currentProperty.isMapContainer)
                    || Boolean.TRUE.equals(currentProperty.isListContainer))) {
            currentProperty = currentProperty.items;
        }

        return currentProperty == null ? new HashMap<String, Object>() : currentProperty.allowableValues;
    }


    /**
     * Update datatypeWithEnum for array container
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
        }
    }

    /**
     * Update datatypeWithEnum for map container
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
     * @param responses Swagger Operation's responses
     * @return default method response or <tt>null</tt> if not found
     */
    protected Response findMethodResponse(Map<String, Response> responses) {

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
     * Convert Swagger Operation object to Codegen Operation object (without providing a Swagger object)
     *
     * @param path the path of the operation
     * @param httpMethod HTTP method
     * @param operation Swagger operation object
     * @param definitions a map of Swagger models
     * @return Codegen Operation object
     */
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Model> definitions) {
        return fromOperation(path, httpMethod, operation, definitions, null);
    }

    /**
     * Convert Swagger Operation object to Codegen Operation object
     *
     * @param path the path of the operation
     * @param httpMethod HTTP method
     * @param operation Swagger operation object
     * @param definitions a map of Swagger models
     * @param swagger a Swagger object representing the spec
     * @return Codegen Operation object
     */
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          Map<String, Model> definitions,
                                          Swagger swagger) {
        CodegenOperation op = CodegenModelFactory.newInstance(CodegenModelType.OPERATION);
        Set<String> imports = new HashSet<String>();
        op.vendorExtensions = operation.getVendorExtensions();

        String operationId = getOrGenerateOperationId(operation, path, httpMethod);
        operationId = removeNonNameElementToCamelCase(operationId);
        op.path = path;
        op.operationId = toOperationId(operationId);
        op.summary = escapeText(operation.getSummary());
        op.unescapedNotes = operation.getDescription();
        op.notes = escapeText(operation.getDescription());
        op.tags = operation.getTags();
        op.hasConsumes = false;
        op.hasProduces = false;

        List<String> consumes = new ArrayList<String>();
        if (operation.getConsumes() != null) {
            if (operation.getConsumes().size() > 0) {
                // use consumes defined in the operation
                consumes = operation.getConsumes();
            } else {
                // empty list, do nothing to override global setting
            }
        } else if (swagger != null && swagger.getConsumes() != null && swagger.getConsumes().size() > 0) {
            // use consumes defined globally
            consumes = swagger.getConsumes();
            LOGGER.debug("No consumes defined in operation. Using global consumes (" + swagger.getConsumes() + ") for " + op.operationId);
        }

        // if "consumes" is defined (per operation or using global definition)
        if (consumes != null && consumes.size() > 0) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            int count = 0;
            for (String key : consumes) {
                Map<String, String> mediaType = new HashMap<String, String>();
                // escape quotation to avoid code injection
                if ("*/*".equals(key)) { // "*/*" is a special case, do nothing
                    mediaType.put("mediaType", key);
                } else {
                    mediaType.put("mediaType", escapeText(escapeQuotationMark(key)));
                }
                count += 1;
                if (count < consumes.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }
                c.add(mediaType);
            }
            op.consumes = c;
            op.hasConsumes = true;
        }

        List<String> produces = new ArrayList<String>();
        if (operation.getProduces() != null) {
            if (operation.getProduces().size() > 0) {
                // use produces defined in the operation
                produces = operation.getProduces();
            } else {
                // empty list, do nothing to override global setting
            }
        } else if (swagger != null && swagger.getProduces() != null && swagger.getProduces().size() > 0) {
            // use produces defined globally
            produces = swagger.getProduces();
            LOGGER.debug("No produces defined in operation. Using global produces (" + swagger.getProduces() + ") for " + op.operationId);
        }

        // if "produces" is defined (per operation or using global definition)
        if (produces != null && !produces.isEmpty()) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            int count = 0;
            for (String key : produces) {
                Map<String, String> mediaType = new HashMap<String, String>();
                // escape quotation to avoid code injection
                if ("*/*".equals(key)) { // "*/*" is a special case, do nothing
                    mediaType.put("mediaType", key);
                } else {
                    mediaType.put("mediaType", escapeText(escapeQuotationMark(key)));
                }
                count += 1;
                if (count < produces.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }
                c.add(mediaType);
            }
            op.produces = c;
            op.hasProduces = true;
        }

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            Response methodResponse = findMethodResponse(operation.getResponses());

            for (Map.Entry<String, Response> entry : operation.getResponses().entrySet()) {
                Response response = entry.getValue();
                CodegenResponse r = fromResponse(entry.getKey(), response);
                r.hasMore = true;
                if (r.baseType != null &&
                        !defaultIncludes.contains(r.baseType) &&
                        !languageSpecificPrimitives.contains(r.baseType)) {
                    imports.add(r.baseType);
                }
                r.isDefault = response == methodResponse;
                op.responses.add(r);
                if (Boolean.TRUE.equals(r.isBinary) && Boolean.TRUE.equals(r.isDefault)){
                    op.isResponseBinary = Boolean.TRUE;
                }
                if (Boolean.TRUE.equals(r.isFile) && Boolean.TRUE.equals(r.isDefault)){
                    op.isResponseFile = Boolean.TRUE;
                }
            }
            op.responses.get(op.responses.size() - 1).hasMore = false;

            if (methodResponse != null) {
                if (methodResponse.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", methodResponse.getSchema());

                    Property responseProperty = methodResponse.getSchema();

                    if (responseProperty instanceof ArrayProperty) {
                        ArrayProperty ap = (ArrayProperty) responseProperty;
                        CodegenProperty innerProperty = fromProperty("response", ap.getItems());
                        op.returnBaseType = innerProperty.baseType;
                    } else {
                        if (cm.complexType != null) {
                            op.returnBaseType = cm.complexType;
                        } else {
                            op.returnBaseType = cm.baseType;
                        }
                    }
                    op.examples = new ExampleGenerator(definitions).generate(methodResponse.getExamples(), operation.getProduces(), responseProperty);
                    op.defaultResponse = toDefaultValue(responseProperty);
                    op.returnType = cm.datatype;
                    op.hasReference = definitions != null && definitions.containsKey(op.returnBaseType);

                    // lookup discriminator
                    if (definitions != null) {
                        Model m = definitions.get(op.returnBaseType);
                        if (m != null) {
                            CodegenModel cmod = fromModel(op.returnBaseType, m, definitions);
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
        CodegenParameter bodyParam = null;
        List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();

        if (parameters != null) {
            for (Parameter param : parameters) {
                CodegenParameter p = fromParameter(param, imports);
                // rename parameters to make sure all of them have unique names
                if (ensureUniqueParams) {
                    while (true) {
                        boolean exists = false;
                        for (CodegenParameter cp : allParams) {
                            if (p.paramName.equals(cp.paramName)) {
                                exists = true;
                                break;
                            }
                        }
                        if (exists) {
                            p.paramName = generateNextName(p.paramName);
                        } else {
                            break;
                        }
                    }
                }

                // set isPrimitiveType and baseType for allParams
                /*if (languageSpecificPrimitives.contains(p.baseType)) {
                    p.isPrimitiveType = true;
                    p.baseType = getSwaggerType(p);
                }*/


                allParams.add(p);
                // Issue #2561 (neilotoole) : Moved setting of is<Type>Param flags
                // from here to fromParameter().
                if (param instanceof QueryParameter) {
                    queryParams.add(p.copy());
                } else if (param instanceof PathParameter) {
                    pathParams.add(p.copy());
                } else if (param instanceof HeaderParameter) {
                    headerParams.add(p.copy());
                } else if (param instanceof CookieParameter) {
                    cookieParams.add(p.copy());
                } else if (param instanceof BodyParameter) {
                    bodyParam = p;
                    bodyParams.add(p.copy());
                } else if (param instanceof FormParameter) {
                    formParams.add(p.copy());
                }
                if (!p.required) {
                    op.hasOptionalParams = true;
                }
            }
        }
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
        // op.cookieParams = cookieParams;
        op.formParams = addHasMore(formParams);
        // legacy support
        op.nickname = op.operationId;

        if (op.allParams.size() > 0) {
            op.hasParams = true;
        }
        op.externalDocs = operation.getExternalDocs();

        // set Restful Flag
        op.isRestfulShow = op.isRestfulShow();
        op.isRestfulIndex = op.isRestfulIndex();
        op.isRestfulCreate = op.isRestfulCreate();
        op.isRestfulUpdate = op.isRestfulUpdate();
        op.isRestfulDestroy = op.isRestfulDestroy();
        op.isRestful = op.isRestful();

        return op;
    }

    /**
     * Convert Swagger Response object to Codegen Response object
     *
     * @param responseCode HTTP response code
     * @param response Swagger Response object
     * @return Codegen Response object
     */
    public CodegenResponse fromResponse(String responseCode, Response response) {
        CodegenResponse r = CodegenModelFactory.newInstance(CodegenModelType.RESPONSE);
        if ("default".equals(responseCode)) {
            r.code = "0";
        } else {
            r.code = responseCode;
        }
        r.message = escapeText(response.getDescription());
        r.schema = response.getSchema();
        r.examples = toExamples(response.getExamples());
        r.jsonSchema = Json.pretty(response);
        r.vendorExtensions = response.getVendorExtensions();
        addHeaders(response, r.headers);
        r.hasHeaders = !r.headers.isEmpty();

        if (r.schema != null) {
            Property responseProperty = response.getSchema();
            responseProperty.setRequired(true);
            CodegenProperty cm = fromProperty("response", responseProperty);

            if (responseProperty instanceof ArrayProperty) {
                ArrayProperty ap = (ArrayProperty) responseProperty;
                CodegenProperty innerProperty = fromProperty("response", ap.getItems());
                r.baseType = innerProperty.baseType;
            } else {
                if (cm.complexType != null) {
                    r.baseType = cm.complexType;
                } else {
                    r.baseType = cm.baseType;
                }
            }
            r.dataType = cm.datatype;

            if (Boolean.TRUE.equals(cm.isString)) {
                r.isString = true;
            } else if (Boolean.TRUE.equals(cm.isBoolean)) {
                r.isBoolean = true;
            } else if (Boolean.TRUE.equals(cm.isLong)) {
                r.isLong = true;
            } else if (Boolean.TRUE.equals(cm.isInteger)) {
                r.isInteger = true;
            } else if (Boolean.TRUE.equals(cm.isDouble)) {
                r.isDouble = true;
            } else if (Boolean.TRUE.equals(cm.isFloat)) {
                r.isFloat = true;
            } else if (Boolean.TRUE.equals(cm.isByteArray)) {
                r.isByteArray = true;
            } else if (Boolean.TRUE.equals(cm.isBinary)) {
                r.isBinary = true;
            } else if (Boolean.TRUE.equals(cm.isFile)) {
                r.isFile = true;
            } else if (Boolean.TRUE.equals(cm.isDate)) {
                r.isDate = true;
            } else if (Boolean.TRUE.equals(cm.isDateTime)) {
                r.isDateTime = true;
            } else {
                LOGGER.debug("Property type is not primitive: " + cm.datatype);
            }

            if (cm.isContainer) {
                r.simpleType = false;
                r.containerType = cm.containerType;
                r.isMapContainer = "map".equals(cm.containerType);
                r.isListContainer = "list".equalsIgnoreCase(cm.containerType) || "array".equalsIgnoreCase(cm.containerType);
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
     * Convert Swagger Parameter object to Codegen Parameter object
     *
     * @param param Swagger parameter object
     * @param imports set of imports for library/package/module
     * @return Codegen Parameter object
     */
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter p = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        p.baseName = param.getName();
        p.description = escapeText(param.getDescription());
        p.unescapedDescription = param.getDescription();
        if (param.getRequired()) {
            p.required = param.getRequired();
        }
        p.jsonSchema = Json.pretty(param);

        if (System.getProperty("debugParser") != null) {
            LOGGER.info("working on Parameter " + param.getName());
        }

        // move the defaultValue for headers, forms and params
        if (param instanceof QueryParameter) {
            QueryParameter qp = (QueryParameter) param;
            if(qp.getDefaultValue() != null) {
                p.defaultValue = qp.getDefaultValue().toString();
            }
        } else if (param instanceof HeaderParameter) {
            HeaderParameter hp = (HeaderParameter) param;
            if(hp.getDefaultValue() != null) {
                p.defaultValue = hp.getDefaultValue().toString();
            }
        } else if (param instanceof FormParameter) {
            FormParameter fp = (FormParameter) param;
            if(fp.getDefaultValue() != null) {
                p.defaultValue = fp.getDefaultValue().toString();
            }
        }

        p.vendorExtensions = param.getVendorExtensions();

        if (param instanceof SerializableParameter) {
            SerializableParameter qp = (SerializableParameter) param;
            Property property;
            String collectionFormat = null;
            String type = qp.getType();
            if (null == type) {
                LOGGER.warn("Type is NULL for Serializable Parameter: " + param.getName());
            }
            if ("array".equals(type)) { // for array parameter
                Property inner = qp.getItems();
                if (inner == null) {
                    LOGGER.warn("warning!  No inner type supplied for array parameter \"" + qp.getName() + "\", using String");
                    inner = new StringProperty().description("//TODO automatically added by swagger-codegen");
                }
                property = new ArrayProperty(inner);
                collectionFormat = qp.getCollectionFormat();
                if (collectionFormat == null) {
                    collectionFormat = "csv";
                }
                CodegenProperty pr = fromProperty("inner", inner);
                p.items = pr;
                p.baseType = pr.datatype;
                p.isContainer = true;
                p.isListContainer = true;
                imports.add(pr.baseType);
            } else if ("object".equals(type)) { // for map parameter
                Property inner = qp.getItems();
                if (inner == null) {
                    LOGGER.warn("warning!  No inner type supplied for map parameter \"" + qp.getName() + "\", using String");
                    inner = new StringProperty().description("//TODO automatically added by swagger-codegen");
                }
                property = new MapProperty(inner);
                collectionFormat = qp.getCollectionFormat();
                CodegenProperty pr = fromProperty("inner", inner);
                p.items = pr;
                p.baseType = pr.datatype;
                p.isContainer = true;
                p.isMapContainer = true;
                imports.add(pr.baseType);
            } else {
                Map<PropertyId, Object> args = new HashMap<PropertyId, Object>();
                String format = qp.getFormat();
                args.put(PropertyId.ENUM, qp.getEnum());
                property = PropertyBuilder.build(type, format, args);
            }

            if (property == null) {
                LOGGER.warn("warning!  Property type \"" + type + "\" not found for parameter \"" + param.getName() + "\", using String");
                property = new StringProperty().description("//TODO automatically added by swagger-codegen.  Type was " + type + " but not supported");
            }

            property.setRequired(param.getRequired());
            CodegenProperty cp = fromProperty(qp.getName(), property);

            // set boolean flag (e.g. isString)
            setParameterBooleanFlagWithCodegenProperty(p, cp);

            p.dataType = cp.datatype;
            p.dataFormat = cp.dataFormat;
            if(cp.isEnum) {
                p.datatypeWithEnum = cp.datatypeWithEnum;
                p.enumName = cp.enumName;
            }

            // enum
            updateCodegenPropertyEnum(cp);
            p.isEnum = cp.isEnum;
            p._enum = cp._enum;
            p.allowableValues = cp.allowableValues;


            if (cp.items != null && cp.items.isEnum) {
                p.datatypeWithEnum = cp.datatypeWithEnum;
                p.enumName = cp.enumName;
                p.items = cp.items;
            }
            p.collectionFormat = collectionFormat;
            if(collectionFormat != null && collectionFormat.equals("multi")) {
                p.isCollectionFormatMulti = true;
            }
            p.paramName = toParamName(qp.getName());

            // import
            if (cp.complexType != null) {
                imports.add(cp.complexType);
            }

            // validation
            // handle maximum, minimum properly for int/long by removing the trailing ".0"
            if ("integer".equals(qp.getType())) {
                p.maximum = qp.getMaximum() == null ? null : String.valueOf(qp.getMaximum().longValue());
                p.minimum = qp.getMinimum() == null ? null : String.valueOf(qp.getMinimum().longValue());
            } else {
                p.maximum = qp.getMaximum() == null ? null : String.valueOf(qp.getMaximum());
                p.minimum = qp.getMinimum() == null ? null : String.valueOf(qp.getMinimum());
            }

            p.exclusiveMaximum = qp.isExclusiveMaximum() == null ? false : qp.isExclusiveMaximum();
            p.exclusiveMinimum = qp.isExclusiveMinimum() == null ? false : qp.isExclusiveMinimum();
            p.maxLength = qp.getMaxLength();
            p.minLength = qp.getMinLength();
            p.pattern = toRegularExpression(qp.getPattern());
            p.maxItems = qp.getMaxItems();
            p.minItems = qp.getMinItems();
            p.uniqueItems = qp.isUniqueItems() == null ? false : qp.isUniqueItems();
            p.multipleOf = qp.getMultipleOf();

            // exclusive* are noop without corresponding min/max
            if (p.maximum != null || p.minimum != null ||
                    p.maxLength != null || p.minLength != null ||
                    p.maxItems != null || p.minItems != null ||
                    p.pattern != null) {
                p.hasValidation = true;
            }

        } else {
            if (!(param instanceof BodyParameter)) {
                LOGGER.error("Cannot use Parameter " + param + " as Body Parameter");
            }

            BodyParameter bp = (BodyParameter) param;
            Model model = bp.getSchema();

            if (model instanceof ModelImpl) {
                ModelImpl impl = (ModelImpl) model;
                CodegenModel cm = fromModel(bp.getName(), impl);
                if (!cm.emptyVars) {
                    p.dataType = getTypeDeclaration(cm.classname);
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
                    if (typeMapping.containsKey(name)) {
                        name = typeMapping.get(name);
                    } else {
                        name = toModelName(name);
                        if (defaultIncludes.contains(name)) {
                            imports.add(name);
                        }
                        imports.add(name);
                        name = getTypeDeclaration(name);
                    }
                    p.dataType = name;
                    p.baseType = name;
                }
            }
            p.paramName = toParamName(bp.getName());
        }

        // Issue #2561 (neilotoole) : Set the is<TYPE>Param flags.
        // This code has been moved to here from #fromOperation
        // because these values should be set before calling #postProcessParameter.
        // See: https://github.com/swagger-api/swagger-codegen/issues/2561
        if (param instanceof QueryParameter) {
            p.isQueryParam = true;
        } else if (param instanceof PathParameter) {
            p.required = true;
            p.isPathParam = true;
        } else if (param instanceof HeaderParameter) {
            p.isHeaderParam = true;
        } else if (param instanceof CookieParameter) {
            p.isCookieParam = true;
        } else if (param instanceof BodyParameter) {
            p.isBodyParam = true;
            p.isBinary = isDataTypeBinary(p.dataType);
        } else if (param instanceof FormParameter) {
            if ("file".equalsIgnoreCase(((FormParameter) param).getType()) || "file".equals(p.baseType)) {
                p.isFile = true;
            } else {
                p.notFile = true;
            }
            p.isFormParam = true;
        }

        // set the example value
        // if not specified in x-example, generate a default value
        if (p.vendorExtensions.containsKey("x-example")) {
            p.example = Objects.toString(p.vendorExtensions.get("x-example"));
        } else if (Boolean.TRUE.equals(p.isString)) {
            p.example = p.paramName + "_example";
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            p.example = "true";
        } else if (Boolean.TRUE.equals(p.isLong)) {
            p.example = "789";
        } else if (Boolean.TRUE.equals(p.isInteger)) {
            p.example = "56";
        } else if (Boolean.TRUE.equals(p.isFloat)) {
            p.example = "3.4";
        } else if (Boolean.TRUE.equals(p.isDouble)) {
            p.example = "1.2";
        } else if (Boolean.TRUE.equals(p.isBinary)) {
            p.example = "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(p.isByteArray)) {
            p.example = "B";
        } else if (Boolean.TRUE.equals(p.isFile)) {
            p.example = "/path/to/file.txt";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            p.example = "2013-10-20";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            p.example = "2013-10-20T19:20:30+01:00";
        } else if (Boolean.TRUE.equals(p.isFile)) {
            p.example = "/path/to/file.txt";
        }

        // set the parameter excample value
        // should be overridden by lang codegen
        setParameterExampleValue(p);

        postProcessParameter(p);
        return p;
    }

    public boolean isDataTypeBinary(String dataType) {
        return dataType.toLowerCase().startsWith("byte");
    }

    public boolean isDataTypeFile(String dataType) {
        return dataType.toLowerCase().equals("file");
    }

    /**
     * Convert map of Swagger SecuritySchemeDefinition objects to a list of Codegen Security objects
     *
     * @param schemes a map of Swagger SecuritySchemeDefinition object
     * @return a list of Codegen Security objects
     */
    @SuppressWarnings("static-method")
    public List<CodegenSecurity> fromSecurity(Map<String, SecuritySchemeDefinition> schemes) {
        if (schemes == null) {
            return Collections.emptyList();
        }

        List<CodegenSecurity> secs = new ArrayList<CodegenSecurity>(schemes.size());
        for (Iterator<Map.Entry<String, SecuritySchemeDefinition>> it = schemes.entrySet().iterator(); it.hasNext(); ) {
            final Map.Entry<String, SecuritySchemeDefinition> entry = it.next();
            final SecuritySchemeDefinition schemeDefinition = entry.getValue();

            CodegenSecurity sec = CodegenModelFactory.newInstance(CodegenModelType.SECURITY);
            sec.name = entry.getKey();
            sec.type = schemeDefinition.getType();
            sec.isCode = sec.isPassword = sec.isApplication = sec.isImplicit = false;

            if (schemeDefinition instanceof ApiKeyAuthDefinition) {
                final ApiKeyAuthDefinition apiKeyDefinition = (ApiKeyAuthDefinition) schemeDefinition;
                sec.isBasic = sec.isOAuth = false;
                sec.isApiKey = true;
                sec.keyParamName = apiKeyDefinition.getName();
                sec.isKeyInHeader = apiKeyDefinition.getIn() == In.HEADER;
                sec.isKeyInQuery = !sec.isKeyInHeader;
            } else if(schemeDefinition instanceof BasicAuthDefinition) {
                sec.isKeyInHeader = sec.isKeyInQuery = sec.isApiKey = sec.isOAuth = false;
                sec.isBasic = true;
            } else {
                final OAuth2Definition oauth2Definition = (OAuth2Definition) schemeDefinition;
                sec.isKeyInHeader = sec.isKeyInQuery = sec.isApiKey = sec.isBasic = false;
                sec.isOAuth = true;
                sec.flow = oauth2Definition.getFlow();
                if (sec.flow == null) {
                    throw new RuntimeException("missing oauth flow in " + sec.name);
                }
                switch(sec.flow) {
                    case "accessCode":
                        sec.isCode = true;
                        break;
                    case "password":
                        sec.isPassword = true;
                        break;
                    case "application":
                        sec.isApplication = true;
                        break;
                    case "implicit":
                        sec.isImplicit = true;
                        break;
                    default:
                        throw new RuntimeException("unknown oauth flow: " + sec.flow);
                }
                sec.authorizationUrl = oauth2Definition.getAuthorizationUrl();
                sec.tokenUrl = oauth2Definition.getTokenUrl();
                if (oauth2Definition.getScopes() != null) {
                    List<Map<String, Object>> scopes = new ArrayList<Map<String, Object>>();
                    int count = 0, numScopes = oauth2Definition.getScopes().size();
                    for(Map.Entry<String, String> scopeEntry : oauth2Definition.getScopes().entrySet()) {
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
                    sec.scopes = scopes;
                }
            }

            secs.add(sec);
        }

        // sort auth methods to maintain the same order
        Collections.sort(secs, new Comparator<CodegenSecurity>() {
            @Override
            public int compare(CodegenSecurity one, CodegenSecurity another) {
                return ObjectUtils.compare(one.name, another.name);
            }
        });
        // set 'hasMore'
        Iterator<CodegenSecurity> it = secs.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            security.hasMore = it.hasNext();
        }

        return secs;
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
     * @param operation the operation object
     * @param path the path of the operation
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
        return !defaultIncludes.contains(type)
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

    private void addHeaders(Response response, List<CodegenProperty> target) {
        if (response.getHeaders() != null) {
            for (Map.Entry<String, Property> headers : response.getHeaders().entrySet()) {
                target.add(fromProperty(headers.getKey(), headers.getValue()));
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
     * @param tag name of the tag
     * @param resourcePath path of the resource
     * @param operation Swagger Operation object
     * @param co Codegen Operation object
     * @param operations map of Codegen operations
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
        for(CodegenOperation op : opList) {
            if(uniqueName.equals(op.operationId)) {
                uniqueName = co.operationId + "_" + counter;
                counter ++;
            }
        }
        if(!co.operationId.equals(uniqueName)) {
            LOGGER.warn("generated unique operationId `" + uniqueName + "`");
        }
        co.operationId = uniqueName;
        co.operationIdLowerCase = uniqueName.toLowerCase();
        co.operationIdCamelCase = DefaultCodegen.camelize(uniqueName);
        opList.add(co);
        co.baseName = tag;
    }

    private void addParentContainer(CodegenModel m, String name, Property property) {
        final CodegenProperty tmp = fromProperty(name, property);
        addImport(m, tmp.complexType);
        m.parent = toInstantiationType(property);
        final String containerType = tmp.containerType;
        final String instantiationType = instantiationTypes.get(containerType);
        if (instantiationType != null) {
            addImport(m, instantiationType);
        }
        final String mappedType = typeMapping.get(containerType);
        if (mappedType != null) {
            addImport(m, mappedType);
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
     *   status    => status2
     *   status2   => status3
     *   myName100 => myName101
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

    private void addVars(CodegenModel m, Map<String, Property> properties, List<String> required) {
        addVars(m, properties, required, null, null);
    }

    private void addVars(CodegenModel m, Map<String, Property> properties, List<String> required,
            Map<String, Property> allProperties, List<String> allRequired) {

        m.hasRequired = false;
        if (properties != null && !properties.isEmpty()) {
            m.hasVars = true;
            m.hasEnums = false;


            Set<String> mandatory = required == null ? Collections.<String> emptySet()
                    : new TreeSet<String>(required);
            addVars(m, m.vars, properties, mandatory);
            m.allMandatory = m.mandatory = mandatory;
        } else {
            m.emptyVars = true;
            m.hasVars = false;
            m.hasEnums = false;
        }

        if (allProperties != null) {
            Set<String> allMandatory = allRequired == null ? Collections.<String> emptySet()
                    : new TreeSet<String>(allRequired);
            addVars(m, m.allVars, allProperties, allMandatory);
            m.allMandatory = allMandatory;
        }
    }

    private void addVars(CodegenModel m, List<CodegenProperty> vars, Map<String, Property> properties, Set<String> mandatory) {
        // convert set to list so that we can access the next entry in the loop
        List<Map.Entry<String, Property>> propertyList = new ArrayList<Map.Entry<String, Property>>(properties.entrySet());
        final int totalCount = propertyList.size();
        for (int i = 0; i < totalCount; i++) {
            Map.Entry<String, Property> entry = propertyList.get(i);
            
            final String key = entry.getKey();
            final Property prop = entry.getValue();

            if (prop == null) {
                LOGGER.warn("null property for " + key);
            } else {
                final CodegenProperty cp = fromProperty(key, prop);
                cp.required = mandatory.contains(key) ? true : false;
                m.hasRequired = m.hasRequired || cp.required;
                if (cp.isEnum) {
                    // FIXME: if supporting inheritance, when called a second time for allProperties it is possible for
                    // m.hasEnums to be set incorrectly if allProperties has enumerations but properties does not.
                    m.hasEnums = true;
                }

                // set model's hasOnlyReadOnly to false if the property is read-only
                if (!Boolean.TRUE.equals(cp.isReadOnly)) {
                    m.hasOnlyReadOnly = false;
                }

                if (i+1 != totalCount) {
                    cp.hasMore = true;
                    // check the next entry to see if it's read only
                    if (!Boolean.TRUE.equals(propertyList.get(i+1).getValue().getReadOnly())) {
                        cp.hasMoreNonReadOnly = true; // next entry is not ready only
                    }
                }

                if (cp.isContainer) {
                    addImport(m, typeMapping.get("array"));
                }

                addImport(m, cp.baseType);
                CodegenProperty innerCp = cp;
                while(innerCp != null) {
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
                    m.readWriteVars.add(cp);
                }
            }
        }
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
     * @param name string to be camelize
     * @param nonNameElementPattern a regex pattern of the characters that is not good to be included in name
     * @return camelized string
     */
    protected String removeNonNameElementToCamelCase(final String name, final String nonNameElementPattern) {
        String result = StringUtils.join(Lists.transform(Lists.newArrayList(name.split(nonNameElementPattern)), new Function<String, String>() {
            @Nullable
            @Override
            public String apply(String input) {
                return StringUtils.capitalize(input);
            }
        }), "");
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
     * @param word string to be camelize
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

        // Remove all underscores
        p = Pattern.compile("(_)(.)");
        m = p.matcher(word);
        while (m.find()) {
            word = m.replaceFirst(m.group(2).toUpperCase());
            m = p.matcher(word);
        }

        if (lowercaseFirstLetter && word.length() > 0) {
            word = word.substring(0, 1).toLowerCase() + word.substring(1);
        }

        return word;
    }

    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return apiFileFolder() + '/' + toApiFilename(tag) + suffix;
    }

    /**
     * Return the full path and API documentation file
     *
     * @param templateName template name
     * @param tag tag
     *
     * @return the API documentation file name with full path
     */
    public String apiDocFilename(String templateName, String tag) {
        String suffix = apiDocTemplateFiles().get(templateName);
        return apiDocFileFolder() + '/' + toApiDocFilename(tag) + suffix;
    }

    /**
     * Return the full path and API test file
     *
     * @param templateName template name
     * @param tag tag
     *
     * @return the API test file name with full path
     */
    public String apiTestFilename(String templateName, String tag) {
        String suffix = apiTestTemplateFiles().get(templateName);
        return apiTestFileFolder() + '/' + toApiTestFilename(tag) + suffix;
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

    /**
     * All library templates supported.
     * (key: library name, value: library description)
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
        if (library != null && !supportedLibraries.containsKey(library))
            throw new RuntimeException("unknown library: " + library);
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
            name = Pattern.compile("\\W", Pattern.UNICODE_CHARACTER_CLASS).matcher(name).replaceAll("");
        }
        else {
            name = name.replaceAll("\\W", "");
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
        // remove spaces and make strong case
        String[] parts = tag.split(" ");
        StringBuilder buf = new StringBuilder();
        for (String part : parts) {
            if (StringUtils.isNotEmpty(part)) {
                buf.append(StringUtils.capitalize(part));
            }
        }
        String returnTag = buf.toString().replaceAll("[^a-zA-Z0-9_]", "");
        if (returnTag.matches("\\d.*")) {
            return "_" + returnTag;
        } else {
            return returnTag;
        }
    }

    /**
     * Only write if the file doesn't exist
     *
     * @param outputFolder Output folder
     * @param supportingFile Supporting file
     */
    public void writeOptional(String outputFolder, SupportingFile supportingFile) {
        String folder = "";

        if(outputFolder != null && !"".equals(outputFolder)) {
            folder += outputFolder + File.separator;
        }
        folder += supportingFile.folder;
        if(!"".equals(folder)) {
            folder += File.separator + supportingFile.destinationFilename;
        }
        else {
            folder = supportingFile.destinationFilename;
        }
        if(!new File(folder).exists()) {
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

        if (Boolean.TRUE.equals(property.isString)) {
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
        } else if (Boolean.TRUE.equals(property.isByteArray)) {
            parameter.isByteArray = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isBinary)) {
            parameter.isByteArray = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isFile)) {
            parameter.isFile = true;
            // file is *not* a primitive type
            //parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDate)) {
            parameter.isDate = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDateTime)) {
            parameter.isDateTime = true;
            parameter.isPrimitiveType = true;
        } else {
            LOGGER.debug("Property type is not primitive: " + property.datatype);
        }
    }


    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     * 
     * @param var list of CodegenProperty
     */
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        Map<String, Object> allowableValues = var.allowableValues;

        // handle ArrayProperty
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
            enumVar.put("name", toEnumVarName(enumName, var.datatype));
            enumVar.put("value", toEnumValue(value.toString(), var.datatype));
            enumVars.add(enumVar);
        }
        allowableValues.put("enumVars", enumVars);

        // handle default value for enum, e.g. available => StatusEnum.AVAILABLE
        if (var.defaultValue != null) {
            String enumName = null;
            for (Map<String, String> enumVar : enumVars) {
                if (toEnumValue(var.defaultValue, var.datatype).equals(enumVar.get("value"))) {
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
        if (pattern != null && !pattern.matches("^/.*")) {
            return "/" + pattern + "/";
        }
        return pattern;
    }

    /**
     * reads propertyKey from additionalProperties, converts it to a boolean and
     * writes it back to additionalProperties to be usable as a boolean in
     * mustache files.
     * 
     * @param propertyKey
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
     * Provides an override location, if any is specified, for the .swagger-codegen-ignore.
     *
     * This is originally intended for the first generation only.
     *
     * @return a string of the full path to an override ignore file.
     */
    public String getIgnoreFilePathOverride() {
        return ignoreFilePathOverride;
    }

    /**
     * Sets an override location for the .swagger-codegen.ignore location for the first code generation.
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
}
