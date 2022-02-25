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

import com.fasterxml.jackson.databind.node.ArrayNode;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.*;

public abstract class AbstractKotlinCodegen extends DefaultCodegen implements CodegenConfig {

    public static final String SERIALIZATION_LIBRARY_DESC = "What serialization library to use: 'moshi' (default), or 'gson' or 'jackson'";

    public enum SERIALIZATION_LIBRARY_TYPE {moshi, gson, jackson, kotlinx_serialization}

    public static final String MODEL_MUTABLE = "modelMutable";
    public static final String MODEL_MUTABLE_DESC = "Create mutable models";

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractKotlinCodegen.class);

    protected String artifactId;
    protected String artifactVersion = "1.0.0";
    protected String groupId = "org.openapitools";
    protected String packageName = "org.openapitools";
    protected String apiSuffix = "Api";

    protected String sourceFolder = "src/main/kotlin";
    protected String testFolder = "src/test/kotlin";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean parcelizeModels = false;
    protected boolean serializableModel = false;
    protected boolean needsDataClassBody = false;

    protected boolean nonPublicApi = false;

    protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase;
    protected SERIALIZATION_LIBRARY_TYPE serializationLibrary = SERIALIZATION_LIBRARY_TYPE.moshi;

    // model classes cannot use the same property names defined in HashMap
    // ref: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/-hash-map/
    protected Set<String> propertyAdditionalKeywords = new HashSet<>(Arrays.asList("entries", "keys", "size", "values"));

    public AbstractKotlinCodegen() {
        super();

        supportsInheritance = true;
        setSortModelPropertiesByRequiredFlag(true);

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                "kotlin.Byte",
                "kotlin.ByteArray",
                "kotlin.Short",
                "kotlin.Int",
                "kotlin.Long",
                "kotlin.Float",
                "kotlin.Double",
                "kotlin.Boolean",
                "kotlin.Char",
                "kotlin.String",
                "kotlin.Array",
                "kotlin.collections.List",
                "kotlin.collections.MutableList",
                "kotlin.collections.Map",
                "kotlin.collections.MutableMap",
                "kotlin.collections.Set",
                "kotlin.collections.MutableSet"
        ));

        // this includes hard reserved words defined by https://github.com/JetBrains/kotlin/blob/master/core/descriptors/src/org/jetbrains/kotlin/renderer/KeywordStringsGenerated.java
        // as well as keywords from https://kotlinlang.org/docs/reference/keyword-reference.html
        reservedWords = new HashSet<>(Arrays.asList(
                "ApiResponse", // Used in the auto-generated api client
                "abstract",
                "actual",
                "annotation",
                "as",
                "break",
                "class",
                "companion",
                "const",
                "constructor",
                "continue",
                "crossinline",
                "data",
                "delegate",
                "do",
                "dynamic",
                "else",
                "enum",
                "expect",
                "external",
                "false",
                "field",
                "final",
                "finally",
                "for",
                "fun",
                "if",
                "import",
                "in",
                "infix",
                "init",
                "inline",
                "inner",
                "interface",
                "internal",
                "is",
                "it",
                "lateinit",
                "noinline",
                "null",
                "object",
                "open",
                "operator",
                "out",
                "override",
                "package",
                "param",
                "private",
                "property",
                "protected",
                "public",
                "receiver",
                "reified",
                "return",
                "sealed",
                "setparam",
                "super",
                "suspend",
                "tailrec",
                "this",
                "throw",
                "true",
                "try",
                "typealias",
                "typeof",
                "val",
                "value",
                "var",
                "vararg",
                "when",
                "where",
                "while"
        ));

        defaultIncludes = new HashSet<>(Arrays.asList(
                "kotlin.Byte",
                "kotlin.ByteArray",
                "kotlin.Short",
                "kotlin.Int",
                "kotlin.Long",
                "kotlin.Float",
                "kotlin.Double",
                "kotlin.Boolean",
                "kotlin.Char",
                "kotlin.Array",
                "kotlin.collections.List",
                "kotlin.collections.MutableList",
                "kotlin.collections.Set",
                "kotlin.collections.MutableSet",
                "kotlin.collections.Map",
                "kotlin.collections.MutableMap"
        ));

        typeMapping = new HashMap<>();
        typeMapping.put("string", "kotlin.String");
        typeMapping.put("boolean", "kotlin.Boolean");
        typeMapping.put("integer", "kotlin.Int");
        typeMapping.put("float", "kotlin.Float");
        typeMapping.put("long", "kotlin.Long");
        typeMapping.put("double", "kotlin.Double");
        typeMapping.put("ByteArray", "kotlin.ByteArray");
        typeMapping.put("number", "java.math.BigDecimal");
        typeMapping.put("decimal", "java.math.BigDecimal");
        typeMapping.put("date-time", "java.time.OffsetDateTime");
        typeMapping.put("date", "java.time.LocalDate");
        typeMapping.put("file", "java.io.File");
        typeMapping.put("array", "kotlin.Array");
        typeMapping.put("list", "kotlin.collections.List");
        typeMapping.put("set", "kotlin.collections.Set");
        typeMapping.put("map", "kotlin.collections.Map");
        typeMapping.put("object", "kotlin.Any");
        typeMapping.put("binary", "kotlin.ByteArray");
        typeMapping.put("Date", "java.time.LocalDate");
        typeMapping.put("DateTime", "java.time.OffsetDateTime");
        typeMapping.put("AnyType", "kotlin.Any");

        instantiationTypes.put("array", "kotlin.collections.ArrayList");
        instantiationTypes.put("list", "kotlin.collections.ArrayList");
        instantiationTypes.put("map", "kotlin.collections.HashMap");

        importMapping = new HashMap<>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.time.LocalDate");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("DateTime", "java.time.OffsetDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");

        specialCharReplacements.put(";", "Semicolon");

        cliOptions.clear();
        addOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC, sourceFolder);
        addOption(CodegenConstants.PACKAGE_NAME, "Generated artifact package name.", packageName);
        addOption(CodegenConstants.API_SUFFIX, CodegenConstants.API_SUFFIX_DESC, apiSuffix);
        addOption(CodegenConstants.GROUP_ID, "Generated artifact package's organization (i.e. maven groupId).", groupId);
        addOption(CodegenConstants.ARTIFACT_ID, "Generated artifact id (name of jar).", artifactId);
        addOption(CodegenConstants.ARTIFACT_VERSION, "Generated artifact's package version.", artifactVersion);

        CliOption enumPropertyNamingOpt = new CliOption(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_DESC);
        cliOptions.add(enumPropertyNamingOpt.defaultValue(enumPropertyNaming.name()));

        CliOption serializationLibraryOpt = new CliOption(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_DESC);
        cliOptions.add(serializationLibraryOpt.defaultValue(serializationLibrary.name()));

        cliOptions.add(new CliOption(CodegenConstants.PARCELIZE_MODELS, CodegenConstants.PARCELIZE_MODELS_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SERIALIZABLE_MODEL, CodegenConstants.SERIALIZABLE_MODEL_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG_DESC));

        cliOptions.add(CliOption.newBoolean(MODEL_MUTABLE, MODEL_MUTABLE_DESC, false));
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + File.separator + testFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeReservedWord(String name) {
        // TODO: Allow enum escaping as an option (e.g. backticks vs append/prepend underscore vs match model property escaping).
        return String.format(Locale.ROOT, "`%s`", name);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    public CodegenConstants.ENUM_PROPERTY_NAMING_TYPE getEnumPropertyNaming() {
        return this.enumPropertyNaming;
    }

    public SERIALIZATION_LIBRARY_TYPE getSerializationLibrary() {
        return this.serializationLibrary;
    }

    /**
     * Sets the naming convention for Kotlin enum properties
     *
     * @param enumPropertyNamingType The string representation of the naming convention, as defined by {@link org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE}
     */
    public void setEnumPropertyNaming(final String enumPropertyNamingType) {
        try {
            this.enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.valueOf(enumPropertyNamingType);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumPropertyNamingType + " is an invalid enum property naming option. Please choose from:");
            for (CodegenConstants.ENUM_PROPERTY_NAMING_TYPE t : CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }

    /**
     * Sets the serialization engine for Kotlin
     *
     * @param enumSerializationLibrary The string representation of the serialization library as defined by
     *                                 {@link org.openapitools.codegen.languages.AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE}
     */
    public void setSerializationLibrary(final String enumSerializationLibrary) {
        try {
            this.serializationLibrary = SERIALIZATION_LIBRARY_TYPE.valueOf(enumSerializationLibrary);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumSerializationLibrary + " is an invalid enum property naming option. Please choose from:");
            for (SERIALIZATION_LIBRARY_TYPE t : SERIALIZATION_LIBRARY_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }

    /**
     * returns the OpenAPI type for the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the type
     **/
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        // This maps, for example, long -> kotlin.Long based on hashes in this type's constructor
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p OpenAPI Property object
     * @return a string presentation of the property type
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = ModelUtils.unaliasSchema(this.openAPI, p, importMapping);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        if (ModelUtils.isArraySchema(target)) {
            Schema<?> items = getSchemaItems((ArraySchema) schema);
            return getSchemaType(target) + "<" + getTypeDeclaration(items) + ">";
        } else if (ModelUtils.isMapSchema(target)) {
            // Note: ModelUtils.isMapSchema(p) returns true when p is a composed schema that also defines
            // additionalproperties: true
            Schema<?> inner = getAdditionalProperties(target);
            if (inner == null) {
                LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
                p.setAdditionalProperties(inner);
            }
            return getSchemaType(target) + "<kotlin.String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(target);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            if (cm.getDiscriminator() != null) {
                cm.vendorExtensions.put("x-has-data-class-body", true);
                break;
            }

            for (CodegenProperty var : cm.vars) {
                if (var.isEnum || isSerializableModel()) {
                    cm.vendorExtensions.put("x-has-data-class-body", true);
                    break;
                }
            }
        }
        return postProcessModelsEnum(objs);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("KOTLIN_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable KOTLIN_POST_PROCESS_FILE not defined so the Kotlin code may not be properly formatted. To define it, try 'export KOTLIN_POST_PROCESS_FILE=\"/usr/local/bin/ktlint -F\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(MODEL_MUTABLE)) {
            additionalProperties.put(MODEL_MUTABLE, Boolean.parseBoolean(additionalProperties.get(MODEL_MUTABLE).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_PROPERTY_NAMING)) {
            setEnumPropertyNaming((String) additionalProperties.get(CodegenConstants.ENUM_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary((String) additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY));
            additionalProperties.put(this.serializationLibrary.name(), true);
        } else {
            additionalProperties.put(this.serializationLibrary.name(), true);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            this.setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE))
                this.setModelPackage(packageName + ".models");
            if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE))
                this.setApiPackage(packageName + ".apis");
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.API_SUFFIX)) {
            this.setApiSuffix((String) additionalProperties.get(CodegenConstants.API_SUFFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.setArtifactId((String) additionalProperties.get(CodegenConstants.ARTIFACT_ID));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.GROUP_ID)) {
            this.setGroupId((String) additionalProperties.get(CodegenConstants.GROUP_ID));
        } else {
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            LOGGER.warn("{} with {} generator is ignored. Use {}.", CodegenConstants.INVOKER_PACKAGE, this.getName(), CodegenConstants.PACKAGE_NAME);
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZABLE_MODEL)) {
            this.setSerializableModel(convertPropertyToBooleanAndWriteBack(CodegenConstants.SERIALIZABLE_MODEL));
        } else {
            additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, serializableModel);
        }

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if (additionalProperties.containsKey(CodegenConstants.PARCELIZE_MODELS)) {
            this.setParcelizeModels(convertPropertyToBooleanAndWriteBack(CodegenConstants.PARCELIZE_MODELS));
        } else {
            additionalProperties.put(CodegenConstants.PARCELIZE_MODELS, parcelizeModels);
        }

        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            this.setNonPublicApi(convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API));
        } else {
            additionalProperties.put(CodegenConstants.NON_PUBLIC_API, nonPublicApi);
        }

        additionalProperties.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, getSortParamsByRequiredFlag());
        additionalProperties.put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, getSortModelPropertiesByRequiredFlag());

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (isModelMutable()) {
            typeMapping.put("list", "kotlin.collections.MutableList");
            typeMapping.put("set", "kotlin.collections.MutableSet");
            typeMapping.put("map", "kotlin.collections.MutableMap");
        }
    }

    protected boolean isModelMutable() {
        return Boolean.TRUE.equals(additionalProperties.get(MODEL_MUTABLE));
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setApiSuffix(String apiSuffix) {
        this.apiSuffix = apiSuffix;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }

    public Boolean getParcelizeModels() {
        return parcelizeModels;
    }

    public void setParcelizeModels(Boolean parcelizeModels) {
        this.parcelizeModels = parcelizeModels;
    }

    public boolean isSerializableModel() {
        return serializableModel;
    }

    public void setSerializableModel(boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    public boolean nonPublicApi() {
        return nonPublicApi;
    }

    public void setNonPublicApi(boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
    }

    public boolean isNeedsDataClassBody() {
        return needsDataClassBody;
    }

    public void setNeedsDataClassBody(boolean needsDataClassBody) {
        this.needsDataClassBody = needsDataClassBody;
    }

    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    @Override
    public String toEnumVarName(String value, String datatype) {
        String modified;
        if (value.length() == 0) {
            modified = "EMPTY";
        } else {
            modified = value;
            modified = sanitizeKotlinSpecificNames(modified);
        }

        switch (getEnumPropertyNaming()) {
            case original:
                // NOTE: This is provided as a last-case allowance, but will still result in reserved words being escaped.
                modified = value;
                break;
            case camelCase:
                // NOTE: Removes hyphens and underscores
                modified = camelize(modified, true);
                break;
            case PascalCase:
                // NOTE: Removes hyphens and underscores
                String result = camelize(modified);
                modified = titleCase(result);
                break;
            case snake_case:
                // NOTE: Removes hyphens
                modified = underscore(modified);
                break;
            case UPPERCASE:
                modified = underscore(modified).toUpperCase(Locale.ROOT);
                break;
        }

        if (reservedWords.contains(modified)) {
            return escapeReservedWord(modified);
        }
        // NOTE: another sanitize because camelize can create an invalid name
        return sanitizeKotlinSpecificNames(modified);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return property.nameInCamelCase;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return (this.apiSuffix.isEmpty() ? camelize(name) : camelize(name) + this.apiSuffix);
    }

    /**
     * Return the fully-qualified "Model" name for import
     *
     * @param name the name of the "Model"
     * @return the fully-qualified "Model" name for import
     */
    @Override
    public String toModelImport(String name) {
        // toModelImport is called while processing operations, but DefaultCodegen doesn't
        // define imports correctly with fully qualified primitives and models as defined in this generator.
        if (needToImport(name)) {
            return super.toModelImport(name);
        }

        return name;
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(final String name) {

        // Allow for explicitly configured kotlin.* and java.* types
        if (name.startsWith("kotlin.") || name.startsWith("java.")) {
            return name;
        }

        // If importMapping contains name, assume this is a legitimate model name.
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        }

        String modifiedName = name.replaceAll("\\.", "");
        String sanitizedName = sanitizeKotlinSpecificNames(modifiedName);

        String nameWithPrefixSuffix = sanitizedName;
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
        }

        // Camelize name of nested properties
        modifiedName = camelize(nameWithPrefixSuffix);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(modifiedName)) {
            final String modelName = "Model" + modifiedName;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", modifiedName, modelName);
            return modelName;
        }

        // model name starts with number
        if (modifiedName.matches("^\\d.*")) {
            final String modelName = "Model" + modifiedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return titleCase(modifiedName);
    }

    /**
     * Return the operation ID (method name)
     *
     * @param operationId operation ID
     * @return the sanitized method name
     */
    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId))
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method sname. Renamed to " + camelize("call_" + operationId), true);
            operationId = camelize("call_" + operationId, true);
        }

        return operationId;
    }

    @Override
    public String toModelFilename(String name) {
        // Should be the same as the model name
        return toModelName(name);
    }

    /**
     * Sanitize against Kotlin specific naming conventions, which may differ from those required by {@link DefaultCodegen#sanitizeName}.
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    private String sanitizeKotlinSpecificNames(final String name) {
        String word = name;
        for (Map.Entry<String, String> specialCharacters : specialCharReplacements.entrySet()) {
            word = replaceSpecialCharacters(word, specialCharacters);
        }

        // Fallback, replace unknowns with underscore.
        word = word.replaceAll("\\W+", "_");
        if (word.matches("\\d.*")) {
            word = "_" + word;
        }

        // _, __, and ___ are reserved in Kotlin. Treat all names with only underscores consistently, regardless of count.
        if (word.matches("^_*$")) {
            word = word.replaceAll("\\Q_\\E", "Underscore");
        }

        return word;
    }

    private String replaceSpecialCharacters(String word, Map.Entry<String, String> specialCharacters) {
        String specialChar = specialCharacters.getKey();
        String replacementChar = specialCharacters.getValue();
        // Underscore is the only special character we'll allow
        if (!specialChar.equals("_") && word.contains(specialChar)) {
            return replaceCharacters(word, specialChar, replacementChar);
        }
        return word;
    }

    private String replaceCharacters(String word, String oldValue, String newValue) {
        if (!word.contains(oldValue)) {
            return word;
        }
        if (word.equals(oldValue)) {
            return newValue;
        }
        int i = word.indexOf(oldValue);
        String start = word.substring(0, i);
        String end = recurseOnEndOfWord(word, oldValue, newValue, i);
        return start + newValue + end;
    }

    private String recurseOnEndOfWord(String word, String oldValue, String newValue, int lastReplacedValue) {
        String end = word.substring(lastReplacedValue + 1);
        if (!end.isEmpty()) {
            end = titleCase(end);
            end = replaceCharacters(end, oldValue, newValue);
        }
        return end;
    }

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase(Locale.ROOT) + input.substring(1);
    }

    @Override
    protected boolean isReservedWord(String word) {
        // We want case-sensitive escaping, to avoid unnecessary backtick-escaping.
        return reservedWords.contains(word);
    }

    /**
     * Check the type to see if it needs import the library/module/package
     *
     * @param type name of the type
     * @return true if the library/module/package of the corresponding type needs to be imported
     */
    @Override
    protected boolean needToImport(String type) {
        // provides extra protection against improperly trying to import language primitives and java types
        boolean imports = !type.startsWith("kotlin.") && !type.startsWith("java.") &&
                !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type) &&
                !type.contains(".");
        return imports;
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        CodegenModel m = super.fromModel(name, schema);
        m.optionalVars = m.optionalVars.stream().distinct().collect(Collectors.toList());
        // Update allVars/requiredVars/optionalVars with isInherited
        // Each of these lists contains elements that are similar, but they are all cloned
        // via CodegenModel.removeAllDuplicatedProperty and therefore need to be updated
        // separately.
        // First find only the parent vars via baseName matching
        Map<String, CodegenProperty> allVarsMap = m.allVars.stream()
                .collect(Collectors.toMap(CodegenProperty::getBaseName, Function.identity()));
        allVarsMap.keySet()
                .removeAll(m.vars.stream().map(CodegenProperty::getBaseName).collect(Collectors.toSet()));
        // Update the allVars
        allVarsMap.values().forEach(p -> p.isInherited = true);
        // Update any other vars (requiredVars, optionalVars)
        Stream.of(m.requiredVars, m.optionalVars)
                .flatMap(List::stream)
                .filter(p -> allVarsMap.containsKey(p.baseName))
                .forEach(p -> p.isInherited = true);
        return m;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("kotlin.Int".equals(datatype) || "kotlin.Long".equals(datatype)) {
            return value;
        } else if ("kotlin.Double".equals(datatype)) {
            if (value.contains(".")) {
                return value;
            } else {
                return value + ".0"; // Float and double must have .0
            }
        } else if ("kotlin.Float".equals(datatype)) {
            return value + "f";
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public boolean isDataTypeString(final String dataType) {
        return "String".equals(dataType) || "kotlin.String".equals(dataType);
    }

    @Override
    public String toParamName(String name) {
        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "paramCallback";
        }

        // should be the same as variable name
        return toVariableName(name);
    }

    @Override
    public String toVarName(String name) {
        name = toVariableName(name);
        if (propertyAdditionalKeywords.contains(name)) {
            return camelize("property_" + name, true);
        } else {
            return name;
        }
    }

    protected String toVariableName(String name) {
        // sanitize name
        name = sanitizeName(name, "\\W-[\\$]");
        name = sanitizeKotlinSpecificNames(name);

        if (name.toLowerCase(Locale.ROOT).matches("^_*class$")) {
            return "propertyClass";
        }

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z0-9_]*$")) {
            return name;
        }

        if (startsWithTwoUppercaseLetters(name)) {
            name = name.substring(0, 2).toLowerCase(Locale.ROOT) + name.substring(2);
        }

        // If name contains special chars -> replace them.
        if ((name.chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character))))) {
            List<String> allowedCharacters = new ArrayList<>();
            allowedCharacters.add("_");
            allowedCharacters.add("$");
            name = escape(name, specialCharReplacements, allowedCharacters, "_");
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number or containing dollar symbol, escape it
        if (isReservedWord(name) || name.matches("(^\\d.*)|(.*[$].*)")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toRegularExpression(String pattern) {
        return escapeText(pattern);
    }

    private boolean startsWithTwoUppercaseLetters(String name) {
        boolean startsWithTwoUppercaseLetters = false;
        if (name.length() > 1) {
            startsWithTwoUppercaseLetters = name.substring(0, 2).equals(name.substring(0, 2).toUpperCase(Locale.ROOT));
        }
        return startsWithTwoUppercaseLetters;
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String kotlinPostProcessFile = System.getenv("KOTLIN_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(kotlinPostProcessFile)) {
            return; // skip if KOTLIN_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with kt extension
        if ("kt".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = kotlinPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                p.waitFor();
                int exitValue = p.exitValue();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
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
    public String toDefaultValue(Schema schema) {
        Schema p = ModelUtils.getReferencedSchema(this.openAPI, schema);
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isURISchema(p)) {
            if (p.getDefault() != null) {
                return "URI.create('" + p.getDefault() + "')";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                String arrInstantiationType = ModelUtils.isSet(p) ? "set" : "arrayList";

                ArrayNode _default = (ArrayNode) p.getDefault();
                if (_default.isEmpty()) {
                    return arrInstantiationType + "Of()";
                }

                StringBuilder defaultContent = new StringBuilder();
                Schema<?> itemsSchema = getSchemaItems((ArraySchema) schema);
                _default.elements().forEachRemaining((element) -> {
                    itemsSchema.setDefault(element.asText());
                    defaultContent.append(toDefaultValue(itemsSchema)).append(",");
                });
                defaultContent.deleteCharAt(defaultContent.length() - 1); // remove trailing comma
                return arrInstantiationType + "Of(" + defaultContent + ")";
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String _default = (String) p.getDefault();
                if (p.getEnum() == null) {
                    return "\"" + escapeText(_default) + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return _default;
                }
            }
            return null;
        }

        return null;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.KOTLIN;
    }
}
