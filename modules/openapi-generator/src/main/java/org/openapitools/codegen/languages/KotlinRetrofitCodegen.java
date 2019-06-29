/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class KotlinRetrofitCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(KotlinRetrofitCodegen.class);

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String COLLECTION_TYPE = "collectionType";

    protected String artifactId = "kotlin-retrofit";
    protected String artifactVersion = "1.0.0";
    protected String groupId = "org.openapitools";
    protected String packageName = "org.openapitools.client";
    protected String apiSuffix = "Api";

    protected String sourceFolder = "src/main/java";
    protected String testFolder = "src/test/java";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected boolean parcelizeModels = false;
    protected String dateLibrary = DateLibrary.STRING.value;
    protected String collectionType = CollectionType.ARRAY.value;
    protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.PascalCase;

    public enum DateLibrary {
        STRING("string"),
        THREETENBP("threetenbp"),
        JAVA8("java8");

        public final String value;

        DateLibrary(String value) {
            this.value = value;
        }
    }

    public enum CollectionType {
        ARRAY("array"),
        LIST("list");

        public final String value;

        CollectionType(String value) {
            this.value = value;
        }
    }

    /**
     * Constructs an instance of `KotlinRetrofitCodegen`.
     */
    public KotlinRetrofitCodegen() {
        supportsInheritance = true;
        specialCharReplacements.put(";", "Semicolon");

        initPrimitives();
        initReservedWords();
        initTypeMapping();
        initInstantiationTypes();
        initImportMapping();

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);

        outputFolder = "generated-code" + File.separator + "kotlin-retrofit";
        embeddedTemplateDir = templateDir = "kotlin-retrofit";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        initCliOptions();
    }

    private void initPrimitives() {
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
                "kotlin.collections.Map",
                "kotlin.collections.Set"
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
                "kotlin.String",
                "kotlin.Array",
                "kotlin.collections.List",
                "kotlin.collections.Set",
                "kotlin.collections.Map"
        ));
    }

    private void initTypeMapping() {
        typeMapping = new HashMap<>();
        typeMapping.put("object", "kotlin.Any");

        typeMapping.put("string", "kotlin.String");
        typeMapping.put("boolean", "kotlin.Boolean");
        typeMapping.put("integer", "kotlin.Int");
        typeMapping.put("long", "kotlin.Long");
        typeMapping.put("float", "kotlin.Float");
        typeMapping.put("double", "kotlin.Double");
        typeMapping.put("number", "java.math.BigDecimal");

        typeMapping.put("array", "kotlin.Array");
        typeMapping.put("list", "kotlin.collections.List");
        typeMapping.put("map", "kotlin.collections.Map");

        typeMapping.put("ByteArray", "kotlin.ByteArray");
        typeMapping.put("file", "java.io.File");
        typeMapping.put("binary", "kotlin.ByteArray");

        typeMapping.put("date-time", "java.time.LocalDateTime");
        typeMapping.put("date", "java.time.LocalDate");
        typeMapping.put("Date", "java.time.LocalDate");
        typeMapping.put("DateTime", "java.time.LocalDateTime");
    }

    private void initInstantiationTypes() {
        instantiationTypes.put("array", "kotlin.arrayOf");
        instantiationTypes.put("list", "kotlin.arrayOf");
        instantiationTypes.put("map", "kotlin.mapOf");
    }

    private void initImportMapping() {
        importMapping = new HashMap<>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
    }

    private void initReservedWords() {
        // this includes hard reserved words defined by https://github.com/JetBrains/kotlin/blob/master/core/descriptors/src/org/jetbrains/kotlin/renderer/KeywordStringsGenerated.java
        // DOES NOT contain keywords from https://kotlinlang.org/docs/reference/keyword-reference.html
        // because 'val data: String' is valid, but not 'val fun: String'
        // also valid: 'val override: Int = 2'
        reservedWords = new HashSet<>(Arrays.asList(
                "package",
                "as",
                "typealias",
                "class",
                "this",
                "super",
                "val",
                "var",
                "fun",
                "for",
                "null",
                "true",
                "false",
                "is",
                "in",
                "throw",
                "return",
                "break",
                "continue",
                "object",
                "if",
                "try",
                "else",
                "while",
                "do",
                "when",
                "interface",
                "typeof"
        ));
    }

    private void initCliOptions() {
        cliOptions.clear();
        addOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC, sourceFolder);
        addOption(CodegenConstants.PACKAGE_NAME, "Generated artifact package name.", packageName);
        addOption(CodegenConstants.API_SUFFIX, CodegenConstants.API_SUFFIX_DESC, apiSuffix);
        addOption(CodegenConstants.GROUP_ID, "Generated artifact package's organization (i.e. maven groupId).", groupId);
        addOption(CodegenConstants.ARTIFACT_ID, "Generated artifact id (name of jar).", artifactId);
        addOption(CodegenConstants.ARTIFACT_VERSION, "Generated artifact's package version.", artifactVersion);

        CliOption enumPropertyNamingOpt = new CliOption(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_DESC);
        cliOptions.add(enumPropertyNamingOpt.defaultValue(enumPropertyNaming.name()));
        cliOptions.add(new CliOption(CodegenConstants.PARCELIZE_MODELS, CodegenConstants.PARCELIZE_MODELS_DESC));

        CliOption dateLibrary = new CliOption(DATE_LIBRARY, "Option. Date library to use");
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DateLibrary.THREETENBP.value, "Threetenbp");
        dateOptions.put(DateLibrary.STRING.value, "String");
        dateOptions.put(DateLibrary.JAVA8.value, "Java 8 native JSR310");
        dateLibrary.setEnum(dateOptions);
        dateLibrary.setDefault(this.dateLibrary);
        cliOptions.add(dateLibrary);

        CliOption collectionType = new CliOption(COLLECTION_TYPE, "Option. Collection type to use");
        Map<String, String> collectionOptions = new HashMap<>();
        collectionOptions.put(CollectionType.ARRAY.value, "kotlin.Array");
        collectionOptions.put(CollectionType.LIST.value, "kotlin.collections.List");
        collectionType.setEnum(collectionOptions);
        collectionType.setDefault(this.collectionType);
        cliOptions.add(collectionType);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "kotlin-retrofit";
    }

    @Override
    public String getHelp() {
        return "Generates an android library module. Uses Kotlin, Retrofit and Coroutines.";
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "\\\"");
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

    //<editor-fold desc="Processing">

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("KOTLIN_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable KOTLIN_POST_PROCESS_FILE not defined so the Kotlin code may not be properly formatted. To define it, try 'export KOTLIN_POST_PROCESS_FILE=\"/usr/local/bin/ktlint -F\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_PROPERTY_NAMING)) {
            setEnumPropertyNaming((String) additionalProperties.get(CodegenConstants.ENUM_PROPERTY_NAMING));
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
            LOGGER.warn(CodegenConstants.INVOKER_PACKAGE + " with " + this.getName() + " generator is ignored. Use " + CodegenConstants.PACKAGE_NAME + ".");
        }

        if (additionalProperties.containsKey(CodegenConstants.PARCELIZE_MODELS)) {
            this.setParcelizeModels(Boolean.valueOf((String) additionalProperties.get(CodegenConstants.PARCELIZE_MODELS)));
            LOGGER.info(CodegenConstants.PARCELIZE_MODELS + " depends on the android framework and " +
                    "experimental parcelize feature. Make sure your build applies the android plugin:\n" +
                    "apply plugin: 'com.android.library' OR apply plugin: 'com.android.application'.\n" +
                    "and enables the experimental features:\n" +
                    "androidExtensions {\n" +
                    "    experimental = true\n" +
                    "}"
            );
        } else {
            additionalProperties.put(CodegenConstants.PARCELIZE_MODELS, parcelizeModels);
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());
        }

        if (DateLibrary.THREETENBP.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.THREETENBP.value, true);
            typeMapping.put("date", "LocalDate");
            typeMapping.put("DateTime", "LocalDateTime");
            importMapping.put("LocalDate", "org.threeten.bp.LocalDate");
            importMapping.put("LocalDateTime", "org.threeten.bp.LocalDateTime");
            defaultIncludes.add("org.threeten.bp.LocalDate");
            defaultIncludes.add("org.threeten.bp.LocalDateTime");
        } else if (DateLibrary.STRING.value.equals(dateLibrary)) {
            typeMapping.put("date-time", "kotlin.String");
            typeMapping.put("date", "kotlin.String");
            typeMapping.put("Date", "kotlin.String");
            typeMapping.put("DateTime", "kotlin.String");
        } else if (DateLibrary.JAVA8.value.equals(dateLibrary)) {
            additionalProperties.put(DateLibrary.JAVA8.value, true);
        }

        if (additionalProperties.containsKey(COLLECTION_TYPE)) {
            setCollectionType(additionalProperties.get(COLLECTION_TYPE).toString());
        }

        if (CollectionType.LIST.value.equals(collectionType)) {
            typeMapping.put("array", "kotlin.collections.List");
            typeMapping.put("list", "kotlin.collections.List");
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName).replace(".", "/");

        supportingFiles.add(new SupportingFile("AuthInterceptors.kt.mustache", infrastructureFolder, "AuthInterceptors.kt"));
        supportingFiles.add(new SupportingFile("RetrofitHolder.kt.mustache", infrastructureFolder, "RetrofitHolder.kt"));

        supportingFiles.add(new SupportingFile("proguard-rules.pro.mustache", "", "proguard-rules.pro"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        supportingFiles.add(new SupportingFile("AndroidManifest.xml.mustache", "src/main", "AndroidManifest.xml"));
    }

    //</editor-fold>

    //<editor-fold desc="Name Generators">

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase(Locale.ROOT) + input.substring(1);
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

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        }
        return super.toInstantiationType(p);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return (this.apiSuffix.isEmpty() ? camelize(name) : camelize(name) + this.apiSuffix);
    }

    @Override
    public String toModelImport(String name) {
        // toModelImport is called while processing operations, but DefaultCodegen doesn't
        // define imports correctly with fully qualified primitives and models as defined in this generator.
        if (needToImport(name)) {
            return super.toModelImport(name);
        }

        return name;
    }

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

        String nameWithPrefixSuffix = sanitizeKotlinSpecificNames(modifiedName);
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
            LOGGER.warn(modifiedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (modifiedName.matches("^\\d.*")) {
            final String modelName = "Model" + modifiedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return titleCase(modifiedName);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId))
            throw new RuntimeException("Empty method/operation name (operationId) not allowed");

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + camelize("call_" + operationId), true);
            operationId = camelize("call_" + operationId, true);
        }

        return operationId;
    }

    @Override
    public String toModelFilename(String name) {
        // Should be the same as the model name
        return toModelName(name);
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
    public String toEnumVarName(String value, String datatype) {
        String modified;
        if (value.length() == 0) {
            modified = "EMPTY";
        } else {
            modified = value;
            modified = sanitizeKotlinSpecificNames(modified);
        }

        switch (enumPropertyNaming) {
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
                modified = modified.toUpperCase(Locale.ROOT);
                break;
        }

        if (reservedWords.contains(modified)) {
            return escapeReservedWord(modified);
        }
        // NOTE: another sanitize because camelize can create an invalid name
        return sanitizeKotlinSpecificNames(modified);
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return value;
    }

    @Override
    public String toParamName(String name) {
        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "paramCallback";
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeKotlinSpecificNames(name);
        name = sanitizeName(name, "\\W-[\\$]");

        if (name.toLowerCase(Locale.ROOT).matches("^_*class$")) {
            return "propertyClass";
        }

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z0-9_]*$")) {
            return name;
        }

        if (startsWithTwoUppercaseLetters(name)) {
            name = name.substring(0, 2).toLowerCase(Locale.ROOT) + name.substring(2);
        }

        // If name contains special chars -> replace them.
        if ((name.chars().anyMatch(character -> specialCharReplacements.keySet().contains("" + ((char) character))))) {
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

    @Override
    public String toDefaultValue(Schema p) {
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
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault() + "\"";
            }
        }

        return null;
    }

    //</editor-fold>

    //<editor-fold desc="Type Mapping">

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

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return getArrayTypeDeclaration((ArraySchema) p);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);

            // Maps will be keyed only by primitive Kotlin string
            return getSchemaType(p) + "<kotlin.String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    /**
     * Provides a strongly typed declaration for simple arrays of some type and arrays of arrays of some type.
     *
     * @param arr Array schema
     * @return type declaration of array
     */
    private String getArrayTypeDeclaration(ArraySchema arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Schema items = arr.getItems();
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        instantiationType.append("<").append(nestedType).append(">");
        return instantiationType.toString();
    }

    //</editor-fold>

    //<editor-fold desc="Getter/Setter">

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

    public void setParcelizeModels(Boolean parcelizeModels) {
        this.parcelizeModels = parcelizeModels;
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

    public void setDateLibrary(String library) {
        dateLibrary = library;
    }

    public void setCollectionType(String collectionType) {
        this.collectionType = collectionType;
    }

    //</editor-fold>

    //<editor-fold desc="Helpers">

    @Override
    public boolean isDataTypeString(final String dataType) {
        return "String".equals(dataType) || "kotlin.String".equals(dataType);
    }

    @Override
    protected boolean isReservedWord(String word) {
        // We want case-sensitive escaping, to avoid unnecessary backtick-escaping.
        return reservedWords.contains(word);
    }

    @Override
    protected boolean needToImport(String type) {
        // provides extra protection against improperly trying to import language primitives and java types
        return !type.startsWith("kotlin.") &&
                !type.startsWith("java.") &&
                !defaultIncludes.contains(type) &&
                !languageSpecificPrimitives.contains(type);
    }

    private boolean startsWithTwoUppercaseLetters(String name) {
        boolean startsWithTwoUppercaseLetters = false;
        if (name.length() > 1) {
            startsWithTwoUppercaseLetters = name.substring(0, 2).equals(name.substring(0, 2).toUpperCase(Locale.ROOT));
        }
        return startsWithTwoUppercaseLetters;
    }

    private String recurseOnEndOfWord(String word, String oldValue, String newValue, int lastReplacedValue) {
        String end = word.substring(lastReplacedValue + 1);
        if (!end.isEmpty()) {
            end = titleCase(end);
            end = replaceCharacters(end, oldValue, newValue);
        }
        return end;
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

    private String replaceSpecialCharacters(String word, Map.Entry<String, String> specialCharacters) {
        String specialChar = specialCharacters.getKey();
        String replacementChar = specialCharacters.getValue();
        // Underscore is the only special character we'll allow
        if (!specialChar.equals("_") && word.contains(specialChar)) {
            return replaceCharacters(word, specialChar, replacementChar);
        }
        return word;
    }

    //</editor-fold>

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
            String command = kotlinPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                p.waitFor();
                int exitValue = p.exitValue();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations == null) return objs;

        List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation operation : ops) {
            if (StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/")) {
                operation.path = operation.path.substring(1);
            }
        }
        return objs;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);
        Set<String> ifaces = new HashSet<>();

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> map = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) map.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel cm = (CodegenModel) model.get("model");
                if (cm.interfaces != null && cm.interfaces.size() > 0) {
                    ifaces.addAll(cm.interfaces);
                    model.put("hasInterfaces", true);
                }
            }
        }

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> map = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) map.get("models");
            for (Map<String, Object> model : models) {
                CodegenModel cm = (CodegenModel) model.get("model");
                if (ifaces.contains(cm.classFilename)) {
                    model.put("isInterface", true);
                }
            }
        }

        return result;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> map = super.postProcessModels(objs);
        postProcessModelsEnum(map);
        return map;
    }
}
