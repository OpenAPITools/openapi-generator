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
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.languages.AbstractJavaCodegen.DATE_LIBRARY;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.*;

public abstract class AbstractKotlinCodegen_ORIG extends DefaultCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractKotlinCodegen_ORIG.class);
    public enum SERIALIZATION_LIBRARY_TYPE {moshi, gson, jackson, kotlinx_serialization}
    public static final String SERIALIZATION_LIBRARY_DESC = "What serialization library to use: 'moshi' (default), or 'gson' or 'jackson'";
    public static final String MODEL_MUTABLE = "modelMutable";
    public static final String MODEL_MUTABLE_DESC = "Create mutable models";
    public static final String ADDITIONAL_MODEL_TYPE_ANNOTATIONS = "additionalModelTypeAnnotations";

    public static final String JAVAX_PACKAGE = "javaxPackage";
    public static final String USE_JAKARTA_EE = "useJakartaEe";
    public static final String CALL_ = "call_";
    public static final String TYPE_BYTE = "kotlin.Byte";
    public static final String TYPE_BYTE_ARRAY = "kotlin.ByteArray";
    public static final String TYPE_SHORT = "kotlin.Short";
    public static final String TYPE_INT = "kotlin.Int";
    public static final String TYPE_LONG = "kotlin.Long";
    public static final String TYPE_FLOAT = "kotlin.Float";
    public static final String TYPE_DOUBLE = "kotlin.Double";
    public static final String TYPE_BOOLEAN = "kotlin.Boolean";
    public static final String TYPE_CHAR = "kotlin.Char";
    public static final String TYPE_STRING = "kotlin.String";
    public static final String TYPE_ARRAY = "kotlin.Array";
    public static final String TYPE_LIST = "kotlin.collections.List";
    public static final String TYPE_MUTABLE_LIST = "kotlin.collections.MutableList";
    public static final String TYPE_MAP = "kotlin.collections.Map";
    public static final String TYPE_MUTABLE_MAP = "kotlin.collections.MutableMap";
    public static final String TYPE_SET = "kotlin.collections.Set";
    public static final String TYPE_MUTABLE_SET = "kotlin.collections.MutableSet";
    public static final String TYPE_ANY = "kotlin.Any";
    public static final String TYPE_BIG_DECIMAL = "java.math.BigDecimal";
    public static final String TYPE_OFFSET_DAY_TIME = "java.time.OffsetDateTime";
    public static final String TYPE_LOCAL_DATE = "java.time.LocalDate";
    public static final String TYPE_FILE = "java.io.File";

    protected String modelPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase.name();
    protected String artifactId;
    protected String artifactVersion = "1.0.0";
    protected String groupId = "org.openapitools";
    protected String packageName = "org.openapitools";
    protected String invokerPackage = "org.openapitools.client";
    protected String apiSuffix = "Api";
    protected String sourceFolder = "src/main/kotlin";
    protected String testFolder = "src/test/kotlin";
    protected String resourcesFolder = "src/main/resources";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean parcelizeModels = false;
    protected boolean serializableModel = false;
    protected boolean useJakartaEe = false;
    protected boolean nonPublicApi = false;
    protected String apiVersion = "1.0";
    protected boolean isStripPackageName = true;
    protected String dateLibrary = AbstractScalaCodegen.DateLibraries.java8.name();

    protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase;
    protected SERIALIZATION_LIBRARY_TYPE serializationLibrary = SERIALIZATION_LIBRARY_TYPE.moshi;

    // model classes cannot use the same property names defined in HashMap
    // ref: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/-hash-map/
    protected Set<String> propertyAdditionalKeywords = new HashSet<>(Arrays.asList("entries", "keys", "size", "values"));

    protected List<String> additionalModelTypeAnnotations = new LinkedList<>();

    protected AbstractKotlinCodegen_ORIG() {
        super();

        supportsInheritance = true;
        setSortModelPropertiesByRequiredFlag(true);

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                TYPE_BYTE,
                TYPE_BYTE_ARRAY,
                TYPE_SHORT,
                TYPE_INT,
                TYPE_LONG,
                TYPE_FLOAT,
                TYPE_DOUBLE,
                TYPE_BOOLEAN,
                TYPE_CHAR,
                TYPE_STRING,
                TYPE_ARRAY,
                TYPE_LIST,
                TYPE_MUTABLE_LIST,
                TYPE_MAP,
                TYPE_MUTABLE_MAP,
                TYPE_SET,
                TYPE_MUTABLE_SET
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
                "contract",
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
                TYPE_BYTE,
                TYPE_BYTE_ARRAY,
                TYPE_SHORT,
                TYPE_INT,
                TYPE_LONG,
                TYPE_FLOAT,
                TYPE_DOUBLE,
                TYPE_BOOLEAN,
                TYPE_CHAR,
                TYPE_STRING,
                TYPE_ARRAY,
                TYPE_LIST,
                TYPE_MUTABLE_LIST,
                TYPE_MAP,
                TYPE_MUTABLE_MAP,
                TYPE_SET,
                TYPE_MUTABLE_SET
        ));

        typeMapping = new HashMap<>();
        typeMapping.put("string", TYPE_STRING);
        typeMapping.put("boolean", TYPE_BOOLEAN);
        typeMapping.put("integer", TYPE_INT);
        typeMapping.put("float", TYPE_FLOAT);
        typeMapping.put("long", TYPE_LONG);
        typeMapping.put("double", TYPE_DOUBLE);
        typeMapping.put("ByteArray", TYPE_BYTE_ARRAY);
        typeMapping.put("number", TYPE_BIG_DECIMAL);
        typeMapping.put("decimal", TYPE_BIG_DECIMAL);
        typeMapping.put("date-time", TYPE_OFFSET_DAY_TIME);
        typeMapping.put("date", TYPE_LOCAL_DATE);
        typeMapping.put("file", TYPE_FILE);
        typeMapping.put("array", TYPE_ARRAY);
        typeMapping.put("list", TYPE_LIST);
        typeMapping.put("set", TYPE_SET);
        typeMapping.put("map", TYPE_MAP);
        typeMapping.put("object", TYPE_ANY);
        typeMapping.put("binary", TYPE_BYTE_ARRAY);
        typeMapping.put("Date", TYPE_LOCAL_DATE);
        typeMapping.put("DateTime", TYPE_OFFSET_DAY_TIME);
        typeMapping.put("AnyType", TYPE_ANY);

        instantiationTypes.put("array", "kotlin.collections.ArrayList");
        instantiationTypes.put("list", "kotlin.collections.ArrayList");
        instantiationTypes.put("map", "kotlin.collections.HashMap");

        importMapping = new HashMap<>();
        importMapping.put("BigDecimal", TYPE_BIG_DECIMAL);
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", TYPE_FILE);
        importMapping.put("Date", TYPE_LOCAL_DATE);
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("DateTime", TYPE_OFFSET_DAY_TIME);
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", TYPE_LOCAL_DATE);
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
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue(modelPropertyNaming));


        cliOptions.add(CliOption.newBoolean(MODEL_MUTABLE, MODEL_MUTABLE_DESC, false));
        cliOptions.add(CliOption.newString(ADDITIONAL_MODEL_TYPE_ANNOTATIONS, "Additional annotations for model type(class level annotations). List separated by semicolon(;) or new line (Linux or Windows)"));
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);

       // return (outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + testFolder + File.separator + apiPackage().replace('.', File.separatorChar);

       // return (outputFolder + File.separator + testFolder + File.separator + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
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
     * @param enumPropertyNamingType The string representation of the naming convention, as defined by {@link CodegenConstants.ENUM_PROPERTY_NAMING_TYPE}
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
     *                                 {@link AbstractKotlinCodegen_ORIG.SERIALIZATION_LIBRARY_TYPE}
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
        Schema<?> schema = unaliasSchema(p);
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
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);
        objs = super.updateAllModels(objs);

        if (!additionalModelTypeAnnotations.isEmpty()) {
            for (String modelName : objs.keySet()) {
                Map<String, Object> models = objs.get(modelName);
                models.put(ADDITIONAL_MODEL_TYPE_ANNOTATIONS, additionalModelTypeAnnotations);
            }
        }

        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModelsEnum(objs);
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.getDiscriminator() != null) {
                cm.vendorExtensions.put("x-has-data-class-body", true);
                break;
            }

            for (CodegenProperty property : cm.vars) {
                if (property.isEnum || isSerializableModel()) {
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
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
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

        if (additionalProperties.containsKey(ADDITIONAL_MODEL_TYPE_ANNOTATIONS)) {
            String additionalAnnotationsList = additionalProperties.get(ADDITIONAL_MODEL_TYPE_ANNOTATIONS).toString();
            this.setAdditionalModelTypeAnnotations(Arrays.asList(additionalAnnotationsList.trim().split("\\s*(;|\\r?\\n)\\s*")));
        }

        additionalProperties.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, getSortParamsByRequiredFlag());
        additionalProperties.put(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG, getSortModelPropertiesByRequiredFlag());

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (isModelMutable()) {
            typeMapping.put("list", TYPE_MUTABLE_LIST);
            typeMapping.put("set", TYPE_MUTABLE_SET);
            typeMapping.put("map", TYPE_MUTABLE_MAP);
        }

        if (additionalProperties.containsKey(USE_JAKARTA_EE)) {
            setUseJakartaEe(Boolean.TRUE.equals(additionalProperties.get(USE_JAKARTA_EE)));
        }
        additionalProperties.put(USE_JAKARTA_EE, useJakartaEe);

        if (useJakartaEe) {
            applyJakartaPackage();
        } else {
            applyJavaxPackage();
        }

        if (additionalProperties.containsKey(CodegenConstants.STRIP_PACKAGE_NAME) &&
                "false".equalsIgnoreCase(additionalProperties.get(CodegenConstants.STRIP_PACKAGE_NAME).toString())) {
            this.isStripPackageName = false;
            additionalProperties.put(CodegenConstants.STRIP_PACKAGE_NAME, false);
            LOGGER.warn("stripPackageName=false. Compilation errors may occur if API type names clash with types " +
                    "in the default imports");
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming(
                    (String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(DATE_LIBRARY)) {
            this.setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString(), false);
        }

    }

    protected String formatIdentifier(String name, boolean capitalized) {
        if (specialCharReplacements.containsKey(name)) {
            name = specialCharReplacements.get(name);
        }
        String identifier = camelize(sanitizeName(name), LOWERCASE_FIRST_LETTER);
        if (capitalized) {
            identifier = StringUtils.capitalize(identifier);
        }
        if (identifier.matches("[a-zA-Z_$][\\w_$]+") && !isReservedWord(identifier)) {
            return identifier;
        }
        return escapeReservedWord(identifier);
    }

    protected String stripPackageName(String input) {
        if (!isStripPackageName || StringUtils.isEmpty(input) || input.lastIndexOf(".") < 0)
            return input;

        int lastIndexOfDot = input.lastIndexOf(".");
        return input.substring(lastIndexOfDot + 1);
    }

    protected boolean isModelMutable() {
        return Boolean.TRUE.equals(additionalProperties.get(MODEL_MUTABLE));
    }

    public void setModelPropertyNaming(String naming) {
        try {
            this.modelPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.valueOf(naming).name();
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("Invalid model property naming '" +
                    naming + "'. Must be 'original', 'camelCase', " +
                    "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setDateLibrary(String dateLibrary, boolean withLegacy) {
        if (withLegacy && dateLibrary.equals(AbstractScalaCodegen.DateLibraries.legacy.name())) {
            this.dateLibrary = dateLibrary;
            return;
        }
        for (AbstractScalaCodegen.DateLibraries dateLib : AbstractScalaCodegen.DateLibraries.values()) {
            if (dateLib.name().equals(dateLibrary)) {
                this.dateLibrary = dateLibrary;
                return;
            }
        }
        throw new IllegalArgumentException("Invalid dateLibrary. Must be 'java8' or 'joda'");
    }

    public String getDateLibrary() {
        return this.dateLibrary;
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

    public void setParcelizeModels(Boolean parcelizeModels) {
        this.parcelizeModels = parcelizeModels;
    }

    public boolean isSerializableModel() {
        return serializableModel;
    }

    public void setSerializableModel(boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    public void setUseJakartaEe(boolean useJakartaEe) {
        this.useJakartaEe = useJakartaEe;
    }

    public boolean nonPublicApi() {
        return nonPublicApi;
    }

    public void setNonPublicApi(boolean nonPublicApi) {
        this.nonPublicApi = nonPublicApi;
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
                modified = camelize(modified, LOWERCASE_FIRST_LETTER);
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
        final String sanitizedName = sanitizeName(modelNamePrefix + this.stripPackageName(name) + modelNameSuffix);

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", camelizedName, modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return camelizedName;
//        // memoization
//        if (schemaKeyToModelNameCache.containsKey(name)) {
//            return schemaKeyToModelNameCache.get(name);
//        }
//
//        // Allow for explicitly configured kotlin.* and java.* types
//        if (name.startsWith("kotlin.") || name.startsWith("java.")) {
//            return name;
//        }
//
//        // If schemaMapping contains name, assume this is a legitimate model name.
//        if (schemaMapping.containsKey(name)) {
//            return schemaMapping.get(name);
//        }
//
//        // TODO review importMapping below as we've added schema mapping support
//        // If importMapping contains name, assume this is a legitimate model name.
//        if (importMapping.containsKey(name)) {
//            return importMapping.get(name);
//        }
//
//        String modifiedName = name.replaceAll("\\.", "");
//
//        String nameWithPrefixSuffix = sanitizeKotlinSpecificNames(modifiedName);
//        if (!StringUtils.isEmpty(modelNamePrefix)) {
//            // add '_' so that model name can be camelized correctly
//            nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
//        }
//
//        if (!StringUtils.isEmpty(modelNameSuffix)) {
//            // add '_' so that model name can be camelized correctly
//            nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
//        }
//
//        // Camelize name of nested properties
//        modifiedName = camelize(nameWithPrefixSuffix);
//
//        // model name cannot use reserved keyword, e.g. return
//        if (isReservedWord(modifiedName)) {
//            final String modelName = "Model" + modifiedName;
//            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", modifiedName, modelName);
//            return modelName;
//        }
//
//        // model name starts with number
//        if (modifiedName.matches("^\\d.*")) {
//            final String modelName = "Model" + modifiedName; // e.g. 200Response => Model200Response (after camelize)
//            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
//                    modelName);
//            return modelName;
//        }
//
//        schemaKeyToModelNameCache.put(name, titleCase(modifiedName));
//        return schemaKeyToModelNameCache.get(name);
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

        operationId = camelize(sanitizeName(operationId), LOWERCASE_FIRST_LETTER);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(CALL_ + operationId, LOWERCASE_FIRST_LETTER);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            String formattedString = String.format("%s (starting with a number) cannot be used as method name. Renamed to %s%s",
                    operationId,
                    camelize(CALL_ + operationId),
                    LOWERCASE_FIRST_LETTER);
            LOGGER.warn(formattedString);

            operationId = camelize(CALL_ + operationId, LOWERCASE_FIRST_LETTER);
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
        word = Pattern.compile("\\W+", Pattern.UNICODE_CHARACTER_CLASS).matcher(word).replaceAll("_");
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

    protected void applyJavaxPackage() {
        writePropertyBack(JAVAX_PACKAGE, "javax");
    }

    protected void applyJakartaPackage() {
        writePropertyBack(JAVAX_PACKAGE, "jakarta");
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
        return !type.startsWith("kotlin.") && !type.startsWith("java.") &&
                !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type) &&
                !type.contains(".");
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
        if (TYPE_INT.equals(datatype) || TYPE_LONG.equals(datatype)) {
            return value;
        } else if (TYPE_DOUBLE.equals(datatype)) {
            if (value.contains(".")) {
                return value;
            } else {
                return value + ".0"; // Float and double must have .0
            }
        } else if (TYPE_FLOAT.equals(datatype)) {
            return value + "f";
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public boolean isDataTypeString(final String dataType) {
        return "String".equals(dataType) || TYPE_STRING.equals(dataType);
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
        String varName = sanitizeName(name);

        if ("_".equals(varName)) {
            varName = "_u";
        }

        // if it's all upper case, do nothing
        if (!varName.matches("^[A-Z_0-9]*$")) {
            varName = getNameUsingModelPropertyNaming(varName);
        }

        if (isReservedWord(varName) || varName.matches("^\\d.*")) {
            varName = escapeReservedWord(varName);
        }

        return varName;
//        name = toVariableName(name);
//        if (propertyAdditionalKeywords.contains(name)) {
//            return camelize("property_" + name, LOWERCASE_FIRST_LETTER);
//        } else {
//            return name;
//        }
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, LOWERCASE_FIRST_LETTER);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
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
        if ((name.chars().anyMatch(character -> specialCharReplacements.containsKey(String.valueOf((char) character))))) {
            List<String> allowedCharacters = new ArrayList<>();
            allowedCharacters.add("_");
            allowedCharacters.add("$");
            name = escape(name, specialCharReplacements, allowedCharacters, "_");
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

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

            // replace dot (.) in api files
            if (fileType.equals("api")) {
                Charset charset = StandardCharsets.UTF_8;
                Path path = file.getAbsoluteFile().toPath();

                try {
                    String content = new String(Files.readAllBytes(path), charset);
                    content = content.replaceAll("TO_REPLACE_FOLLOWING_DOT\n {8}.", "    ");
                    Files.write(path, content.getBytes(charset));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }

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

    private String fixNumberValue(String number, Schema p) {
        if (ModelUtils.isFloatSchema(p)) {
            return number + "f";
        } else if (ModelUtils.isDoubleSchema(p)) {
            if (number.contains(".")) {
                return number;
            }
            return number + ".0";
        } else if (ModelUtils.isLongSchema(p)) {
            return number + "L";
        }
        return number;
    }

    @Override
    public String toDefaultValue(Schema schema) {
        Schema<?> p = ModelUtils.getReferencedSchema(this.openAPI, schema);
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
            return null;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
            return null;
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return fixNumberValue(p.getDefault().toString(), p);
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return fixNumberValue(p.getDefault().toString(), p);
            }
        } else if (ModelUtils.isURISchema(p)) {
            if (p.getDefault() != null) {
                return importMapping.get("URI") + ".create(\"" + p.getDefault() + "\")";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                String arrInstantiationType = ModelUtils.isSet(p) ? "set" : "arrayList";

                if (!(p.getDefault() instanceof ArrayNode)) {
                    return null;
                }
                ArrayNode defaultArrayNode = (ArrayNode) p.getDefault();
                if (defaultArrayNode.isEmpty()) {
                    return arrInstantiationType + "Of()";
                }

                StringBuilder defaultContent = new StringBuilder();
                Schema<?> itemsSchema = getSchemaItems((ArraySchema) schema);
                defaultArrayNode.elements().forEachRemaining((element) -> {
                    itemsSchema.setDefault(element.asText());
                    defaultContent.append(toDefaultValue(itemsSchema)).append(",");
                });
                defaultContent.deleteCharAt(defaultContent.length() - 1); // remove trailing comma
                return arrInstantiationType + "Of(" + defaultContent + ")";
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String defaultString = String.valueOf(p.getDefault());
                if (p.getEnum() == null) {
                    return "\"" + escapeText(defaultString) + "\"";
                } else {
                    // convert to enum var name later in postProcessModels
                    return defaultString;
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

    @SuppressWarnings({"rawtypes", "unchecked"})
    @Override
    protected void updateModelForObject(CodegenModel m, Schema schema) {
        /*
         * we have a custom version of this function so we only set isMap to true if
         * ModelUtils.isMapSchema
         * In other generators, isMap is true for all type object schemas
         */
        if (schema.getProperties() != null || schema.getRequired() != null && !(schema instanceof ComposedSchema)) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        if (ModelUtils.isMapSchema(schema)) {
            // an object or anyType composed schema that has additionalProperties set
            addAdditionPropertiesToCodeGenModel(m, schema);
        } else {
            m.setIsMap(false);
            if (ModelUtils.isFreeFormObject(openAPI, schema)) {
                // non-composed object type with no properties + additionalProperties
                // additionalProperties must be null, ObjectSchema, or empty Schema
                addAdditionPropertiesToCodeGenModel(m, schema);
            }
        }
        // process 'additionalProperties'
        setAddProps(schema, m);
    }

    public List<String> getAdditionalModelTypeAnnotations() {
        return additionalModelTypeAnnotations;
    }

    public void setAdditionalModelTypeAnnotations(final List<String> additionalModelTypeAnnotations) {
        this.additionalModelTypeAnnotations = additionalModelTypeAnnotations;
    }
}
