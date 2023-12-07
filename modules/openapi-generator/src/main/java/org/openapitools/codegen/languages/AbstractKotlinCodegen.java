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
import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
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

import static org.openapitools.codegen.languages.AbstractJavaCodegen.DATE_LIBRARY;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.*;

public abstract class AbstractKotlinCodegen extends DefaultCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractKotlinCodegen.class);
    public enum SERIALIZATION_LIBRARY_TYPE {moshi, gson, jackson, kotlinx_serialization}
    public static final String SERIALIZATION_LIBRARY_DESC = "What serialization library to use: 'moshi' (default), or 'gson' or 'jackson'";
    public static final String MODEL_MUTABLE = "modelMutable";
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
    protected String invokerPackage = "org.openapitools.client";
    protected String sourceFolder = "src/main/kotlin";
    protected String testFolder = "src/test/kotlin";
    protected String apiSuffix = "Api";
    protected String resourcesFolder = "src/main/resources";
    protected String appName = "OpenAPI Sample";
    protected String appDescription = "A sample openapi server";
    protected String infoUrl = "http://org.openapitools";
    protected String infoEmail = "team@openapitools.org";
    protected String licenseInfo = "All rights reserved";
    protected String licenseUrl = "http://apache.org/licenses/LICENSE-2.0.html";
    protected String apiVersion = "1.0";
    protected String artifactId;
    protected String artifactVersion = "1.0.0";
    protected String packageName = "org.openapitools";
    protected String groupId = "org.openapitools";
    protected boolean isStripPackageName = true;
    protected boolean useJakartaEe = false;
    protected String dateLibrary = AbstractKotlinCodegen.DateLibraries.java8.name();
    protected AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE serializationLibrary = AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE.moshi;
    protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase;

    protected enum DateLibraries {
        java8("Java 8 native JSR310 (preferred for JDK 1.8+)"),
        joda("Joda (for legacy app)"),
        legacy("Backport to http-client (deprecated)");

        private final String description;

        DateLibraries(String description) {
            this.description = description;
        }
    }

    protected AbstractKotlinCodegen() {
        super();

        languageSpecificPrimitives.addAll(Arrays.asList(
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
        reservedWords.addAll(Arrays.asList(
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

        defaultIncludes.addAll(Arrays.asList(
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
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Array", "java.util.List");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("DateTime", TYPE_OFFSET_DAY_TIME);
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", TYPE_LOCAL_DATE);
        importMapping.put("LocalTime", "java.time.LocalTime");

        specialCharReplacements.put(";", "Semicolon");
        specialCharReplacements.put("&#x3D;", "Equal");
        specialCharReplacements.put("!&#x3D;", "Not_Equal");
        specialCharReplacements.put("&gt;", "Greater_Than");
        specialCharReplacements.put("&lt;", "Less_Than");
        specialCharReplacements.put("&gt;&#x3D;", "Greater_Than_Or_Equal_To");
        specialCharReplacements.put("&lt;&#x3D;", "Less_Than_Or_Equal_To");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue(modelPropertyNaming));

        cliOptions.clear();
        addOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC, sourceFolder);
        addOption(CodegenConstants.PACKAGE_NAME, "Generated artifact package name.", packageName);
        addOption(CodegenConstants.API_SUFFIX, CodegenConstants.API_SUFFIX_DESC, apiSuffix);
        addOption(CodegenConstants.GROUP_ID, "Generated artifact package's organization (i.e. maven groupId).", groupId);
        addOption(CodegenConstants.ARTIFACT_ID, "Generated artifact id (name of jar).", artifactId);
        addOption(CodegenConstants.ARTIFACT_VERSION, "Generated artifact's package version.", artifactVersion);


        CliOption dateLibraryCliOption = new CliOption(DATE_LIBRARY, "Option. Date library to use").defaultValue(this.dateLibrary);
        Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(AbstractKotlinCodegen.DateLibraries.java8.name(), AbstractKotlinCodegen.DateLibraries.java8.description);
        dateOptions.put(AbstractKotlinCodegen.DateLibraries.joda.name(), AbstractKotlinCodegen.DateLibraries.joda.description);
        dateLibraryCliOption.setEnum(dateOptions);
        cliOptions.add(dateLibraryCliOption);

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("KOTLIN_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable KOTLIN_POST_PROCESS_FILE not defined so the Kotlin code may not be properly formatted. To define it, try 'export KOTLIN_POST_PROCESS_FILE=\"/usr/local/bin/ktlint -F\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        this.appName = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getTitle()).filter(t -> t != null).orElse(this.appName);
        this.appDescription = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getDescription()).filter(d -> d != null).orElse(this.appDescription);
        this.infoUrl = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getContact()).filter(c -> c != null).map(c -> c.getUrl()).filter(u -> u != null).orElse(this.infoUrl);
        this.infoEmail = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getContact()).filter(c -> c != null).map(c -> c.getEmail()).filter(v -> v != null).orElse(this.infoEmail);
        this.licenseInfo = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getLicense()).filter(l -> l != null).map(l -> l.getName()).filter(n -> n != null).orElse(this.licenseInfo);
        this.licenseUrl = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getLicense()).filter(l -> l != null).map(l -> l.getUrl()).filter(u -> u != null).orElse(this.licenseUrl);

        this.apiVersion = Optional.ofNullable(openAPI).map(o -> o.getInfo()).filter(i -> i != null).map(i -> i.getVersion()).filter(v -> v != null).orElse(this.apiVersion);

        if (additionalProperties.containsKey(USE_JAKARTA_EE)) {
            setUseJakartaEe(Boolean.TRUE.equals(additionalProperties.get(USE_JAKARTA_EE)));
        }
        additionalProperties.put(USE_JAKARTA_EE, useJakartaEe);

        if (useJakartaEe) {
            applyJakartaPackage();
        } else {
            applyJavaxPackage();
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            setSerializationLibrary((String) additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY));
            additionalProperties.put(this.serializationLibrary.name(), true);
        } else {
            additionalProperties.put(this.serializationLibrary.name(), true);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.setArtifactId((String) additionalProperties.get(CodegenConstants.ARTIFACT_ID));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
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
        if (AbstractKotlinCodegen.DateLibraries.java8.name().equals(dateLibrary)) {
            this.importMapping.put("LocalDate", TYPE_LOCAL_DATE);
            this.importMapping.put("OffsetDateTime", TYPE_OFFSET_DAY_TIME);
            this.typeMapping.put("date", "LocalDate");
            this.typeMapping.put("DateTime", "OffsetDateTime");
            additionalProperties.put("java8", "true");
        } else if (AbstractKotlinCodegen.DateLibraries.joda.name().equals(dateLibrary)) {
            this.importMapping.put("LocalDate", "org.joda.time.LocalDate");
            this.importMapping.put("DateTime", "org.joda.time.DateTime");
            this.importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
            this.importMapping.put("LocalTime", "org.joda.time.LocalTime");
            this.typeMapping.put("date", "LocalDate");
            this.typeMapping.put("DateTime", "DateTime");
            additionalProperties.put("joda", "true");
        }

        if (isModelMutable()) {
            typeMapping.put("list", TYPE_MUTABLE_LIST);
            typeMapping.put("set", TYPE_MUTABLE_SET);
            typeMapping.put("map", TYPE_MUTABLE_MAP);
        }
    }

    protected boolean isModelMutable() {
        return Boolean.TRUE.equals(additionalProperties.get(MODEL_MUTABLE));
    }

    public void setUseJakartaEe(boolean useJakartaEe) {
        this.useJakartaEe = useJakartaEe;
    }
    public void setSerializationLibrary(final String enumSerializationLibrary) {
        try {
            this.serializationLibrary = AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE.valueOf(enumSerializationLibrary);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumSerializationLibrary + " is an invalid enum property naming option. Please choose from:");
            for (AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE t : AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
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
    protected void applyJavaxPackage() {
        writePropertyBack(JAVAX_PACKAGE, "javax");
    }

    protected void applyJakartaPackage() {
        writePropertyBack(JAVAX_PACKAGE, "jakarta");
    }
    public void setDateLibrary(String dateLibrary, boolean withLegacy) {
        if (withLegacy && dateLibrary.equals(AbstractKotlinCodegen.DateLibraries.legacy.name())) {
            this.dateLibrary = dateLibrary;
            return;
        }
        for (AbstractKotlinCodegen.DateLibraries dateLib : AbstractKotlinCodegen.DateLibraries.values()) {
            if (dateLib.name().equals(dateLibrary)) {
                this.dateLibrary = dateLibrary;
                return;
            }
        }
        throw new IllegalArgumentException("Invalid dateLibrary. Must be 'java8' or 'joda'");
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

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }


    public String getDateLibrary() {
        return this.dateLibrary;
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

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public AbstractKotlinCodegen.SERIALIZATION_LIBRARY_TYPE getSerializationLibrary() {
        return this.serializationLibrary;
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
    public String getSourceFolder() {
        return sourceFolder;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        // TODO: Allow enum escaping as an option (e.g. backticks vs append/prepend underscore vs match model property escaping).
        return String.format(Locale.ROOT, "`%s`", name);
    }

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        Mustache.Escaper KOTLIN = text -> {
            // Fix included as suggested by akkie in #6393
            // The given text is a reserved word which is escaped by enclosing it with grave accents. If we would
            // escape that with the default Mustache `HTML` escaper, then the escaper would also escape our grave
            // accents. So we remove the grave accents before the escaping and add it back after the escaping.
            if (text.startsWith("`") && text.endsWith("`")) {
                String unescaped = text.substring(1, text.length() - 1);
                return "`" + Escapers.HTML.escape(unescaped) + "`";
            }

            // All none reserved words will be escaped with the default Mustache `HTML` escaper
            return Escapers.HTML.escape(text);
        };

        return compiler.withEscaper(KOTLIN);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = unaliasSchema(p);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        if (ModelUtils.isArraySchema(target)) {
            Schema<?> items = getSchemaItems((ArraySchema) schema);
            return getSchemaType(target) + "[" + getTypeDeclaration(items) + "]";
        } else if (ModelUtils.isMapSchema(target)) {
            Schema<?> inner = getAdditionalProperties(target);
            if (inner == null) {
                LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
                p.setAdditionalProperties(inner);
            }
            return getSchemaType(target) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(target);
    }

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
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return instantiationTypes.get("map") + "[String, " + inner + "]";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return (ModelUtils.isSet(ap) ? instantiationTypes.get("set") : instantiationTypes.get("array")) + "[" + inner + "]";
        } else {
            return null;
        }
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

    /**
     * Convert OAS Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema p, boolean required) {
        CodegenProperty prop = super.fromProperty(name, p, required);
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema as = (ArraySchema) p;
            if (ModelUtils.isSet(as)) {
                prop.containerType = "set";
            }
        }
        return prop;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // remove model imports to avoid warnings for importing class in the same package
        List<Map<String, String>> imports = objs.getImports();
        final String prefix = modelPackage() + ".";
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix)) iterator.remove();
        }
        return objs;
    }

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
    }

    @Override
    public String toModelFilename(String name) {
        // Should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
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

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

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

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.KOTLIN;
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
}
