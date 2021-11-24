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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants.ENUM_PROPERTY_NAMING_TYPE;
import org.openapitools.codegen.CodegenConstants.MODEL_PROPERTY_NAMING_TYPE;
import org.openapitools.codegen.CodegenConstants.PARAM_NAMING_TYPE;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public abstract class AbstractTypeScriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractTypeScriptClientCodegen.class);

    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";
    private static final String UNDEFINED_VALUE = "undefined";
    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String SNAPSHOT = "snapshot";

    public static final String MODEL_PROPERTY_NAMING_DESC_WITH_WARNING = CodegenConstants.MODEL_PROPERTY_NAMING_DESC
            + ". Only change it if you provide your own run-time code for (de-)serialization of models";

    public static final String NULL_SAFE_ADDITIONAL_PROPS = "nullSafeAdditionalProps";
    public static final String NULL_SAFE_ADDITIONAL_PROPS_DESC = "Set to make additional properties types declare that their indexer may return undefined";

    // NOTE: SimpleDateFormat is not thread-safe and may not be static unless it is thread-local
    @SuppressWarnings("squid:S5164")
    protected static final ThreadLocal<SimpleDateFormat> SNAPSHOT_SUFFIX_FORMAT = ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyyMMddHHmm", Locale.ROOT));

    protected MODEL_PROPERTY_NAMING_TYPE modelPropertyNaming = MODEL_PROPERTY_NAMING_TYPE.original;
    protected ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = ENUM_PROPERTY_NAMING_TYPE.PascalCase;
    protected PARAM_NAMING_TYPE paramNaming = PARAM_NAMING_TYPE.camelCase;
    protected Boolean supportsES6 = false;
    protected Boolean nullSafeAdditionalProps = false;
    protected HashSet<String> languageGenericTypes;
    protected String npmName = null;
    protected String npmVersion = "1.0.0";

    protected String enumSuffix = "Enum";

    protected String classEnumSeparator = ".";

    public AbstractTypeScriptClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;

        // to support multiple inheritance e.g. export interface ModelC extends ModelA, ModelB
        //supportsMultipleInheritance = true;

        // NOTE: TypeScript uses camel cased reserved words, while models are title cased. We don't want lowercase comparisons.
        reservedWords.addAll(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "varLocalPath", "queryParameters", "headerParams", "formParams", "useFormData", "varLocalDeferred",
                "requestOptions",
                // Typescript reserved words
                "abstract", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "debugger", "default", "delete", "do", "double", "else", "enum", "export", "extends", "false", "final", "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface", "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"));

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                "string",
                "String",
                "boolean",
                "Boolean",
                "Double",
                "Integer",
                "Long",
                "Float",
                "Object",
                "Array",
                "ReadonlyArray",
                "Date",
                "number",
                "any",
                "File",
                "Error",
                "Map",
                "object",
                "Set"
        ));

        languageGenericTypes = new HashSet<>(Collections.singletonList(
                "Array"
        ));

        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Set", "Set");
        typeMapping.put("set", "Set");
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "object");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "any");
        typeMapping.put("map", "any");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("binary", "any");
        typeMapping.put("File", "any");
        typeMapping.put("file", "any");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("Error", "Error");
        typeMapping.put("AnyType", "any");

        cliOptions.add(new CliOption(CodegenConstants.ENUM_NAME_SUFFIX, CodegenConstants.ENUM_NAME_SUFFIX_DESC).defaultValue(this.enumSuffix));
        cliOptions.add(new CliOption(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_DESC).defaultValue(this.enumPropertyNaming.name()));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, MODEL_PROPERTY_NAMING_DESC_WITH_WARNING).defaultValue(this.modelPropertyNaming.name()));
        cliOptions.add(new CliOption(CodegenConstants.SUPPORTS_ES6, CodegenConstants.SUPPORTS_ES6_DESC).defaultValue(String.valueOf(this.getSupportsES6())));
        cliOptions.add(new CliOption(CodegenConstants.PARAM_NAMING, CodegenConstants.PARAM_NAMING_DESC).defaultValue(this.paramNaming.name()));
        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package." +
                " Required to generate a full package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package. If not provided, using the version from the OpenAPI specification file.").defaultValue(this.getNpmVersion()));
        this.cliOptions.add(CliOption.newBoolean(SNAPSHOT,
                "When setting this property to true, the version will be suffixed with -SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.get().toPattern(),
                false));
        this.cliOptions.add(new CliOption(NULL_SAFE_ADDITIONAL_PROPS, NULL_SAFE_ADDITIONAL_PROPS_DESC).defaultValue(String.valueOf(this.getNullSafeAdditionalProps())));
    }

    protected void supportModelPropertyNaming(MODEL_PROPERTY_NAMING_TYPE defaultModelPropertyNaming) {
        removeOption(CodegenConstants.MODEL_PROPERTY_NAMING);
        modelPropertyNaming = defaultModelPropertyNaming;
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue(modelPropertyNaming.name()));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("TS_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable 'TS_POST_PROCESS_FILE' (optional) not defined. E.g. to format the source code, please try 'export TS_POST_PROCESS_FILE=\"/usr/local/bin/prettier --write\"' (Linux/Mac)");
            LOGGER.info("Note: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'TS_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_NAME_SUFFIX)) {
            enumSuffix = additionalProperties.get(CodegenConstants.ENUM_NAME_SUFFIX).toString();
        }
        if (additionalProperties.containsKey(CodegenConstants.ENUM_PROPERTY_NAMING)) {
            setEnumPropertyNaming((String) additionalProperties.get(CodegenConstants.ENUM_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.PARAM_NAMING)) {
            setParamNaming((String) additionalProperties.get(CodegenConstants.PARAM_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.SUPPORTS_ES6)) {
            setSupportsES6(Boolean.valueOf(additionalProperties.get(CodegenConstants.SUPPORTS_ES6).toString()));
            additionalProperties.put("supportsES6", getSupportsES6());
        }

        if (additionalProperties.containsKey(NULL_SAFE_ADDITIONAL_PROPS)) {
            setNullSafeAdditionalProps(Boolean.valueOf(additionalProperties.get(NULL_SAFE_ADDITIONAL_PROPS).toString()));
        }

        if (additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }
    }

    @Override
    public String toModelImport(String name){
        if(isUnionType(name)){
           LOGGER.warn("The import is a union type. Consider using the toModelImportMap method.");
           return toModelImportMap(name).values().stream().collect(Collectors.joining("|"));
        }
        if(isIntersectionType(name)){
           LOGGER.warn("The import is a intersection type. Consider using the toModelImportMap method.");
           return toModelImportMap(name).values().stream().collect(Collectors.joining("&"));
        }
        return super.toModelImport(name);
    }

    /**
     * Maps the fully qualified model import to the initial given name. In case of union types the map will have multiple entries.
     * For example for "classA | classB" the map will two entries have ["model.classA","classA"] and ["model.classB","classB"].
     *
     * @param name the name of the "Model"
     * @return Map between the fully qualified model import and the initial given name.
     */
    @Override
    public Map<String,String> toModelImportMap(String name){
        return toImportMap(splitComposedType(name));
    }

    private String[] splitComposedType (String name) {
       return name.replace(" ","").split("[|&<>]");
    }

    private boolean isUnionType(String name){
        return name.contains("|");
    }

    private boolean isIntersectionType(String name){
        return name.contains("&");
    }

    private Map<String,String> toImportMap(String... names) {
        Map<String,String> result = new HashMap<>();
        for(final String name : names) {
            if(needToImport(name)) {
                result.put(toModelImport(name), name);
            }
        }
        return result;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {

        if (additionalProperties.containsKey(NPM_NAME)) {

            // If no npmVersion is provided in additional properties, version from API specification is used.
            // If none of them is provided then fallbacks to default version
            if (additionalProperties.containsKey(NPM_VERSION)) {
                this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
            } else if (openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
                this.setNpmVersion(openAPI.getInfo().getVersion());
            }

            if (additionalProperties.containsKey(SNAPSHOT) && Boolean.parseBoolean(additionalProperties.get(SNAPSHOT).toString())) {
                if (npmVersion.toUpperCase(Locale.ROOT).matches("^.*-SNAPSHOT$")) {
                    this.setNpmVersion(npmVersion + "." + SNAPSHOT_SUFFIX_FORMAT.get().format(new Date()));
                } else {
                    this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.get().format(new Date()));
                }
            }
            additionalProperties.put(NPM_VERSION, npmVersion);

        }

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toParamName(String name) {
        name = sanitizeName(name, "[^\\w$]");

        if ("_".equals(name)) {
            name = "_u";
        }

        name = getNameUsingParamNaming(name);
        name = toSafeIdentifier(name);

        return name;
    }

    @Override
    public String toVarName(String name) {
        name = sanitizeName(name, "[^\\w$]");

        if ("_".equals(name)) {
            name = "_u";
        }

        name = getNameUsingModelPropertyNaming(name);
        name = toSafeIdentifier(name);

        return name;
    }

    private String toSafeIdentifier(String name) {
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }
        return name;
    }

    @Override
    public String toModelName(final String name) {
        String fullModelName = name;
        fullModelName = addPrefix(fullModelName, modelNamePrefix);
        fullModelName = addSuffix(fullModelName, modelNameSuffix);
        return toTypescriptTypeName(fullModelName, "Model");
    }

    protected String addPrefix(String name, String prefix) {
        if (!StringUtils.isEmpty(prefix)) {
            name = prefix + "_" + name;
        }
        return name;
    }

    protected String addSuffix(String name, String suffix) {
        if (!StringUtils.isEmpty(suffix)) {
            name = name + "_" + suffix;
        }

        return name;
    }

    protected String toTypescriptTypeName(final String name, String safePrefix) {
        ArrayList<String> exceptions = new ArrayList<String>(Arrays.asList("\\|", " "));
        String sanName = sanitizeName(name, "(?![| ])\\W", exceptions);

        sanName = camelize(sanName);

        // model name cannot use reserved keyword, e.g. return
        // this is unlikely to happen, because we have just camelized the name, while reserved words are usually all lowercase
        if (isReservedWord(sanName)) {
            String modelName = safePrefix + sanName;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", sanName, modelName);
            return modelName;
        }

        // model name starts with number
        if (sanName.matches("^\\d.*")) {
            String modelName = safePrefix + sanName; // e.g. 200Response => Model200Response
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", sanName,
                    modelName);
            return modelName;
        }

        if (languageSpecificPrimitives.contains(sanName)) {
            String modelName = safePrefix + sanName;
            LOGGER.warn("{} (model name matches existing language type) cannot be used as a model name. Renamed to {}",
                    sanName, modelName);
            return modelName;
        }

        return sanName;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema<?> items = getSchemaItems((ArraySchema) p);
            return getSchemaType(p) + "<" + getTypeDeclaration(ModelUtils.unaliasSchema(this.openAPI, items)) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema<?> inner = getSchemaAdditionalProperties(p);
            String nullSafeSuffix = getNullSafeAdditionalProps() ? " | undefined" : "";
            return "{ [key: string]: " + getTypeDeclaration(ModelUtils.unaliasSchema(this.openAPI, inner)) + nullSafeSuffix  + "; }";
        } else if (ModelUtils.isFileSchema(p)) {
            return "any";
        } else if (ModelUtils.isBinarySchema(p)) {
            return "any";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    protected String getParameterDataType(Parameter parameter, Schema p) {
        // handle enums of various data types
        Schema inner;
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema mp1 = (ArraySchema) p;
            inner = mp1.getItems();
            return this.getSchemaType(p) + "<" + this.getParameterDataType(parameter, inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            inner = getAdditionalProperties(p);
            return "{ [key: string]: " + this.getParameterDataType(parameter, inner) + "; }";
        } else if (ModelUtils.isStringSchema(p)) {
            // Handle string enums
            if (p.getEnum() != null) {
                return enumValuesToEnumTypeUnion(p.getEnum(), "string");
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            // Handle integer enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            // Handle double enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        }
        /* TODO revise the logic below
        else if (ModelUtils.isDateSchema(p)) {
            // Handle date enums
            DateSchema sp = (DateSchema) p;
            if (sp.getEnum() != null) {
                return enumValuesToEnumTypeUnion(sp.getEnum(), "string");
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // Handle datetime enums
            DateTimeSchema sp = (DateTimeSchema) p;
            if (sp.getEnum() != null) {
                return enumValuesToEnumTypeUnion(sp.getEnum(), "string");
            }
        }*/
        return this.getTypeDeclaration(p);
    }

    /**
     * Converts a list of strings to a literal union for representing enum values as a type.
     * Example output: 'available' | 'pending' | 'sold'
     *
     * @param values   list of allowed enum values
     * @param dataType either "string" or "number"
     * @return a literal union for representing enum values as a type
     */
    private String enumValuesToEnumTypeUnion(List<String> values, String dataType) {
        StringBuilder b = new StringBuilder();
        boolean isFirst = true;
        for (String value : values) {
            if (!isFirst) {
                b.append(" | ");
            }
            b.append(toEnumValue(value, dataType));
            isFirst = false;
        }
        return b.toString();
    }

    /**
     * Converts a list of numbers to a literal union for representing enum values as a type.
     * Example output: 3 | 9 | 55
     *
     * @param values a list of numbers
     * @return a literal union for representing enum values as a type
     */
    private String numericEnumValuesToEnumTypeUnion(List<Number> values) {
        List<String> stringValues = new ArrayList<>();
        for (Number value : values) {
            stringValues.add(value.toString());
        }
        return enumValuesToEnumTypeUnion(stringValues, "number");
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isNumberSchema(p) || ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "'" + escapeText((String) p.getDefault()) + "'";
            }
            return UNDEFINED_VALUE;
        } else {
            return UNDEFINED_VALUE;
        }

    }

    @Override
    protected boolean isReservedWord(String word) {
        // NOTE: This differs from super's implementation in that TypeScript does _not_ want case insensitive matching.
        return reservedWords.contains(word);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (ModelUtils.isComposedSchema(p)) {
            return openAPIType;
        } else if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        operationId = camelize(sanitizeName(operationId), true);
        operationId = toSafeIdentifier(operationId);

        return operationId;
    }

    public void setModelPropertyNaming(String naming) {
        try {
            modelPropertyNaming = MODEL_PROPERTY_NAMING_TYPE.valueOf(naming);
        } catch (IllegalArgumentException e) {
            String values = Stream.of(MODEL_PROPERTY_NAMING_TYPE.values())
                    .map(value -> "'" + value.name() + "'")
                    .collect(Collectors.joining(", "));

            String msg = String.format(Locale.ROOT, "Invalid model property naming '%s'. Must be one of %s.", naming, values);
            throw new IllegalArgumentException(msg);
        }
    }

    public void setParamNaming(String naming) {
        try {
            paramNaming = PARAM_NAMING_TYPE.valueOf(naming);
        } catch (IllegalArgumentException e) {
            String values = Stream.of(PARAM_NAMING_TYPE.values())
                    .map(value -> "'" + value.name() + "'")
                    .collect(Collectors.joining(", "));

            String msg = String.format(Locale.ROOT, "Invalid parameter naming '%s'. Must be one of %s.", naming, values);
            throw new IllegalArgumentException(msg);
        }
    }

    public MODEL_PROPERTY_NAMING_TYPE getModelPropertyNaming() {
        return modelPropertyNaming;
    }

    public PARAM_NAMING_TYPE getParamNaming() {
        return paramNaming;
    }

    private String getNameUsingParamNaming(String name) {
        switch (getParamNaming()) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid param naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }

    }

    private String getNameUsingModelPropertyNaming(String name) {
        switch (getModelPropertyNaming()) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
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

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("number".equals(datatype) || "boolean".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return getNameUsingEnumPropertyNaming("empty");
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getNameUsingEnumPropertyNaming(getSymbolName(name));
        }

        // number
        if ("number".equals(datatype)) {
            String varName = "NUMBER_" + name;

            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(name);
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        enumName = getNameUsingEnumPropertyNaming(enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = property.name;
        enumName = addSuffix(enumName, enumSuffix);
        return toTypescriptTypeName(enumName, "_");
    }

    protected void setEnumPropertyNaming(String naming) {
        try {
            enumPropertyNaming = ENUM_PROPERTY_NAMING_TYPE.valueOf(naming);
        } catch (IllegalArgumentException e) {
            String values = Stream.of(ENUM_PROPERTY_NAMING_TYPE.values())
                    .map(value -> "'" + value.name() + "'")
                    .collect(Collectors.joining(", "));

            String msg = String.format(Locale.ROOT, "Invalid enum property naming '%s'. Must be one of %s.",naming, values);
            throw new IllegalArgumentException(msg);
        }
    }

    protected ENUM_PROPERTY_NAMING_TYPE getEnumPropertyNaming() {
        return enumPropertyNaming;
    }

    private String getNameUsingEnumPropertyNaming(String name) {
        switch (getEnumPropertyNaming()) {
            case original:
                return name;
            case camelCase:
                return camelize(underscore(name), true);
            case PascalCase:
                return camelize(underscore(name));
            case snake_case:
                return underscore(name);
            case UPPERCASE:
                return underscore(name).toUpperCase(Locale.ROOT);
            default:
                throw new IllegalArgumentException("Unsupported enum property naming: '" + name);
        }
    }

    @Override
    protected void addImport(CodegenModel m, String type) {
        if (type == null) {
            return;
        }

        String[] parts = splitComposedType(type);
        for (String s : parts) {
            if (needToImport(s)) {
                m.imports.add(s);
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            // name enum with model name, e.g. StatusEnum => Pet.StatusEnum
            for (CodegenProperty var : cm.vars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + classEnumSeparator + var.enumName);
                }
            }
            if (cm.parent != null) {
                for (CodegenProperty var : cm.allVars) {
                    if (Boolean.TRUE.equals(var.isEnum)) {
                        var.datatypeWithEnum = var.datatypeWithEnum
                                .replace(var.enumName, cm.classname + classEnumSeparator + var.enumName);
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.discriminator != null && cm.children != null) {
                    for (CodegenModel child : cm.children) {
                        this.setDiscriminatorValue(child, cm.discriminator.getPropertyName(), this.getDiscriminatorValue(child));
                    }
                }
            }
        }
        return result;
    }

    public void setSupportsES6(Boolean value) {
        supportsES6 = value;
    }

    public Boolean getSupportsES6() {
        return supportsES6;
    }

    public Boolean getNullSafeAdditionalProps() {
        return nullSafeAdditionalProps;
    }

    public void setNullSafeAdditionalProps(Boolean value) {
        nullSafeAdditionalProps = value;
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    private void setDiscriminatorValue(CodegenModel model, String baseName, String value) {
        for (CodegenProperty prop : model.allVars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
        if (model.children != null) {
            final boolean newDiscriminator = model.discriminator != null;
            for (CodegenModel child : model.children) {
                this.setDiscriminatorValue(child, baseName, newDiscriminator ? value : this.getDiscriminatorValue(child));
            }
        }
    }

    private String getDiscriminatorValue(CodegenModel model) {
        return model.vendorExtensions.containsKey(X_DISCRIMINATOR_TYPE) ?
                (String) model.vendorExtensions.get(X_DISCRIMINATOR_TYPE) : model.classname;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");

    }

    @Override
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        // replace ' with \'
        return super.escapeText(input).replace("\'", "\\\'");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);

        if (file == null) {
            return;
        }
        String tsPostProcessFile = System.getenv("TS_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(tsPostProcessFile)) {
            return; // skip if TS_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with ts extension
        if ("ts".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = tsPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
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
    public String toAnyOfName(List<String> names, ComposedSchema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getAnyOf());

        return String.join(" | ", types);
    }

    @Override
    public String toOneOfName(List<String> names, ComposedSchema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getOneOf());

        return String.join(" | ", types);
    }

    @Override
    public String toAllOfName(List<String> names, ComposedSchema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getAllOf());

        return String.join(" & ", types);
    }

    /**
     * Extracts the list of type names from a list of schemas.
     * Excludes `AnyType` if there are other valid types extracted.
     *
     * @param schemas list of schemas
     * @return list of types
     */
    protected List<String> getTypesFromSchemas(List<Schema> schemas) {
        List<Schema> filteredSchemas = schemas.size() > 1
            ? schemas.stream().filter(schema -> !"AnyType".equals(super.getSchemaType(schema))).collect(Collectors.toList())
            : schemas;

        return filteredSchemas.stream().map(schema -> {
            String schemaType = getSchemaType(schema);
            if (ModelUtils.isArraySchema(schema)) {
                ArraySchema ap = (ArraySchema) schema;
                Schema inner = ap.getItems();
                schemaType = schemaType + "<" + getSchemaType(inner) + ">";
            }
            return schemaType;
        }).distinct().collect(Collectors.toList());
    }
}
