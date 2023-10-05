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
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.time.OffsetDateTime;
import java.time.Instant;
import java.time.temporal.ChronoField;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class SwiftCombineClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(SwiftCombineClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    public static final String MAP_FILE_BINARY_TO_DATA = "mapFileBinaryToData";
    protected String projectName = "OpenAPIClient";
    protected String privateFolder = "Sources/Private";
    protected String sourceFolder = "Sources";
    protected String transportFolder = "OpenAPITransport";
    protected List<String> notCodableTypes = Arrays.asList("Any", "AnyObject", "[String: Any]", "[String: [String: Any]]", "[Any]");
    protected boolean mapFileBinaryToData = true;

    protected boolean anyDecoderWasAdded = false;

    /**
     * Constructor for the swift language codegen module.
     */
    public SwiftCombineClientCodegen() {
        super();
        this.supportsMultipleInheritance = true;
        this.useOneOfInterfaces = true;
        this.supportsAdditionalPropertiesWithComposedSchema = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + "swift";
        modelTemplateFiles.put("model.mustache", ".swift");
        apiTemplateFiles.put("api.mustache", ".swift");
        embeddedTemplateDir = templateDir = "swift-combine";
        apiPackage = File.separator + "APIs";
        modelPackage = File.separator + "Models";

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Int",
                        "Int32",
                        "Int64",
                        "Float",
                        "Double",
                        "Bool",
                        "Void",
                        "String",
                        "Data",
                        "Date",
                        "Character",
                        "UUID",
                        "URL",
                        "AnyObject",
                        "Any",
                        "[String: Any]",
                        "Decimal")
        );
        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "Data",
                        "Date",
                        "URL", // for file
                        "UUID",
                        "Array",
                        "Dictionary",
                        "Set",
                        "Any",
                        "Empty",
                        "AnyObject",
                        "Decimal")
        );

        reservedWords = new HashSet<>(
                Arrays.asList(
                        // Swift keywords. This list is taken from here:
                        // https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410
                        //
                        // Keywords used in declarations
                        "associatedtype", "class", "deinit", "enum", "extension", "fileprivate", "func", "import", "init",
                        "inout", "internal", "let", "open", "operator", "private", "protocol", "public", "static", "struct",
                        "subscript", "typealias", "var",
                        // Keywords uses in statements
                        "break", "case", "continue", "default", "defer", "do", "else", "fallthrough", "for", "guard", "if",
                        "in", "repeat", "return", "switch", "where", "while",
                        // Keywords used in expressions and types
                        "as", "Any", "catch", "false", "is", "nil", "rethrows", "super", "self", "Self", "throw", "throws", "true", "try",
                        // Keywords used in patterns
                        "_",
                        // Keywords that begin with a number sign
                        "#available", "#colorLiteral", "#column", "#else", "#elseif", "#endif", "#file", "#fileLiteral", "#function", "#if",
                        "#imageLiteral", "#line", "#selector", "#sourceLocation",
                        // Keywords reserved in particular contexts
                        "associativity", "convenience", "dynamic", "didSet", "final", "get", "infix", "indirect", "lazy", "left",
                        "mutating", "none", "nonmutating", "optional", "override", "postfix", "precedence", "prefix", "Protocol",
                        "required", "right", "set", "Type", "unowned", "weak", "willSet",

                        //
                        // Swift Standard Library types
                        // https://developer.apple.com/documentation/swift
                        //
                        // Numbers and Basic Values
                        "Bool", "Int", "Double", "Float", "Range", "ClosedRange", "Error", "Optional",
                        // Special-Use Numeric Types
                        "UInt", "UInt8", "UInt16", "UInt32", "UInt64", "Int8", "Int16", "Int32", "Int64", "Float80", "Float32", "Float64",
                        // Strings and Text
                        "String", "Character", "Unicode", "StaticString",
                        // Collections
                        "Array", "Dictionary", "Set", "OptionSet", "CountableRange", "CountableClosedRange",

                        // The following are commonly-used Foundation types
                        "URL", "Data", "Codable", "Encodable", "Decodable", "Result",

                        // The following are other words we want to reserve
                        "Void", "AnyObject", "Class", "dynamicType", "COLUMN", "FILE", "FUNCTION", "LINE"
                )
        );

        typeMapping = new HashMap<>();
        typeMapping.put("array", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("set", "Set");
        typeMapping.put("date", "Date");
        typeMapping.put("Date", "Date");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "Character");
        typeMapping.put("short", "Int");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Int64");
        typeMapping.put("integer", "Int");
        typeMapping.put("Integer", "Int");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "Double");
        typeMapping.put("double", "Double");
        typeMapping.put("ByteArray", "Data");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("URI", "String");
        typeMapping.put("decimal", "Decimal");
        typeMapping.put("object", "[String: Any]");
        typeMapping.put("AnyType", "Any");
        typeMapping.put("file", "Data");
        typeMapping.put("binary", "Data");

        instantiationTypes.put("array", "Array");
        instantiationTypes.put("list", "Array");

        importMapping = new HashMap<>();

        cliOptions.add(new CliOption(PROJECT_NAME, "Project name in Xcode"));
        cliOptions.add(new CliOption(CodegenConstants.API_NAME_PREFIX, CodegenConstants.API_NAME_PREFIX_DESC));
        cliOptions.add(new CliOption(MAP_FILE_BINARY_TO_DATA,
                "Map File and Binary to Data (default: true)")
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "swift-combine";
    }

    @Override
    public String getHelp() {
        return "Generates a Swift Combine client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        anyDecoderWasAdded = false;
        if (StringUtils.isEmpty(System.getenv("SWIFT_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable SWIFT_POST_PROCESS_FILE not defined so the Swift code may not be properly formatted. To define it, try 'export SWIFT_POST_PROCESS_FILE=/usr/local/bin/swiftformat' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        // Setup project name
        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(PROJECT_NAME));
        } else {
            additionalProperties.put(PROJECT_NAME, projectName);
        }

        supportingFiles.add(new SupportingFile("Package.mustache",
                projectName,
                "Package.swift"));
        supportingFiles.add(new SupportingFile("OpenAPITransportPackage.mustache",
                transportFolder,
                "Package.swift"));
        supportingFiles.add(new SupportingFile("OpenAPITransport.mustache",
                transportFolder + File.separator + sourceFolder,
                "OpenAPITransport.swift"));
        supportingFiles.add(new SupportingFile("OpenISO8601DateFormatter.mustache",
                projectName + File.separator + privateFolder,
                "OpenISO8601DateFormatter.swift"));
        if (additionalProperties.containsKey(MAP_FILE_BINARY_TO_DATA)) {
            mapFileBinaryToData = convertPropertyToBooleanAndWriteBack(MAP_FILE_BINARY_TO_DATA);
        }
        additionalProperties.put(MAP_FILE_BINARY_TO_DATA, mapFileBinaryToData);
        if (mapFileBinaryToData) {
            typeMapping.put("file", "Data");
            typeMapping.put("binary", "Data");
        } else {
            typeMapping.put("file", "URL");
            typeMapping.put("binary", "URL");
        }
    }

    @Override
    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word); //don't lowercase as super does
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + projectName + File.separator + sourceFolder
                + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + projectName + File.separator + sourceFolder
                + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = ModelUtils.unaliasSchema(this.openAPI, p, importMapping);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        if (ModelUtils.isArraySchema(target)) {
            Schema<?> items = getSchemaItems((ArraySchema) schema);
            return ModelUtils.isSet(target) && ModelUtils.isObjectSchema(items) ? "Set<" + getTypeDeclaration(items) + ">" : "[" + getTypeDeclaration(items) + "]";
        } else if (ModelUtils.isMapSchema(target)) {
            // Note: ModelUtils.isMapSchema(p) returns true when p is a composed schema that also defines
            // additionalproperties: true
            Schema<?> inner = ModelUtils.getAdditionalProperties(target);
            if (inner == null) {
                LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", p.getName());
                inner = new StringSchema().description("TODO default missing map inner type to string");
                p.setAdditionalProperties(inner);
            }
            return "[String: " + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isComposedSchema(target)) {
            List<Schema> schemas = ModelUtils.getInterfaces((ComposedSchema) target);
            if (schemas.size() == 1) {
                return getTypeDeclaration(schemas.get(0));
            } else {
                super.getTypeDeclaration(target);
            }
        }
        return super.getTypeDeclaration(target);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type) || defaultIncludes.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public boolean isDataTypeFile(String dataType) {
        return "URL".equals(dataType);
    }

    @Override
    public boolean isDataTypeBinary(final String dataType) {
        return "Data".equals(dataType);
    }

    /**
     * Output the proper model name (capitalized).
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(String name) {
        // FIXME parameter should not be assigned. Also declare it as "final"
        name = sanitizeName(name);

        if (!StringUtils.isEmpty(modelNameSuffix)) { // set model suffix
            name = name + "_" + modelNameSuffix;
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) { // set model prefix
            name = modelNamePrefix + "_" + name;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            // e.g. 200Response => Model200Response (after camelize)
            String modelName = "Model" + name;
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return name;
    }

    /**
     * Return the capitalized file name of the model.
     *
     * @param name the model name
     * @return the file name of the model
     */
    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getEnum() != null && !p.getEnum().isEmpty()) {
            if (p.getDefault() != null) {
                return "." + toEnumVarName(escapeText(String.valueOf(p.getDefault())), p.getType());
            }
        }
        if (p.getDefault() != null) {
            if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
                return p.getDefault().toString();
            } else if (ModelUtils.isDateTimeSchema(p)) {
                // Datetime time stamps in Swift are expressed as Seconds with Microsecond precision.
                // In Java, we need to be creative to get the Timestamp in Microseconds as a long.
                Instant instant = ((OffsetDateTime) p.getDefault()).toInstant();
                long epochMicro = TimeUnit.SECONDS.toMicros(instant.getEpochSecond()) + (instant.get(ChronoField.MICRO_OF_SECOND));
                return "Date(timeIntervalSince1970: " + String.valueOf(epochMicro) + ".0 / 1_000_000)";
            } else if (ModelUtils.isUUIDSchema(p)) {
                return "\"" + escapeText(String.valueOf(p.getDefault())) + "\"";
            } else if (ModelUtils.isStringSchema(p)) {
                return "\"" + escapeText(String.valueOf(p.getDefault())) + "\"";
            }
            // TODO: Handle more cases from `ModelUtils`, such as Date
        }
        return null;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return getSchemaType(ModelUtils.getAdditionalProperties(p));
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            return ModelUtils.isSet(p) ? "Set<" + inner + ">" : "[" + inner + "]";
        }
        return null;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultAPI";
        }
        return camelize(apiNamePrefix + "_" + name) + "API";
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = camelize(sanitizeName(operationId), LOWERCASE_FIRST_LETTER);

        // Throw exception if method name is empty.
        // This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("call_" + operationId), LOWERCASE_FIRST_LETTER);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), LOWERCASE_FIRST_LETTER));
            operationId = camelize(sanitizeName("call_" + operationId), LOWERCASE_FIRST_LETTER);
        }


        return operationId;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        // for reserved words surround with `` or append _
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, append _
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // sanitize name
        name = sanitizeName(name);

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        // for reserved words surround with ``
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // for words starting with number, append _
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        return name;
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

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // for string, array of string
        if ("String".equals(datatype) || "[String]".equals(datatype) || "[String: String]".equals(datatype)) {
            return "\"" + String.valueOf(value) + "\"";
        } else {
            return String.valueOf(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "empty";
        }

        Pattern startWithNumberPattern = Pattern.compile("^\\d+");
        Matcher startWithNumberMatcher = startWithNumberPattern.matcher(name);
        if (startWithNumberMatcher.find()) {
            String startingNumbers = startWithNumberMatcher.group(0);
            String nameWithoutStartingNumbers = name.substring(startingNumbers.length());

            return "_" + startingNumbers + camelize(nameWithoutStartingNumbers, LOWERCASE_FIRST_LETTER);
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(WordUtils.capitalizeFully(getSymbolName(name).toUpperCase(Locale.ROOT)), LOWERCASE_FIRST_LETTER);
        }

        // Camelize only when we have a structure defined below
        Boolean camelized = false;
        if (name.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            name = camelize(name, LOWERCASE_FIRST_LETTER);
            camelized = true;
        }

        // Reserved Name
        String nameLowercase = StringUtils.lowerCase(name);
        if (isReservedWord(nameLowercase)) {
            return escapeReservedWord(nameLowercase);
        }

        // Check for numerical conversions
        if ("Int".equals(datatype) || "Int32".equals(datatype) || "Int64".equals(datatype)
                || "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "number" + camelize(name);
            varName = varName.replaceAll("-", "minus");
            varName = varName.replaceAll("\\+", "plus");
            varName = varName.replaceAll("\\.", "dot");
            return varName;
        }

        // If we have already camelized the word, don't progress
        // any further
        if (camelized) {
            return name;
        }

        char[] separators = {'-', '_', ' ', ':', '(', ')'};
        return camelize(WordUtils.capitalizeFully(StringUtils.lowerCase(name), separators)
                        .replaceAll("[-_ :\\(\\)]", ""),
                LOWERCASE_FIRST_LETTER);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // Ensure that the enum type doesn't match a reserved word or
        // the variable name doesn't match the generated enum type or the
        // Swift compiler will generate an error
        if (isReservedWord(property.datatypeWithEnum)
                || toVarName(property.name).equals(property.datatypeWithEnum)) {
            enumName = property.datatypeWithEnum + "Enum";
        }

        // TODO: toModelName already does something for names starting with number,
        // so this code is probably never called
        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        ModelsMap postProcessedModelsEnum = postProcessModelsEnum(objs);
        List<Object> models = (List<Object>) postProcessedModelsEnum.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            boolean modelHasPropertyWithEscapedName = false;
            for (CodegenProperty prop : cm.allVars) {
                if (!prop.name.equals(prop.baseName)) {
                    prop.vendorExtensions.put("x-codegen-escaped-property-name", true);
                    modelHasPropertyWithEscapedName = true;
                }
                if (notCodableTypes.contains(prop.dataType) || notCodableTypes.contains(prop.baseType)) {
                    prop.vendorExtensions.put("x-swift-is-not-codable", true);
                }
                if (modelHasPropertyWithEscapedName || notCodableTypes.contains(prop.dataType) || notCodableTypes.contains(prop.baseType)) {
                    cm.vendorExtensions.put("x-swift-contains-not-codable", true);
                    addAnyDecoderIfNeeded();
                }
            }
            if (modelHasPropertyWithEscapedName) {
                cm.vendorExtensions.put("x-codegen-has-escaped-property-names", true);
            }
        }

        return postProcessedModelsEnum;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String swiftPostProcessFile = System.getenv("SWIFT_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(swiftPostProcessFile)) {
            return; // skip if SWIFT_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with swift extension
        if ("swift".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = swiftPostProcessFile + " " + file.toString();
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
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");

        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            operation.allParams.forEach(cp -> addVendorExtensions(cp, operation, modelMaps));
            operation.queryParams.forEach(cp -> addVendorExtensions(cp, operation, modelMaps));
            operation.headerParams.forEach(cp -> addVendorExtensions(cp, operation, modelMaps));
            operation.bodyParams.forEach(cp -> addVendorExtensions(cp, operation, modelMaps));
            operation.formParams.forEach(cp -> addFormVendorExtensions(cp, operation, modelMaps));
            if (notCodableTypes.contains(operation.returnType) || notCodableTypes.contains(operation.returnBaseType)) {
                operation.vendorExtensions.put("x-swift-is-not-codable", true);
                addAnyDecoderIfNeeded();
            }
            List<CodegenResponse> responses = operation.responses;
            for (CodegenResponse response : responses) {
                if (response.is4xx || response.is5xx) {
                    response.vendorExtensions.put("x-swift-has-custom-error-type", true);
                    response.vendorExtensions.put("x-swift-custom-error-type", WordUtils.capitalize(operation.operationId) + "Error");
                    operation.vendorExtensions.put("x-swift-custom-error-type", WordUtils.capitalize(operation.operationId) + "Error");
                }
                response.vendorExtensions.put("x-swift-is-response-code-explicit", !response.code.contains("x"));
            }
        }
        return objs;
    }

    protected void addVendorExtensions(CodegenParameter cp, CodegenOperation operation, HashMap<String, CodegenModel> modelMaps) {
        CodegenModel model = modelMaps.get(cp.dataType);
        cp.vendorExtensions.put("x-swift-use-encoder", cp.isModel);
        if (cp.isArray && cp.items != null) {
            CodegenModel baseModel = modelMaps.get(cp.items.dataType);
            boolean isBaseTypeEnum = cp.items.isEnum || cp.isEnum || (baseModel != null && baseModel.isEnum);
            cp.vendorExtensions.put("x-swift-is-base-type-enum", isBaseTypeEnum);
            boolean isBaseTypeUdid = cp.items.isUuid || cp.isUuid;
            cp.vendorExtensions.put("x-swift-is-base-type-udid", isBaseTypeUdid);

            boolean useEncoder = !isBaseTypeEnum && !cp.items.isString || (baseModel != null && !baseModel.isString);
            cp.vendorExtensions.put("x-swift-use-encoder", useEncoder);
        }
        if (cp.isEnum || (model != null && model.isEnum)) {
            cp.vendorExtensions.put("x-swift-is-enum-type", true);
        }
        if (cp.isEnum) {
            String newDataType = WordUtils.capitalize(operation.operationId) + WordUtils.capitalize(cp.enumName);
            cp.vendorExtensions.put("x-swift-nested-enum-type", newDataType);
            if (cp.isArray) {
                if (cp.uniqueItems) {
                    cp.dataType = "Set<" + newDataType + ">";
                } else {
                    cp.dataType = "[" + newDataType + "]";
                }
            } else {
                cp.baseType = cp.dataType;
                cp.dataType = newDataType;
            }
        }
    }

    protected void addFormVendorExtensions(CodegenParameter cp, CodegenOperation operation, HashMap<String, CodegenModel> modelMaps) {
        addVendorExtensions(cp, operation, modelMaps);
        if (operation.isMultipart && cp.isArray && cp.items.isFile) {
            cp.vendorExtensions.put("x-swift-enumerate-multipart", true);
        }
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# swift combine generator is contributed by @dydus0x14 and @ptiz.          #");
        System.out.println("################################################################################");
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.SWIFT;
    }

    protected void addAnyDecoderIfNeeded() {
        if (!anyDecoderWasAdded) {
            supportingFiles.add(new SupportingFile("AnyDecodable.mustache",
                    projectName + File.separator + privateFolder,
                    "AnyDecodable.swift"));
            anyDecoderWasAdded = true;
        }
    }
}
