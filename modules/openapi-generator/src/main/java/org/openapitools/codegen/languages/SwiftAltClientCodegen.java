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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.time.OffsetDateTime;
import java.time.Instant;
import java.time.temporal.ChronoField;
import java.util.concurrent.TimeUnit;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class SwiftAltClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(SwiftAltClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    protected String projectName = "OpenAPIClient";
    protected String privateFolder = "Sources/Private";
    protected String sourceFolder = "Sources";
    protected String transportFolder = "OpenAPITransport";

    /**
     * Constructor for the swift alt language codegen module.
     */
    public SwiftAltClientCodegen() {
        super();
        this.useOneOfInterfaces = true;

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code" + File.separator + "swift";
        modelTemplateFiles.put("model.mustache", ".swift");
        apiTemplateFiles.put("api.mustache", ".swift");
        embeddedTemplateDir = templateDir = "swift-alt";
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
                        "Any",
                        "Decimal")
        );

        reservedWords = new HashSet<>(
                Arrays.asList(
                        // name used by swift client
                        "ErrorResponse", "Response",

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
                        "URL", "Data", "Codable", "Encodable", "Decodable",

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
        typeMapping.put("file", "URL");
        typeMapping.put("binary", "URL");
        typeMapping.put("ByteArray", "Data");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("URI", "String");
        typeMapping.put("decimal", "Decimal");
        typeMapping.put("object", "AnyCodable");
        typeMapping.put("AnyType", "AnyCodable");
        typeMapping.put("file", "Data");
        typeMapping.put("binary", "Data");

        importMapping = new HashMap<>();

        cliOptions.add(new CliOption(PROJECT_NAME, "Project name in Xcode"));
        cliOptions.add(new CliOption(CodegenConstants.API_NAME_PREFIX, CodegenConstants.API_NAME_PREFIX_DESC));
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "Library template (sub-template) to use");
    }

    private static CodegenModel reconcileProperties(CodegenModel codegenModel,
                                                    CodegenModel parentCodegenModel) {
        // To support inheritance in this generator, we will analyze
        // the parent and child models, look for properties that match, and remove
        // them from the child models and leave them in the parent.
        // Because the child models extend the parents, the properties
        // will be available via the parent.

        // Get the properties for the parent and child models
        final List<CodegenProperty> parentModelCodegenProperties = parentCodegenModel.vars;
        List<CodegenProperty> codegenProperties = codegenModel.vars;
        codegenModel.allVars = new ArrayList<CodegenProperty>(codegenProperties);
        codegenModel.parentVars = parentCodegenModel.allVars;

        // Iterate over all of the parent model properties
        boolean removedChildProperty = false;

        for (CodegenProperty parentModelCodegenProperty : parentModelCodegenProperties) {
            // Now that we have found a prop in the parent class,
            // and search the child class for the same prop.
            Iterator<CodegenProperty> iterator = codegenProperties.iterator();
            while (iterator.hasNext()) {
                CodegenProperty codegenProperty = iterator.next();
                if (codegenProperty.baseName.equals(parentModelCodegenProperty.baseName)) {
                    // We found a property in the child class that is
                    // a duplicate of the one in the parent, so remove it.
                    iterator.remove();
                    removedChildProperty = true;
                }
            }
        }

        if (removedChildProperty) {
            codegenModel.vars = codegenProperties;
        }

        return codegenModel;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "swift-alt";
    }

    @Override
    public String getHelp() {
        return "Generates a Swift 5 alternative client library.";
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel,
                                                       Schema schema) {

        final Schema additionalProperties = getAdditionalProperties(schema);

        if (additionalProperties != null) {
            Schema inner = null;
            if (ModelUtils.isArraySchema(schema)) {
                ArraySchema ap = (ArraySchema) schema;
                inner = ap.getItems();
            } else if (ModelUtils.isMapSchema(schema)) {
                inner = getAdditionalProperties(schema);
            }

            codegenModel.additionalPropertiesType = inner != null ? getTypeDeclaration(inner) : getSchemaType(additionalProperties);
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

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
    }

    @Override
    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word); //don't lowercase as super does
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
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return ModelUtils.isSet(p) ? "Set<" + getTypeDeclaration(inner) + ">" : "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return "[String: " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
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
                if (ModelUtils.isStringSchema(p)) {
                    return "." + toEnumVarName(escapeText((String) p.getDefault()), p.getType());
                } else {
                    return "." + toEnumVarName(escapeText(p.getDefault().toString()), p.getType());
                }
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
            } else if (ModelUtils.isStringSchema(p)) {
                return "\"" + escapeText((String) p.getDefault()) + "\"";
            }
            // TODO: Handle more cases from `ModelUtils`, such as Date
        }
        return null;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return getSchemaType(getAdditionalProperties(p));
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
        operationId = camelize(sanitizeName(operationId), true);

        // Throw exception if method name is empty.
        // This should not happen but keep the check just in case
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(("call_" + operationId), true);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, camelize(sanitizeName("call_" + operationId), true));
            operationId = camelize(sanitizeName("call_" + operationId), true);
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
        name = camelize(name, true);

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
        name = camelize(name, true);

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
    public CodegenModel fromModel(String name, Schema model) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel codegenModel = super.fromModel(name, model);
        if (codegenModel.description != null) {
            codegenModel.imports.add("ApiModel");
        }
        if (allDefinitions != null) {
            String parentSchema = codegenModel.parentSchema;

            // multilevel inheritance: reconcile properties of all the parents
            while (parentSchema != null) {
                final Schema parentModel = allDefinitions.get(parentSchema);
                final CodegenModel parentCodegenModel = super.fromModel(codegenModel.parent,
                        parentModel);
                codegenModel = SwiftAltClientCodegen.reconcileProperties(codegenModel, parentCodegenModel);

                // get the next parent
                parentSchema = parentCodegenModel.parentSchema;
            }
        }
        return codegenModel;
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

            return "_" + startingNumbers + camelize(nameWithoutStartingNumbers, true);
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(WordUtils.capitalizeFully(getSymbolName(name).toUpperCase(Locale.ROOT)), true);
        }

        // Camelize only when we have a structure defined below
        Boolean camelized = false;
        if (name.matches("[A-Z][a-z0-9]+[a-zA-Z0-9]*")) {
            name = camelize(name, true);
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
                true);
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
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> postProcessedModelsEnum = postProcessModelsEnum(objs);

        // We iterate through the list of models, and also iterate through each of the
        // properties for each model. For each property, if:
        //
        // CodegenProperty.name != CodegenProperty.baseName
        //
        // then we set
        //
        // CodegenProperty.vendorExtensions["x-codegen-escaped-property-name"] = true
        //
        // Also, if any property in the model has x-codegen-escaped-property-name=true, then we mark:
        //
        // CodegenModel.vendorExtensions["x-codegen-has-escaped-property-names"] = true
        //
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
            }
            if (modelHasPropertyWithEscapedName) {
                cm.vendorExtensions.put("x-codegen-has-escaped-property-names", true);
            }
        }

        return postProcessedModelsEnum;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        boolean isSwiftScalarType = property.isInteger || property.isLong || property.isFloat
                || property.isDouble || property.isBoolean;
        if ((!property.required || property.isNullable) && isSwiftScalarType) {
            // Optional scalar types like Int?, Int64?, Float?, Double?, and Bool?
            // do not translate to Objective-C. So we want to flag those
            // properties in case we want to put special code in the templates
            // which provide Objective-C compatibility.
            property.vendorExtensions.put("x-swift-optional-scalar", true);
        }
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");

        HashMap<String, CodegenModel> modelMaps = new HashMap<String, CodegenModel>();
        for (Object o : allModels) {
            HashMap<String, Object> h = (HashMap<String, Object>) o;
            CodegenModel m = (CodegenModel) h.get("model");
            modelMaps.put(m.classname, m);
        }

        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            for (CodegenParameter cp : operation.allParams) {
                cp.vendorExtensions.put("x-swift-example", constructExampleCode(cp, modelMaps, new HashSet<String>()));
            }
        }
        return objs;
    }

    public String constructExampleCode(CodegenParameter codegenParameter, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        if (codegenParameter.isArray) { // array
            return "[" + constructExampleCode(codegenParameter.items, modelMaps, visitedModels) + "]";
        } else if (codegenParameter.isMap) { // TODO: map, file type
            return "\"TODO\"";
        } else if (languageSpecificPrimitives.contains(codegenParameter.dataType)) { // primitive type
            if ("String".equals(codegenParameter.dataType) || "Character".equals(codegenParameter.dataType)) {
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return "\"" + codegenParameter.example + "\"";
                } else {
                    return "\"" + codegenParameter.paramName + "_example\"";
                }
            } else if ("Bool".equals(codegenParameter.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenParameter.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if ("URL".equals(codegenParameter.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("Data".equals(codegenParameter.dataType)) { // URL
                return "Data([9, 8, 7])";
            } else if ("Date".equals(codegenParameter.dataType)) { // date
                return "Date()";
            } else { // numeric
                if (StringUtils.isEmpty(codegenParameter.example)) {
                    return codegenParameter.example;
                } else {
                    return "987";
                }
            }
        } else { // model
            // look up the model
            if (modelMaps.containsKey(codegenParameter.dataType)) {
                if (visitedModels.contains(codegenParameter.dataType)) {
                    // recursive/self-referencing model, simply return nil to avoid stackoverflow
                    return "nil";
                } else {
                    visitedModels.add(codegenParameter.dataType);
                    return constructExampleCode(modelMaps.get(codegenParameter.dataType), modelMaps, visitedModels);
                }
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenParameter.dataType);
                return "TODO";
            }
        }
    }

    public String constructExampleCode(CodegenProperty codegenProperty, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        if (codegenProperty.isArray) { // array
            return "[" + constructExampleCode(codegenProperty.items, modelMaps, visitedModels) + "]";
        } else if (codegenProperty.isMap) { // TODO: map, file type
            return "\"TODO\"";
        } else if (languageSpecificPrimitives.contains(codegenProperty.dataType)) { // primitive type
            if ("String".equals(codegenProperty.dataType) || "Character".equals(codegenProperty.dataType)) {
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return "\"" + codegenProperty.example + "\"";
                } else {
                    return "\"" + codegenProperty.name + "_example\"";
                }
            } else if ("Bool".equals(codegenProperty.dataType)) { // boolean
                if (Boolean.parseBoolean(codegenProperty.example)) {
                    return "true";
                } else {
                    return "false";
                }
            } else if ("URL".equals(codegenProperty.dataType)) { // URL
                return "URL(string: \"https://example.com\")!";
            } else if ("Date".equals(codegenProperty.dataType)) { // date
                return "Date()";
            } else { // numeric
                if (StringUtils.isEmpty(codegenProperty.example)) {
                    return codegenProperty.example;
                } else {
                    return "123";
                }
            }
        } else {
            // look up the model
            if (modelMaps.containsKey(codegenProperty.dataType)) {
                if (visitedModels.contains(codegenProperty.dataType)) {
                    // recursive/self-referencing model, simply return nil to avoid stackoverflow
                    return "nil";
                } else {
                    visitedModels.add(codegenProperty.dataType);
                    return constructExampleCode(modelMaps.get(codegenProperty.dataType), modelMaps, visitedModels);
                }
            } else {
                //LOGGER.error("Error in constructing examples. Failed to look up the model " + codegenProperty.dataType);
                return "\"TODO\"";
            }
        }
    }

    public String constructExampleCode(CodegenModel codegenModel, HashMap<String, CodegenModel> modelMaps, Set<String> visitedModels) {
        String example;
        example = codegenModel.name + "(";
        List<String> propertyExamples = new ArrayList<>();
        for (CodegenProperty codegenProperty : codegenModel.vars) {
            propertyExamples.add(codegenProperty.name + ": " + constructExampleCode(codegenProperty, modelMaps, visitedModels));
        }
        example += StringUtils.join(propertyExamples, ", ");
        example += ")";
        return example;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# swift alternative generator #");
        System.out.println("################################################################################");
    }
}
