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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class NimClientCodegen extends DefaultCodegen implements CodegenConfig {
    final Logger LOGGER = LoggerFactory.getLogger(NimClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";

    @Setter protected String packageName = "openapiclient";
    @Setter protected String packageVersion = "1.0.0";

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "nim";
    }

    @Override
    public String getHelp() {
        return "Generates a nim client (beta).";
    }

    public NimClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
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

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "nim";
        modelTemplateFiles.put("model.mustache", ".nim");
        apiTemplateFiles.put("api.mustache", ".nim");
        embeddedTemplateDir = templateDir = "nim-client";
        apiPackage = File.separator + packageName + File.separator + "apis";
        modelPackage = File.separator + packageName + File.separator + "models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("sample_client.mustache", "", "sample_client.nim"));
        supportingFiles.add(new SupportingFile("config.mustache", "", "config.nim"));

        setReservedWordsLowerCase(
                Arrays.asList(
                        "addr", "and", "as", "asm",
                        "bind", "block", "break",
                        "case", "cast", "concept", "const", "continue", "converter",
                        "defer", "discard", "distinct", "div", "do",
                        "elif", "else", "end", "enum", "except", "export",
                        "finally", "for", "from", "func",
                        "if", "import", "in", "include", "interface", "is", "isnot", "iterator",
                        "let",
                        "macro", "method", "mixin", "mod",
                        "nil", "not", "notin",
                        "object", "of", "or", "out",
                        "proc", "ptr",
                        "raise", "ref", "return",
                        "shl", "shr", "static",
                        "template", "try", "tuple", "type",
                        "using",
                        "var",
                        "when", "while",
                        "xor",
                        "yield"
                )
        );

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "array",
                        "map"
                )
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "int",
                        "int8",
                        "int16",
                        "int32",
                        "int64",
                        "uint",
                        "uint8",
                        "uint16",
                        "uint32",
                        "uint64",
                        "float",
                        "float32",
                        "float64",
                        "bool",
                        "char",
                        "string",
                        "cstring",
                        "pointer")
        );

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "float64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("password", "string");
        typeMapping.put("file", "string");
        typeMapping.put("object", "JsonNode");
        typeMapping.put("AnyType", "JsonNode");
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = postProcessModelsEnum(objs);

        // Mark top-level string enums for proper enum generation in template
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.isEnum && cm.allowableValues != null && cm.allowableValues.containsKey("enumVars")) {
                cm.vendorExtensions.put("x-is-top-level-enum", true);
            }

            // Fix dataType fields that contain underscored type names
            // This handles cases like Table[string, Record_string__foo__value]
            for (CodegenProperty var : cm.vars) {
                if (var.dataType != null && var.dataType.contains("Record_")) {
                    var.dataType = fixRecordTypeReferences(var.dataType);
                }
                if (var.datatypeWithEnum != null && var.datatypeWithEnum.contains("Record_")) {
                    var.datatypeWithEnum = fixRecordTypeReferences(var.datatypeWithEnum);
                }
            }
        }

        return objs;
    }

    /**
     * Fix underscored Record type references in dataType strings.
     * Converts Record_string__foo___value to RecordStringFooValue.
     */
    private String fixRecordTypeReferences(String typeString) {
        if (typeString == null || !typeString.contains("Record_")) {
            return typeString;
        }

        // Pattern to match Record_string_... type names with underscores
        // These are embedded in strings like: Table[string, Record_string__foo__value]
        String result = typeString;

        // Match Record_ followed by any characters until end or comma/bracket
        Pattern pattern = Pattern.compile("Record_[a-z_]+");
        java.util.regex.Matcher matcher = pattern.matcher(result);

        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            String matched = matcher.group();
            // Camelize the matched Record type name
            String camelized = camelize(matched);
            matcher.appendReplacement(sb, camelized);
        }
        matcher.appendTail(sb);

        return sb.toString();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        apiPackage = File.separator + packageName + File.separator + "apis";
        modelPackage = File.separator + packageName + File.separator + "models";
        supportingFiles.add(new SupportingFile("lib.mustache", "", packageName + ".nim"));
        supportingFiles.add(new SupportingFile("model_any_type.mustache", packageName + File.separator + "models", "model_any_type.nim"));
        supportingFiles.add(new SupportingFile("model_object.mustache", packageName + File.separator + "models", "model_object.nim"));
    }

    @Override
    public String escapeReservedWord(String name) {
        LOGGER.warn("A reserved word \"{}\" is used. Consider renaming the field name", name);
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "`" + name + "`";
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String toModelImport(String name) {
        name = normalizeSchemaName(name);
        name = name.replaceAll("-", "_");

        if (importMapping.containsKey(name)) {
            return sanitizeNimIdentifier("model_" + StringUtils.underscore(importMapping.get(name)));
        } else {
            return sanitizeNimIdentifier("model_" + StringUtils.underscore(name));
        }
    }

    @Override
    public String toApiImport(String name) {
        name = name.replaceAll("-", "_");
        if (importMapping.containsKey(name)) {
            return sanitizeNimIdentifier("api_" + StringUtils.underscore(importMapping.get(name)));
        } else {
            return sanitizeNimIdentifier("api_" + StringUtils.underscore(name));
        }
    }

    /**
     * Normalize schema names to ensure consistency across filename, import, and type name generation.
     * This is called early in the pipeline so downstream methods work with consistent names.
     */
    private String normalizeSchemaName(String name) {
        if (name == null) {
            return null;
        }
        // Remove underscores around and before digits (HTTP status codes, version numbers, etc.)
        // e.g., "GetComments_200_response" -> "GetComments200response"
        // e.g., "Config_anyOf_1" -> "ConfiganyOf1"
        // This ensures consistent handling whether the name comes with or without underscores
        name = name.replaceAll("_(\\d+)_", "$1");  // Underscores on both sides
        name = name.replaceAll("_(\\d+)$", "$1");   // Trailing underscore before digits
        return name;
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        // Normalize the schema name before any processing
        name = normalizeSchemaName(name);
        return super.fromModel(name, schema);
    }

    @Override
    public String toModelName(String name) {
        // Name should be normalized by fromModel, but normalize again for safety
        name = normalizeSchemaName(name);
        return camelize(sanitizeName(name));
    }

    @Override
    public String toModelFilename(String name) {
        name = normalizeSchemaName(name);
        name = name.replaceAll("-", "_");
        return sanitizeNimIdentifier("model_" + StringUtils.underscore(name));
    }

    @Override
    public String toApiFilename(String name) {
        name = name.replaceAll("-", "_");
        return sanitizeNimIdentifier("api_" + StringUtils.underscore(name));
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        if (isReservedWord(sanitizedOperationId)) {
            sanitizedOperationId = "call" + StringUtils.camelize(sanitizedOperationId);
        }

        return StringUtils.camelize(sanitizedOperationId, LOWERCASE_FIRST_LETTER);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();
        for (CodegenOperation operation : operations) {
            operation.httpMethod = operation.httpMethod.toLowerCase(Locale.ROOT);

            // Set custom flag for DELETE operations with body to use different template logic
            // Nim's httpClient.delete() doesn't support a body parameter
            if ("delete".equals(operation.httpMethod) && operation.getHasBodyParam()) {
                operation.vendorExtensions.put("x-nim-delete-with-body", true);
            }
        }

        return objs;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            if (inner == null) {
                return null;
            }
            return "seq[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            if (inner == null) {
                inner = new StringSchema();
            }
            return "Table[string, " + getTypeDeclaration(inner) + "]";
        }

        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (schemaType.matches("\\d.*")) { // starts with number
            return "`" + schemaType + "`";
        } else {
            return schemaType;
        }
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name, "\\W-[\\$]"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if ("_".equals(name)) {
            name = "_u";
        }

        // numbers are not allowed at the beginning
        if (name.matches("^\\d.*")) {
            name = "`" + name + "`";
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z0-9_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, LOWERCASE_FIRST_LETTER);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    protected boolean needToImport(String type) {
        if (defaultIncludes.contains(type)) {
            return false;
        } else if (languageSpecificPrimitives.contains(type)) {
            return false;
        } else if (typeMapping.containsKey(type) && languageSpecificPrimitives.contains(typeMapping.get(type))) {
            return false;
        }

        return true;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String name = StringUtils.camelize(property.name);

        if (name.matches("\\d.*")) { // starts with number
            return "`" + name + "`";
        } else {
            return name;
        }
    }

    private boolean isValidIdentifier(String identifier) {
        //see https://nim-lang.org/docs/manual.html#lexical-analysis-identifiers-amp-keywords
        return identifier.matches("^(?:[A-Z]|[a-z]|[\\x80-\\xff])(_?(?:[A-Z]|[a-z]|[\\x80-\\xff]|[0-9]))*$");
    }

    /**
     * Sanitize a Nim identifier by removing trailing underscores and collapsing multiple underscores.
     * Nim does not allow identifiers to end with underscores.
     *
     * @param name the identifier to sanitize
     * @return the sanitized identifier
     */
    private String sanitizeNimIdentifier(String name) {
        if (name == null || name.isEmpty()) {
            return name;
        }
        // Remove trailing underscores (Nim identifiers cannot end with underscore)
        name = name.replaceAll("_+$", "");
        // Collapse multiple consecutive underscores to single underscore
        name = name.replaceAll("_+", "_");
        return name;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        name = name.replace(" ", "_");
        name = StringUtils.camelize(name);

        // starts with number or contains any character not allowed,see
        if (isValidIdentifier(name)) {
            return name;
        } else {
            return "`" + name + "`";
        }
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.NIM;
    }
}
