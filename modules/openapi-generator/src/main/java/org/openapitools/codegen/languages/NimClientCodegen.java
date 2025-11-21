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
import java.util.regex.Matcher;
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
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf
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
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> allModels) {
        allModels = super.postProcessAllModels(allModels);

        // First pass: identify all models that have fields with custom JSON names
        Set<String> modelsWithCustomJson = new HashSet<>();

        for (Map.Entry<String, ModelsMap> entry : allModels.entrySet()) {
            ModelsMap modelsMap = entry.getValue();
            for (ModelMap mo : modelsMap.getModels()) {
                CodegenModel cm = mo.getModel();

                // Check if this model has fields with custom JSON names
                for (CodegenProperty var : cm.vars) {
                    if (var.vendorExtensions.containsKey("x-json-name")) {
                        modelsWithCustomJson.add(cm.classname);
                        break;
                    }
                }
            }
        }

        // Second pass: cascade custom JSON handling to parent models and mark array fields
        // We need multiple passes to handle transitive dependencies
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Map.Entry<String, ModelsMap> entry : allModels.entrySet()) {
                ModelsMap modelsMap = entry.getValue();
                for (ModelMap mo : modelsMap.getModels()) {
                    CodegenModel cm = mo.getModel();

                    // Check if any field's type needs custom JSON and mark array fields appropriately
                    for (CodegenProperty var : cm.vars) {
                        String fieldType = var.complexType != null ? var.complexType : var.baseType;

                        // Handle arrays - check if the inner type has custom JSON
                        if (var.isArray && var.items != null) {
                            String innerType = var.items.complexType != null ? var.items.complexType : var.items.baseType;
                            if (innerType != null && modelsWithCustomJson.contains(innerType)) {
                                // Mark this array field as containing types with custom JSON
                                var.vendorExtensions.put("x-is-array-with-custom-json", "true");
                                var.vendorExtensions.put("x-array-inner-type", innerType);
                            }
                            fieldType = innerType;
                        }

                        // Cascade custom JSON to parent model if not already marked
                        if (fieldType != null && modelsWithCustomJson.contains(fieldType)) {
                            if (!cm.vendorExtensions.containsKey("x-has-custom-json-names")) {
                                cm.vendorExtensions.put("x-has-custom-json-names", true);
                                modelsWithCustomJson.add(cm.classname);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }

        return allModels;
    }

    /**
     * Strips surrounding quotes from integer enum values.
     * The base OpenAPI Generator stores all enum values as quoted strings (e.g., "0", "1", "2")
     * regardless of the enum's actual type. For Nim integer enums, we need the raw numbers
     * without quotes so they serialize correctly: %(0) instead of %("0")
     */
    private void stripQuotesFromIntegerEnumValues(Map<String, Object> allowableValues) {
        if (allowableValues == null || !allowableValues.containsKey("enumVars")) {
            return;
        }

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");
        for (Map<String, Object> enumVar : enumVars) {
            Object value = enumVar.get("value");
            if (value instanceof String) {
                String strValue = (String) value;
                // Remove surrounding quotes if present
                if (strValue.startsWith("\"") && strValue.endsWith("\"")) {
                    enumVar.put("value", strValue.substring(1, strValue.length() - 1));
                }
            }
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = postProcessModelsEnum(objs);

        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();

            if (cm.isEnum && cm.allowableValues != null && cm.allowableValues.containsKey("enumVars")) {
                cm.vendorExtensions.put("x-is-top-level-enum", true);

                // For integer enums, strip quotes from enum values
                if (cm.vendorExtensions.containsKey("x-is-integer-enum")) {
                    stripQuotesFromIntegerEnumValues(cm.allowableValues);
                }
            }

            // Check if any fields need custom JSON name mapping
            boolean hasCustomJsonNames = false;

            // Fix dataType fields that contain underscored type names
            // This handles cases like Table[string, Record_string__foo__value]
            // Also wrap optional fields in Option[T]
            for (CodegenProperty var : cm.vars) {
                if (var.dataType != null && var.dataType.contains("Record_")) {
                    var.dataType = fixRecordTypeReferences(var.dataType);
                }
                if (var.datatypeWithEnum != null && var.datatypeWithEnum.contains("Record_")) {
                    var.datatypeWithEnum = fixRecordTypeReferences(var.datatypeWithEnum);
                }

                // Check if the field name was changed from the original (baseName)
                // This happens for fields like "_id" which are renamed to "id"
                // But we need to exclude cases where the name is just escaped with backticks
                // (e.g., "from" becomes "`from`" because it's a reserved word)
                if (var.baseName != null && !var.baseName.equals(var.name)) {
                    // Check if this is just a reserved word escaping (name is `baseName`)
                    String escapedName = "`" + var.baseName + "`";
                    if (!var.name.equals(escapedName)) {
                        // This is a real rename, not just escaping
                        var.vendorExtensions.put("x-json-name", var.baseName);
                        hasCustomJsonNames = true;
                    }
                }

                // Wrap optional (non-required) or nullable fields in Option[T]
                // For non-enum fields only (enums are handled specially in the template)
                if ((!var.required || var.isNullable) && !var.isReadOnly && !var.isEnum) {
                    String baseType = var.dataType;
                    if (baseType != null && !baseType.startsWith("Option[")) {
                        var.dataType = "Option[" + baseType + "]";
                        if (var.datatypeWithEnum != null) {
                            var.datatypeWithEnum = "Option[" + var.datatypeWithEnum + "]";
                        }
                    }
                }

                // For enum fields, set x-is-optional if they are not required
                if (var.isEnum && (!var.required || var.isNullable)) {
                    var.vendorExtensions.put("x-is-optional", true);
                }

                // Always set x-is-optional based on the final dataType (for non-enum fields)
                // This ensures consistency between type declaration and JSON handling
                if (!var.isEnum && var.dataType != null && var.dataType.startsWith("Option[")) {
                    var.vendorExtensions.put("x-is-optional", true);
                }
            }

            // Mark the model as needing custom JSON deserialization if any fields have custom names
            if (hasCustomJsonNames) {
                cm.vendorExtensions.put("x-has-custom-json-names", true);
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
        Matcher matcher = pattern.matcher(result);

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
        CodegenModel mdl = super.fromModel(name, schema);

        // Detect integer enums - check both the schema type and the dataType
        if (mdl.isEnum) {
            String schemaType = schema != null ? schema.getType() : null;
            if ("integer".equals(schemaType) || "int".equals(mdl.dataType) || "int64".equals(mdl.dataType)) {
                mdl.vendorExtensions.put("x-is-integer-enum", true);
            }
        }

        // Handle oneOf/anyOf schemas to use Nim object variants
        if (mdl.getComposedSchemas() != null) {
            if (mdl.getComposedSchemas().getOneOf() != null && !mdl.getComposedSchemas().getOneOf().isEmpty()) {
                mdl.vendorExtensions.put("x-is-one-of", true);
                processComposedSchemaVariants(mdl, mdl.getComposedSchemas().getOneOf(), schema);
            } else if (mdl.getComposedSchemas().getAnyOf() != null && !mdl.getComposedSchemas().getAnyOf().isEmpty()) {
                mdl.vendorExtensions.put("x-is-any-of", true);
                processComposedSchemaVariants(mdl, mdl.getComposedSchemas().getAnyOf(), schema);
            }
        }

        return mdl;
    }

    /**
     * Process oneOf/anyOf schemas to generate proper variant names for Nim object variants.
     */
    private void processComposedSchemaVariants(CodegenModel mdl, List<CodegenProperty> variants, Schema schema) {
        List<CodegenProperty> newVariants = new ArrayList<>();
        List<Schema> schemas = ModelUtils.getInterfaces(schema);

        if (variants.size() != schemas.size()) {
            LOGGER.warn("Variant size does not match schema interfaces size for model " + mdl.name);
            return;
        }

        for (int i = 0; i < variants.size(); i++) {
            CodegenProperty variant = variants.get(i);
            Schema variantSchema = schemas.get(i);

            // Create a clone to avoid modifying the original
            CodegenProperty newVariant = variant.clone();

            // Sanitize baseName to remove underscores and properly format for Nim
            if (newVariant.baseName != null) {
                // Remove trailing underscores and convert to proper format
                String sanitizedBase = newVariant.baseName.replaceAll("_+$", ""); // Remove trailing underscores
                if (sanitizedBase.length() > 0 && Character.isUpperCase(sanitizedBase.charAt(0))) {
                    newVariant.baseName = toModelName(sanitizedBase);
                } else {
                    newVariant.baseName = sanitizeNimIdentifier(sanitizedBase);
                }
            }

            // Sanitize dataType to remove underscores and properly format for Nim
            // For model types (not primitives), use toModelName to get the proper type name
            if (newVariant.dataType != null) {
                // Check if this is a model type (starts with uppercase) vs primitive
                if (newVariant.dataType.length() > 0 && Character.isUpperCase(newVariant.dataType.charAt(0))) {
                    // This is likely a model type, use toModelName to properly format it
                    newVariant.dataType = toModelName(newVariant.dataType);
                } else {
                    // Primitive type, just sanitize
                    newVariant.dataType = sanitizeNimIdentifier(newVariant.dataType);
                }
            }
            if (newVariant.datatypeWithEnum != null) {
                if (newVariant.datatypeWithEnum.length() > 0 && Character.isUpperCase(newVariant.datatypeWithEnum.charAt(0))) {
                    newVariant.datatypeWithEnum = toModelName(newVariant.datatypeWithEnum);
                } else {
                    newVariant.datatypeWithEnum = sanitizeNimIdentifier(newVariant.datatypeWithEnum);
                }
            }

            // Set variant name based on schema reference or type
            if (variantSchema.get$ref() != null && !variantSchema.get$ref().isEmpty()) {
                String refName = ModelUtils.getSimpleRef(variantSchema.get$ref());
                if (refName != null) {
                    newVariant.setName(toModelName(refName));
                    newVariant.setBaseName(refName);
                }
            } else if (variantSchema.getType() != null) {
                // For primitive types or inline schemas
                String typeName = variantSchema.getType();
                if (variantSchema.getTitle() != null && !variantSchema.getTitle().isEmpty()) {
                    typeName = variantSchema.getTitle();
                }
                newVariant.setName(camelize(typeName));
                newVariant.setBaseName(typeName);
            }

            newVariants.add(newVariant);
        }

        // Replace the original variants with the processed ones
        if (mdl.getComposedSchemas().getOneOf() != null) {
            mdl.getComposedSchemas().setOneOf(newVariants);
        } else if (mdl.getComposedSchemas().getAnyOf() != null) {
            mdl.getComposedSchemas().setAnyOf(newVariants);
        }
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
