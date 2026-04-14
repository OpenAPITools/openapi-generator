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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import io.swagger.v3.oas.models.media.Schema;

import java.io.File;
import java.util.*;

public class MotokoClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";
    public static final String USE_DFX = "useDfx";
    public static final String USE_ICP = "useIcp";

    protected String projectName = "OpenAPI";
    protected boolean useDfx = false;
    protected boolean useIcp = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "motoko";
    }

    @Override
    public String getHelp() {
        return "Generates a Motoko client (beta).";
    }

    public MotokoClientCodegen() {
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
                        ClientModificationFeature.BasePath
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "motoko";
        modelTemplateFiles.put("model.mustache", ".mo");
        apiTemplateFiles.put("api.mustache", ".mo");
        embeddedTemplateDir = templateDir = "motoko";
        apiPackage = "src/Apis";
        modelPackage = "src/Models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("mops.toml.mustache", "", "mops.toml"));
        supportingFiles.add(new SupportingFile("Config.mustache", "src", "Config.mo"));

        // Motoko language-specific primitives (don't need imports)
        // All builtin types are listed to prevent naming clashes with OpenAPI models
        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.addAll(Arrays.asList(
            "Text", "Char", "Bool",
            "Int", "Int8", "Int16", "Int32", "Int64",
            "Nat", "Nat8", "Nat16", "Nat32", "Nat64",
            "Float", "Blob", "Any", "Null", "Principal",
            "Region", "Error", "None"
        ));

        // Motoko reserved words
        // Based on Motoko language specification
        reservedWords.addAll(Arrays.asList(
            "actor", "and", "assert", "async", "await", "break", "case", "catch", "class",
            "continue", "debug", "do", "else", "false", "for", "func", "if",
            "in", "import", "module", "not", "null", "object", "or", "label",
            "let", "loop", "private", "public", "query", "return", "shared", "switch",
            "system", "throw", "true", "try", "type", "var", "while", "with"
        ));
        // Add lowercase versions of all primitives (isReservedWord() converts to lowercase)
        languageSpecificPrimitives.forEach(p -> reservedWords.add(p.toLowerCase(Locale.ROOT)));
        // Add "map" since Map<K,V> is a parameterized type that could conflict with user models
        reservedWords.add("map");

        // Motoko type mappings
        typeMapping.clear();
        typeMapping.putAll(Map.of(
            "string", "Text",
            "char", "Char",
            "boolean", "Bool",
            "int", "Int",
            "integer", "Int",
            "long", "Int",
            "float", "Float",
            "double", "Float",
            "number", "Float",
            "date", "Text"
        ));
        typeMapping.putAll(Map.of(
            "DateTime", "Text",
            "password", "Text",
            "file", "Blob",
            "binary", "Blob",
            "ByteArray", "Blob",
            "UUID", "Text",
            "URI", "Text",
            "array", "Array",  // Handled in getTypeDeclaration to produce [T] syntax
            "map", "Map",  // Maps use the red-black tree based Map from core/pure/Map
            "object", "Any"
        ));
        // Map AnyType (from additionalProperties/free-form objects) to Text as a placeholder
        // TODO: Better support for additionalProperties - see related TODO in postProcessModels
        typeMapping.put("AnyType", "Text");

        cliOptions.add(CliOption.newString(PROJECT_NAME, "Project name for generated code"));
        cliOptions.add(CliOption.newBoolean(USE_DFX, "Generate code for dfx with ic:aaaaa-aa imports", useDfx));
        cliOptions.add(CliOption.newBoolean(USE_ICP, "Generate icp.yaml for deployment with icp-cli (canary; replaces dfx)", useIcp));

        // Enable inline enum resolution to create model files for inline enum parameters
        // This ensures type-safe enum variants instead of raw Text types
        inlineSchemaOption.put("RESOLVE_INLINE_ENUMS", "true");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(PROJECT_NAME));
        }

        if (additionalProperties.containsKey(USE_DFX)) {
            setUseDfx(convertPropertyToBooleanAndWriteBack(USE_DFX));
        }
        additionalProperties.put(USE_DFX, useDfx);

        if (additionalProperties.containsKey(USE_ICP)) {
            setUseIcp(convertPropertyToBooleanAndWriteBack(USE_ICP));
        }
        additionalProperties.put(USE_ICP, useIcp);
        if (useIcp) {
            supportingFiles.add(new SupportingFile("icp.yaml.mustache", "", "icp.yaml"));
        }
    }

    @Override
    public String escapeReservedWord(String name) {
        // Escape reserved words and Motoko primitives/core types by appending underscore
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return name + "_";
    }

    @Override
    public String toModelName(String name) {
        // Apply parent sanitization first
        name = super.toModelName(name);

        // Check if model name conflicts with reserved words or Motoko primitives
        if (isReservedWord(name)) {
            return escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toModelFilename(String name) {
        // Filename should match the model name
        return toModelName(name);
    }

    @Override
    public String toVarName(String name) {
        // Sanitize name but keep it as snake_case (convert hyphens to underscores)
        name = name.replace("-", "_");

        // Handle reserved words by appending underscore
        if (isReservedWord(name)) {
            name = escapeReservedWord(name);
        }

        // Ensure valid Motoko identifier (starts with letter or underscore)
        if (!name.isEmpty() && !Character.isJavaIdentifierStart(name.charAt(0))) {
            name = "_" + name;
        }

        return name;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setUseDfx(boolean useDfx) {
        this.useDfx = useDfx;
    }

    public boolean getUseDfx() {
        return useDfx;
    }

    public void setUseIcp(boolean useIcp) {
        this.useIcp = useIcp;
    }

    public boolean getUseIcp() {
        return useIcp;
    }

    @Override
    public String toModelImport(String name) {
        // For Motoko, imports are relative to the current Models directory
        // Just return the model name without package prefix
        return name;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        // Check for custom mapping
        if (enumNameMapping.containsKey(name)) {
            return enumNameMapping.get(name);
        }

        // Handle empty string
        if (name == null || name.isEmpty()) {
            return "empty";
        }

        // For purely numeric values (Candid-style), wrap with underscores
        if (name.matches("^\\d+$")) {
            return "_" + name + "_";
        }

        // Lowercase for Motoko variant convention (idiomatic, though not required)
        String enumVarName = name.toLowerCase(Locale.ROOT);

        // Handle leading minus sign (negative numbers) before generic replacement,
        // so -1 becomes "minus_1" rather than colliding with 1 -> "_1"
        if (enumVarName.startsWith("-")) {
            enumVarName = "minus_" + enumVarName.substring(1);
        }

        // Replace remaining special characters with underscores
        // Motoko identifiers: [a-zA-Z_][a-zA-Z0-9_]*
        enumVarName = enumVarName.replaceAll("[^a-zA-Z0-9_]", "_");

        // Remove consecutive underscores
        enumVarName = enumVarName.replaceAll("_+", "_");

        // Remove leading/trailing underscores
        enumVarName = enumVarName.replaceAll("^_+|_+$", "");

        // If name starts with a number (but not purely numeric), prefix with underscore
        if (enumVarName.matches("^\\d.*")) {
            enumVarName = "_" + enumVarName;
        }

        // Fallback for invalid names after sanitization
        if (enumVarName.isEmpty()) {
            enumVarName = "value_" + Math.abs(name.hashCode());
        }

        // Escape reserved words
        if (isReservedWord(enumVarName)) {
            return escapeReservedWord(enumVarName);
        }

        return enumVarName;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        // Check for custom mapping
        if (enumNameMapping.containsKey(property.name)) {
            return enumNameMapping.get(property.name);
        }

        // Use the property's base name to create enum type name
        String enumName = toModelName(property.baseName);

        // Avoid collision with property variable name by checking if they would be identical
        // For example, if property is "status", enum type should be "Status" not "status"
        if (enumName.equals(property.name)) {
            enumName = enumName + "Enum";
        }

        // Check for reserved word collision
        if (isReservedWord(enumName)) {
            enumName = escapeReservedWord(enumName);
        }

        return enumName;
    }

    @Override
    public String getTypeDeclaration(io.swagger.v3.oas.models.media.Schema schema) {
        // Handle array types: convert to Motoko syntax [ElementType]
        String result;
        if (ModelUtils.isArraySchema(schema)) {
            io.swagger.v3.oas.models.media.Schema inner = ModelUtils.getSchemaItems(schema);
            result = "[" + getTypeDeclaration(inner) + "]";

            return result;
        } else if (ModelUtils.isMapSchema(schema)) {
            // Handle map types: convert to Motoko Map syntax Map<Text, ValueType>
            io.swagger.v3.oas.models.media.Schema inner = ModelUtils.getAdditionalProperties(schema);
            result = "Map<Text, " + getTypeDeclaration(inner) + ">";

            return result;
        }

        return super.getTypeDeclaration(schema);
    }

    @Override
    public String getSchemaType(io.swagger.v3.oas.models.media.Schema schema) {
        // Handle array types first, before calling super.getSchemaType()
        // This is critical because super.getSchemaType() returns "array" without the element type
        if (ModelUtils.isArraySchema(schema)) {
            String inner = getSchemaType(ModelUtils.getSchemaItems(schema));
            return "[" + inner + "]";
        } else if (ModelUtils.isMapSchema(schema)) {
            io.swagger.v3.oas.models.media.Schema inner = ModelUtils.getAdditionalProperties(schema);
            return "Map<Text, " + getSchemaType(inner) + ">";
        }

        String openAPIType = super.getSchemaType(schema);
        String type;
        // Check if we have a type mapping for this OpenAPI type
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            // If it's a language-specific primitive, return it directly
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        // Otherwise, convert to model name
        return toModelName(type);
    }

    @Override
    public String toInstantiationType(io.swagger.v3.oas.models.media.Schema schema) {
        if (ModelUtils.isArraySchema(schema)) {
            String inner = getSchemaType(ModelUtils.getSchemaItems(schema));
            return "[" + inner + "]";
        } else if (ModelUtils.isMapSchema(schema)) {
            io.swagger.v3.oas.models.media.Schema inner = ModelUtils.getAdditionalProperties(schema);
            return "Map<Text, " + getSchemaType(inner) + ">";
        }
        return null;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        // Fix dataType for arrays and maps that may have slipped through as bare types
        // This happens when the dataType is set before our getSchemaType is called
        if ("array".equals(parameter.dataType)) {
            // Try to reconstruct the array type from the parameter
            if (parameter.isArray && parameter.items != null) {
                // items is a CodegenProperty, not CodegenParameter - just use its dataType
                String old = parameter.dataType;
                parameter.dataType = "[" + parameter.items.dataType + "]";
            }
        }

        // Check if this is an integer parameter with minimum >= 0 constraint
        // Convert to Nat type for type safety (unsigned integers)
        if (parameter.dataType != null) {
            String dataType = parameter.dataType;
            boolean isIntType = "Int".equals(dataType) || dataType.matches("Int\\d+");

            if (isIntType && parameter.minimum != null) {
                try {
                    java.math.BigDecimal minimum = new java.math.BigDecimal(parameter.minimum);
                    if (minimum.compareTo(java.math.BigDecimal.ZERO) >= 0) {
                        // Convert Int to Nat (unsigned)
                        parameter.dataType = "Nat";
                        parameter.vendorExtensions.put("x-is-unsigned", true);
                        parameter.vendorExtensions.put("x-original-type", dataType);
                    }
                } catch (NumberFormatException e) {
                    // Ignore invalid minimum values
                }
            }
        }

        // For enum parameters, ensure enumVars are built for template use
        if (Boolean.TRUE.equals(parameter.isEnum) && parameter.allowableValues != null) {
            @SuppressWarnings("unchecked")
            List<Object> values = (List<Object>) parameter.allowableValues.get("values");

            if (values != null && !values.isEmpty()) {
                List<Map<String, Object>> enumVars = new ArrayList<>();
                for (int i = 0; i < values.size(); i++) {
                    Object value = values.get(i);
                    Map<String, Object> enumVar = new HashMap<>();

                    // Get the variant name using toEnumVarName
                    String variantName = toEnumVarName(String.valueOf(value), parameter.dataType);

                    enumVar.put("name", variantName);
                    enumVar.put("value", String.valueOf(value));
                    enumVar.put("isString", value instanceof String);

                    // Mark the last item
                    if (i == values.size() - 1) {
                        enumVar.put("-last", true);
                    }

                    enumVars.add(enumVar);
                }
                parameter.allowableValues.put("enumVars", enumVars);
            }
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        CodegenModel model = super.fromModel(name, schema);

        // For standalone enum schemas, ensure enumVars are built
        if (Boolean.TRUE.equals(model.isEnum) && model.allowableValues != null) {
            @SuppressWarnings("unchecked")
            List<Object> values = (List<Object>) model.allowableValues.get("values");

            if (values != null && !values.isEmpty()) {
                List<Map<String, Object>> enumVars = new ArrayList<>();
                for (int i = 0; i < values.size(); i++) {
                    Object value = values.get(i);
                    Map<String, Object> enumVar = new HashMap<>();

                    // Get the variant name using toEnumVarName
                    String variantName = toEnumVarName(String.valueOf(value), model.dataType);

                    enumVar.put("name", variantName);
                    enumVar.put("value", String.valueOf(value));
                    enumVar.put("isString", value instanceof String);

                    // Mark the last item
                    if (i == values.size() - 1) {
                        enumVar.put("-last", true);
                    }

                    enumVars.add(enumVar);
                }
                model.allowableValues.put("enumVars", enumVars);
            }
        }

        // Handle oneOf schemas - generate as discriminated unions (variant types)
        if (model.getComposedSchemas() != null && model.getComposedSchemas().getOneOf() != null) {
            List<CodegenProperty> oneOfList = model.getComposedSchemas().getOneOf();

            if (!oneOfList.isEmpty()) {
                model.vendorExtensions.put("x-is-oneof", true);

                // Build variant cases for oneOf options
                List<Map<String, Object>> oneOfVariants = new ArrayList<>();
                boolean hasUnsignedVariants = false;

                for (int i = 0; i < oneOfList.size(); i++) {
                    CodegenProperty oneOfProp = oneOfList.get(i);

                    // Check if this is an inline enum - expand it into multiple unit variants
                    if (Boolean.TRUE.equals(oneOfProp.isEnum) && oneOfProp.allowableValues != null) {
                        @SuppressWarnings("unchecked")
                        List<Object> enumValues = (List<Object>) oneOfProp.allowableValues.get("values");

                        if (enumValues != null && !enumValues.isEmpty()) {
                            // Expand inline enum into multiple unit variants
                            for (Object enumValue : enumValues) {
                                Map<String, Object> variant = new HashMap<>();
                                String variantName = toEnumVarName(String.valueOf(enumValue), oneOfProp.dataType);

                                variant.put("name", variantName);
                                variant.put("hasType", false);  // Unit variant (no associated data)
                                variant.put("isUnsigned", false);
                                variant.put("needsConversion", false);
                                variant.put("enumValue", String.valueOf(enumValue));
                                variant.put("isStringEnum", enumValue instanceof String);

                                // Type classification flags (not used for unit variants, but added for consistency)
                                variant.put("isNumericType", false);
                                variant.put("isEnumType", false);
                                variant.put("isObjectType", false);

                                oneOfVariants.add(variant);
                            }
                            continue;  // Skip regular processing for this enum option
                        }
                    }

                    // Regular oneOf option (not an inline enum)
                    Map<String, Object> variant = new HashMap<>();

                    // Determine variant name and type
                    String variantName = getOneOfVariantName(oneOfProp);
                    String variantType = oneOfProp.dataType;
                    String jsonType = variantType;  // JSON-facing type (may differ for Nat->Int)

                    // For integer types with minimum >= 0, convert to Nat in user-facing type
                    boolean isUnsigned = Boolean.TRUE.equals(oneOfProp.vendorExtensions.get("x-is-unsigned"));
                    if (isUnsigned || ("Nat".equals(variantType))) {
                        // User-facing uses Nat, JSON-facing uses Int
                        variantType = "Nat";
                        jsonType = "Int";
                        isUnsigned = true;
                        hasUnsignedVariants = true;
                    }

                    variant.put("name", variantName);
                    variant.put("dataType", variantType);
                    variant.put("jsonType", jsonType);
                    variant.put("hasType", true);  // Typed variant (has associated data)
                    variant.put("isUnsigned", isUnsigned);
                    variant.put("needsConversion", !variantType.equals(jsonType));

                    // Add type classification flags for toText() generation
                    boolean isNumericType = false;
                    boolean isEnumType = false;
                    boolean isObjectType = false;

                    if (isUnsigned || "Nat".equals(variantType) || "Int".equals(variantType)) {
                        // Numeric types: Int, Nat
                        isNumericType = true;
                    } else if (Boolean.TRUE.equals(oneOfProp.isEnum) ||
                               (variantType != null && variantType.endsWith("Enum")) ||
                               Boolean.TRUE.equals(oneOfProp.vendorExtensions.get("x-is-enum")) ||
                               (oneOfProp.allowableValues != null && !oneOfProp.allowableValues.isEmpty())) {
                        // Enum types: detected by isEnum flag, "Enum" suffix, vendor extension, or allowableValues
                        isEnumType = true;
                    } else if (oneOfProp.complexType != null || Boolean.TRUE.equals(oneOfProp.isModel)) {
                        // Complex object/record types
                        isObjectType = true;
                    } else {
                        // Default to object type for unknown cases
                        isObjectType = true;
                    }

                    variant.put("isNumericType", isNumericType);
                    variant.put("isEnumType", isEnumType);
                    variant.put("isObjectType", isObjectType);

                    oneOfVariants.add(variant);
                }

                // Mark the last variant
                if (!oneOfVariants.isEmpty()) {
                    oneOfVariants.get(oneOfVariants.size() - 1).put("-last", true);
                }

                model.vendorExtensions.put("oneOfVariants", oneOfVariants);

                // Set flag if any variant needs Int import for unsigned conversion
                if (hasUnsignedVariants) {
                    model.vendorExtensions.put("x-has-unsigned-fields", true);
                }
            }
        }

        return model;
    }

    /**
     * Generate a variant name for a oneOf option.
     * For simple types, use the type name. For enum strings, use the enum value.
     */
    private String getOneOfVariantName(CodegenProperty prop) {
        // For enum types, use the first enum value or a sanitized name
        if (Boolean.TRUE.equals(prop.isEnum) && prop.allowableValues != null) {
            @SuppressWarnings("unchecked")
            List<Object> values = (List<Object>) prop.allowableValues.get("values");
            if (values != null && !values.isEmpty()) {
                // Use the enum values directly as variant names
                // This handles string enums like ["up", "down"]
                return toEnumVarName(String.valueOf(values.get(0)), prop.dataType);
            }
        }

        // For reference types, use the referenced type name
        if (prop.complexType != null) {
            return toVarName(prop.complexType);
        }

        // For primitive types, use the base type name
        String baseName = prop.baseName != null ? prop.baseName : prop.dataType;
        if (baseName != null) {
            return toVarName(baseName);
        }

        // Default to "integer", "string", etc. for simple types
        return toVarName(prop.dataType);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema propertySchema, boolean required, boolean schemaIsFromAdditionalProperties) {
        CodegenProperty property = super.fromProperty(name, propertySchema, required, schemaIsFromAdditionalProperties);

        // Check if this is an integer type with minimum >= 0 constraint
        // Convert to Nat type for type safety (unsigned integers)
        if (property != null && property.dataType != null) {
            String dataType = property.dataType;

            // Check if it's an Int type (including parameterized types in arrays/maps)
            boolean isIntType = "Int".equals(dataType) || dataType.matches("Int\\d+");

            if (isIntType && propertySchema != null) {
                // Check minimum constraint
                java.math.BigDecimal minimum = propertySchema.getMinimum();

                if (minimum != null && minimum.compareTo(java.math.BigDecimal.ZERO) >= 0) {
                    // Convert Int to Nat (unsigned)
                    // Note: This creates Janus-like behavior where JSON has Int but Motoko uses Nat
                    property.dataType = "Nat";

                    // Add vendor extension to signal this conversion in templates
                    property.vendorExtensions.put("x-is-unsigned", true);
                    property.vendorExtensions.put("x-original-type", dataType);
                }
            }
        }

        return property;
    }

    @Override
    public ModelsMap postProcessModelsEnum(ModelsMap objs) {
        // Call parent to process enums with default logic
        objs = super.postProcessModelsEnum(objs);

        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // Call parent first (which calls postProcessModelsEnum internally)
        objs = super.postProcessModels(objs);

        // Track enum models to add to imports
        Set<String> enumModelNames = new HashSet<>();

        // Check if we need to import Map
        boolean needsMapImport = false;

        // Collect field mappings for JSON serialization
        Map<String, List<Map<String, String>>> fieldMappings = new HashMap<>();

        // Check all model properties for Map usage and enum references
        List<ModelMap> models = objs.getModels();

        // FIRST PASS: Collect all enum model names
        if (models != null) {
            for (ModelMap modelMap : models) {
                org.openapitools.codegen.CodegenModel model = modelMap.getModel();
                if (model != null && Boolean.TRUE.equals(model.isEnum)) {
                    enumModelNames.add(model.classname);
                }
            }
        }

        // SECOND PASS: Process all models with full enum knowledge
        if (models != null) {
            for (ModelMap modelMap : models) {
                org.openapitools.codegen.CodegenModel model = modelMap.getModel();

                if (model != null) {
                    // Mark enum models for conditional template logic
                    if (Boolean.TRUE.equals(model.isEnum)) {
                        model.vendorExtensions.put("x-is-motoko-enum", true);
                    }

                    // Track if this model has any enum fields (needs JSON sub-module)
                    boolean hasEnumFields = false;
                    // Track if this model has any unsigned (Nat) fields
                    boolean hasUnsignedFields = false;

                    if (model.vars != null) {
                        // Collect field name mappings
                        List<Map<String, String>> fieldEscapeMappings = new ArrayList<>();

                        for (org.openapitools.codegen.CodegenProperty prop : model.vars) {
                            // Check for Map usage
                            if (prop.dataType != null && prop.dataType.contains("Map<")) {
                                needsMapImport = true;
                            }

                            // Check for unsigned fields (Nat types converted from Int)
                            if (Boolean.TRUE.equals(prop.vendorExtensions.get("x-is-unsigned"))) {
                                hasUnsignedFields = true;
                            }

                            // Collect escaped field names (where Motoko name differs from JSON name)
                            if (!prop.baseName.equals(prop.name)) {
                                Map<String, String> mapping = new HashMap<>();
                                mapping.put("motokoName", prop.name);
                                mapping.put("jsonName", prop.baseName);
                                fieldEscapeMappings.add(mapping);
                            }

                            // Handle enum properties
                            if (Boolean.TRUE.equals(prop.isEnum)) {
                                // TODO: Inline enums need proper implementation to generate separate model files
                                // For now, keep them as Text type
                                // This is an inline enum - would need to generate a separate model for it
                                // String enumTypeName = toEnumName(prop);
                                // prop.datatypeWithEnum = enumTypeName;
                                // prop.dataType = enumTypeName;
                                // enumModelNames.add(enumTypeName);
                            } else if (Boolean.TRUE.equals(prop.isEnumRef)) {
                                // This is a reference to an existing enum
                                // The datatypeWithEnum should already be set by DefaultCodegen
                                if (prop.datatypeWithEnum != null) {
                                    prop.vendorExtensions.put("x-is-motoko-enum", true);
                                    prop.vendorExtensions.put("xIsMotokoEnum", true);
                                    prop.vendorExtensions.put("x-motoko-enum-type", prop.datatypeWithEnum);
                                    prop.vendorExtensions.put("xMotokoEnumType", prop.datatypeWithEnum);
                                    enumModelNames.add(prop.datatypeWithEnum);
                                    hasEnumFields = true;
                                }
                            } else if (enumModelNames.contains(prop.dataType)) {
                                // Property's dataType matches an enum model name
                                // This catches cases where isEnumRef might not be set correctly
                                prop.vendorExtensions.put("x-is-motoko-enum", true);
                                prop.vendorExtensions.put("xIsMotokoEnum", true);
                                prop.vendorExtensions.put("x-motoko-enum-type", prop.dataType);
                                prop.vendorExtensions.put("xMotokoEnumType", prop.dataType);
                                hasEnumFields = true;
                            }
                        }

                        // Store field mappings if any
                        if (!fieldEscapeMappings.isEmpty()) {
                            fieldMappings.put(model.classname, fieldEscapeMappings);
                            model.vendorExtensions.put("x-has-field-mappings", true);
                            model.vendorExtensions.put("x-field-mappings", fieldEscapeMappings);
                        }
                    }

                    // Mark models that need JSON sub-modules (have enum fields)
                    if (hasEnumFields) {
                        model.vendorExtensions.put("x-needs-json-module", true);
                        model.vendorExtensions.put("xNeedsJsonModule", true);
                    }

                    // Mark models that have unsigned (Nat) fields (need Int import for conversion)
                    if (hasUnsignedFields) {
                        model.vendorExtensions.put("x-has-unsigned-fields", true);
                    }
                }
            }
        }

        // Store field mappings in context for templates
        objs.put("fieldMappings", fieldMappings);
        objs.put("hasAnyMappings", !fieldMappings.isEmpty());

        // Mark imports that are mapped types (primitives) or array/map types so they can be filtered out
        List<Map<String, String>> imports = objs.getImports();
        if (imports != null) {
            for (Map<String, String> im : imports) {
                String importName = im.get("import");
                // Check if this import is a primitive/mapped type or array/map type
                if (importName != null) {
                    // TODO: Support additionalProperties by modeling as Map<Text, Text> or similar
                    //   Currently schemas with additionalProperties: true are not fully supported.
                    //   Future work: Add a field like "additionalProperties: ?Map<Text, Text>" to models.
                    boolean isMappedType = typeMapping.containsKey(importName) ||
                                            typeMapping.containsValue(importName) ||
                                            languageSpecificPrimitives.contains(importName) ||
                                            importName.startsWith("[") ||
                                            importName.contains("<") ||     // Filter out parameterized types like "Map<Text, Int>"
                                            "AnyType".equals(importName);   // Filter out AnyType - not yet implemented
                    if (isMappedType) {
                        im.put("isMappedType", "true");
                    }

                    // Mark enum imports
                    if (enumModelNames.contains(importName)) {
                        im.put("isEnum", "true");
                    }
                }
            }
        }

        // Add Map import if needed
        // NOTE: Model name escaping is implemented in toModelName() and tested in
        //       samples/client/type-coverage/motoko-test with a user-defined "Map" model.
        //       User model "Map" is escaped to "Map_" while Map<K,V> refers to the core type.
        if (needsMapImport) {
            if (imports == null) {
                imports = new ArrayList<>();
                objs.put("imports", imports);
            }
            imports.add(Map.of(
                "import", "Map",
                "isMap", "true",
                "isMappedType", "true"  // Prevent it from being imported as a model
            ));
        }

        return objs;
    }

    /**
     * Check if a type is a primitive or mapped type (not a generated model).
     * Uses languageSpecificPrimitives and typeMapping as the single source of truth.
     */
    private boolean isPrimitiveOrMappedType(String type) {
        if (type == null) {
            return false;
        }
        // Check if it's a language-specific primitive
        if (languageSpecificPrimitives.contains(type)) {
            return true;
        }
        // Check if it's in typeMapping (like Any for object)
        if (typeMapping.containsValue(type)) {
            return true;
        }
        return false;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        // Call parent first
        objs = super.postProcessAllModels(objs);

        // Collect all models from all ModelsMap objects
        List<CodegenModel> allModels = new ArrayList<>();
        for (ModelsMap modelsMap : objs.values()) {
            List<ModelMap> modelMaps = modelsMap.getModels();
            if (modelMaps != null) {
                for (ModelMap modelMap : modelMaps) {
                    CodegenModel model = modelMap.getModel();
                    if (model != null) {
                        allModels.add(model);
                    }
                }
            }
        }

        // THIRD PASS: Detect transitive enum references (models containing fields that reference models with enums)
        // Iterate until we reach a fixed point (no new models marked as having enum fields)
        boolean changed = true;
        while (changed) {
            changed = false;
            for (CodegenModel model : allModels) {
                // Skip if already marked as having enum fields or if it's an enum itself
                if (!Boolean.TRUE.equals(model.vendorExtensions.get("xNeedsJsonModule"))
                    && !Boolean.TRUE.equals(model.isEnum)) {

                    boolean hasTransitiveEnums = false;

                    if (model.vars != null) {
                        for (CodegenProperty prop : model.vars) {
                            // Check if this field's type is a model that has enum fields
                            if (prop.dataType != null && !prop.isEnum && !prop.isEnumRef) {
                                // Look for the referenced model
                                for (CodegenModel refModel : allModels) {
                                    if (refModel.classname.equals(prop.dataType)
                                        && Boolean.TRUE.equals(refModel.vendorExtensions.get("xNeedsJsonModule"))) {
                                        hasTransitiveEnums = true;
                                        break;
                                    }
                                }
                            }
                            if (hasTransitiveEnums) break;
                        }
                    }

                    if (hasTransitiveEnums) {
                        model.vendorExtensions.put("x-needs-json-module", true);
                        model.vendorExtensions.put("xNeedsJsonModule", true);
                        changed = true;
                    }
                }
            }
        }

        // FOURTH PASS: Mark models that have no enum fields (neither direct nor transitive) and no unsigned fields for identity optimization
        for (CodegenModel model : allModels) {
            if (!Boolean.TRUE.equals(model.isEnum)
                && !Boolean.TRUE.equals(model.vendorExtensions.get("xNeedsJsonModule"))
                && !Boolean.TRUE.equals(model.vendorExtensions.get("x-has-unsigned-fields"))) {
                // Model has no enum fields and no unsigned fields - can use identity transform
                model.vendorExtensions.put("x-no-enum-fields", true);
                model.vendorExtensions.put("xNoEnumFields", true);
            }
        }

        // FIFTH PASS: Mark properties that reference models needing JSON conversion
        for (CodegenModel model : allModels) {
            if (model.vars != null) {
                for (CodegenProperty prop : model.vars) {
                    // Check if this property references a model that needs JSON conversion
                    if (prop.dataType != null && !prop.isEnum && !prop.isEnumRef) {
                        for (CodegenModel refModel : allModels) {
                            if (refModel.classname.equals(prop.dataType)
                                && Boolean.TRUE.equals(refModel.vendorExtensions.get("xNeedsJsonModule"))) {
                                // This property references a model that needs JSON conversion
                                prop.vendorExtensions.put("x-needs-json-conversion", true);
                                prop.vendorExtensions.put("xNeedsJsonConversion", true);
                                break;
                            }
                        }
                    }
                }
            }
        }

        // SIXTH PASS: Count transformed fields for record-update optimization
        for (CodegenModel model : allModels) {
            // Skip enum models - they don't have record fields
            if (Boolean.TRUE.equals(model.isEnum)) {
                continue;
            }

            if (model.vars != null && !model.vars.isEmpty()) {
                int totalFields = model.vars.size();
                int transformedFields = 0;

                // Count fields that need transformation
                for (CodegenProperty prop : model.vars) {
                    // A field needs transformation if it's an enum reference or needs JSON conversion
                    // Check both isEnumRef and the vendor extension we set
                    boolean needsTransform = Boolean.TRUE.equals(prop.isEnumRef) ||
                                            Boolean.TRUE.equals(prop.vendorExtensions.get("xIsMotokoEnum")) ||
                                            Boolean.TRUE.equals(prop.vendorExtensions.get("xNeedsJsonConversion"));
                    if (needsTransform) {
                        transformedFields++;
                    }
                }

                // Store the counts
                model.vendorExtensions.put("x-total-fields", totalFields);
                model.vendorExtensions.put("x-transform-count", transformedFields);

                // Determine optimization strategy:
                // - If transformedFields == 0: use identity (already handled by xNoEnumFields)
                // - If 0 < transformedFields < totalFields: use record-update syntax (with)
                // - If transformedFields == totalFields: construct fresh record
                if (transformedFields > 0 && transformedFields < totalFields) {
                    model.vendorExtensions.put("x-has-partial-transform", true);
                    model.vendorExtensions.put("xHasPartialTransform", true);
                }
            }
        }

        return objs;
    }

    /**
     * Collect enum variant mappings for JSON serialization.
     * Returns a list of mappings where Motoko variant name differs from OpenAPI value.
     */
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap result = super.postProcessOperationsWithModels(objs, allModels);

        // Parse security schemes from OpenAPI spec to enable authentication support
        Map<String, io.swagger.v3.oas.models.security.SecurityScheme> securitySchemes = new HashMap<>();
        if (openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getSecuritySchemes() != null) {
            securitySchemes = openAPI.getComponents().getSecuritySchemes();
        }

        // Determine which authentication types are used
        boolean usesBearerAuth = false;
        boolean usesApiKeyAuth = false;
        boolean usesBasicAuth = false;
        String apiKeyHeaderName = null;
        String apiKeyQueryName = null;

        for (Map.Entry<String, io.swagger.v3.oas.models.security.SecurityScheme> entry : securitySchemes.entrySet()) {
            io.swagger.v3.oas.models.security.SecurityScheme scheme = entry.getValue();

            if (io.swagger.v3.oas.models.security.SecurityScheme.Type.HTTP.equals(scheme.getType())) {
                if ("bearer".equalsIgnoreCase(scheme.getScheme())) {
                    usesBearerAuth = true;
                } else if ("basic".equalsIgnoreCase(scheme.getScheme())) {
                    usesBasicAuth = true;
                }
            } else if (io.swagger.v3.oas.models.security.SecurityScheme.Type.APIKEY.equals(scheme.getType())) {
                usesApiKeyAuth = true;
                if (io.swagger.v3.oas.models.security.SecurityScheme.In.HEADER.equals(scheme.getIn())) {
                    apiKeyHeaderName = scheme.getName();
                } else if (io.swagger.v3.oas.models.security.SecurityScheme.In.QUERY.equals(scheme.getIn())) {
                    apiKeyQueryName = scheme.getName();
                }
            }
        }

        // Add authentication context to operations for use in templates
        result.put("usesBearerAuth", usesBearerAuth);
        result.put("usesApiKeyAuth", usesApiKeyAuth);
        result.put("usesBasicAuth", usesBasicAuth);
        result.put("apiKeyHeaderName", apiKeyHeaderName);
        result.put("apiKeyQueryName", apiKeyQueryName);

        // Response code processing is implemented in api.mustache template
        // - HTTP status codes are checked before parsing (2xx vs 4xx/5xx)
        // - Error responses use generated error models when available
        // - Structured error details are included in thrown errors

        // Collect all enum types and models with escaped fields for global API context
        List<Map<String, Object>> allEnumTypes = new ArrayList<>();
        List<Map<String, Object>> allModelsWithEscapedFields = new ArrayList<>();

        if (allModels != null) {
            for (ModelMap modelMap : allModels) {
                CodegenModel model = modelMap.getModel();

                if (model != null) {
                    // Collect enum types with mappings
                    if (Boolean.TRUE.equals(model.isEnum) &&
                        Boolean.TRUE.equals(model.vendorExtensions.get("x-has-enum-mappings"))) {
                        Map<String, Object> enumInfo = new HashMap<>();
                        enumInfo.put("name", model.classname);
                        enumInfo.put("mappings", model.vendorExtensions.get("x-enum-mappings"));
                        allEnumTypes.add(enumInfo);
                    }

                    // Collect models with escaped field names
                    if (Boolean.TRUE.equals(model.vendorExtensions.get("x-has-field-mappings"))) {
                        Map<String, Object> modelInfo = new HashMap<>();
                        modelInfo.put("name", model.classname);
                        modelInfo.put("mappings", model.vendorExtensions.get("x-field-mappings"));
                        allModelsWithEscapedFields.add(modelInfo);
                    }
                }
            }
        }

        // Mark first/last items for template iteration (Mustache uses these for comma handling)
        // Clear any existing flags first
        for (Map<String, Object> enumInfo : allEnumTypes) {
            enumInfo.remove("-last");
            enumInfo.remove("-first");
        }
        for (Map<String, Object> modelInfo : allModelsWithEscapedFields) {
            modelInfo.remove("-last");
            modelInfo.remove("-first");
        }
        // Set flags on boundary items
        if (!allEnumTypes.isEmpty()) {
            allEnumTypes.get(allEnumTypes.size() - 1).put("-last", true);
        }
        if (!allModelsWithEscapedFields.isEmpty()) {
            allModelsWithEscapedFields.get(0).put("-first", true);
            allModelsWithEscapedFields.get(allModelsWithEscapedFields.size() - 1).put("-last", true);
        }

        // Store in context for API template
        result.put("allEnumTypes", allEnumTypes);
        result.put("allModelsWithEscapedFields", allModelsWithEscapedFields);
        result.put("hasAnyMappings", !allEnumTypes.isEmpty() || !allModelsWithEscapedFields.isEmpty());

        // Also add to additionalProperties for supporting files (EnumMappings.mustache)
        additionalProperties.put("allEnumTypes", allEnumTypes);
        additionalProperties.put("allModelsWithEscapedFields", allModelsWithEscapedFields);
        additionalProperties.put("hasAnyMappings", !allEnumTypes.isEmpty() || !allModelsWithEscapedFields.isEmpty());

        // Check if we need to import Map
        boolean needsMapImport = false;

        // Fix array types in operations
        org.openapitools.codegen.model.OperationMap operations = result.getOperations();
        if (operations != null) {
            for (org.openapitools.codegen.CodegenOperation op : operations.getOperation()) {
                // Fix return type if it's a bare "array"
                if ("array".equals(op.returnType)) {
                    if (op.returnContainer != null && op.returnContainer.equals("array")) {
                        op.returnType = "[" + op.returnBaseType + "]";
                    }
                }

                // Mark operations with array return types for special handling in template
                if (op.returnContainer != null && op.returnContainer.equals("array")) {
                    op.vendorExtensions.put("x-return-is-array", true);
                    op.vendorExtensions.put("x-return-base-type", op.returnBaseType);

                    // Check if array element type is a primitive/mapped type
                    boolean isElementPrimitive = isPrimitiveOrMappedType(op.returnBaseType);
                    op.vendorExtensions.put("x-return-array-element-is-primitive", isElementPrimitive);
                }

                // Mark operations with primitive/mapped return types (non-model types)
                if (op.returnType != null && op.returnContainer == null) {
                    boolean isPrimitive = isPrimitiveOrMappedType(op.returnType);
                    op.vendorExtensions.put("x-return-is-primitive", isPrimitive);
                }

                // Mark operations with map return types for special handling in template
                if (op.returnContainer != null && op.returnContainer.equals("map")) {
                    op.vendorExtensions.put("x-return-is-map", true);
                    if (op.returnBaseType != null) {
                        op.vendorExtensions.put("x-return-map-value-type", op.returnBaseType);
                    }
                    needsMapImport = true;
                }

                // Check if return type uses Map
                if (op.returnType != null && op.returnType.contains("Map<")) {
                    needsMapImport = true;
                }

                // Check body parameter for special handling
                if (op.bodyParam != null) {
                    org.openapitools.codegen.CodegenParameter bodyParam = op.bodyParam;

                    // Handle array body parameters
                    if (bodyParam.isArray && bodyParam.items != null) {
                        bodyParam.vendorExtensions.put("x-body-is-array", true);
                        bodyParam.vendorExtensions.put("x-body-base-type", bodyParam.items.dataType);

                        // Check if array element type is primitive
                        boolean isElementPrimitive = isPrimitiveOrMappedType(bodyParam.items.dataType);
                        bodyParam.vendorExtensions.put("x-body-array-element-is-primitive", isElementPrimitive);
                    } else if (bodyParam.dataType != null) {
                        // Handle primitive body parameters
                        boolean isPrimitive = isPrimitiveOrMappedType(bodyParam.dataType);
                        bodyParam.vendorExtensions.put("x-body-is-primitive", isPrimitive);
                    }
                }

                // Check if any parameters use Map
                if (op.allParams != null) {
                    for (org.openapitools.codegen.CodegenParameter param : op.allParams) {
                        if (param.dataType != null && param.dataType.contains("Map<")) {
                            needsMapImport = true;
                            break;
                        }
                    }
                }

                // Mark oneOf parameters with x-is-oneof-type vendor extension
                // This tells api.mustache to use .toText() instead of .toJSON() for URL parameters
                // We need to mark parameters in allParams, queryParams, and pathParams lists
                if (allModels != null) {
                    List<List<org.openapitools.codegen.CodegenParameter>> paramLists = new ArrayList<>();
                    if (op.allParams != null) paramLists.add(op.allParams);
                    if (op.queryParams != null) paramLists.add(op.queryParams);
                    if (op.pathParams != null) paramLists.add(op.pathParams);

                    for (List<org.openapitools.codegen.CodegenParameter> paramList : paramLists) {
                        for (org.openapitools.codegen.CodegenParameter param : paramList) {
                            if (param.dataType != null) {
                                // Check if this parameter's type is a oneOf model
                                for (ModelMap modelMap : allModels) {
                                    CodegenModel model = modelMap.getModel();
                                    if (model != null && model.classname.equals(param.dataType)) {
                                        if (Boolean.TRUE.equals(model.vendorExtensions.get("x-is-oneof"))) {
                                            param.vendorExtensions.put("x-is-oneof-type", true);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Mark imports that are mapped types (primitives) or array/map types so they can be filtered out
        List<Map<String, String>> imports = result.getImports();
        if (imports != null) {
            for (Map<String, String> im : imports) {
                // Get the classname field - this is what we use in the template
                String className = im.get("classname");
                // Check if this classname is a key in typeMapping (meaning it's a primitive/mapped type)
                // OR if it starts with '[' (array/map type) which shouldn't be imported
                if (className != null) {
                    boolean isMappedType = typeMapping.containsKey(className) ||
                                            className.startsWith("[") ||
                                            className.contains("<") ||     // Filter out parameterized types like "Map<Text, Int>"
                                            "AnyType".equals(className);   // Filter out AnyType - not yet implemented
                    // In Mustache, only add the key if it's true (for conditional sections)
                    if (isMappedType) {
                        im.put("isMappedType", "true");
                    }
                }
            }
        }

        // Add Map import if needed
        // NOTE: Model name escaping is implemented in toModelName() and tested in
        //       samples/client/type-coverage/motoko-test with a user-defined "Map" model.
        //       User model "Map" is escaped to "Map_" while Map<K,V> refers to the core type.
        if (needsMapImport) {
            if (imports == null) {
                imports = new ArrayList<>();
                result.put("imports", imports);
            }
            imports.add(Map.of(
                "import", "Map",
                "classname", "Map",
                "isMap", "true",
                "isMappedType", "true"  // Prevent it from being imported as a model
            ));
        }

        return result;
    }
}
