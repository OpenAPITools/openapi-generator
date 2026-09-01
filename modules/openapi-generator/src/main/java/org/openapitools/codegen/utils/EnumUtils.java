package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.model.EnumVarMap;

import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.X_ENUM_DEPRECATED;
import static org.openapitools.codegen.CodegenConstants.X_ENUM_DESCRIPTIONS;
import static org.openapitools.codegen.model.EnumVarMap.*;

public class EnumUtils {

    public static final String BUNGIE_X_ENUM_VALUES = "x-enum-values";
    private static final String BUNGIE_ENUM_DESCRIPTION = "description";
    private static final String BUNGIE_ENUM_IDENTIFIER = "identifier";
    private static final String BUNGIE_ENUM_NUMERIC_VALUE = "numericValue";

    public static final String ONE_OF = "oneOf";
    public static final String ANY_OF = "anyOf";

    /**
     *
     * @param allowableValues The allowableValues map
     * @return whether the allowableValues map contains {@value EnumVarMap#ENUM_VARS}
     */
    public static boolean hasEnumVars(Map<String, Object> allowableValues) {
        return allowableValues != null && allowableValues.containsKey(ENUM_VARS);
    }

    /**
     *
     * @param allowableValues The allowableValues map
     * @return whether the allowableValues map contains {@value EnumVarMap#ENUM_VALUES}
     */
    public static boolean hasEnumValues(Map<String, Object> allowableValues) {
        return allowableValues != null && allowableValues.containsKey(ENUM_VALUES);
    }

    /**
     * Get the {@value EnumVarMap#ENUM_VARS} from the allowableValues map.
     *
     * @param allowableValues The allowableValues map
     * @return the list of enumVars
     */
    public static List<EnumVarMap> getEnumVars(Map<String, Object> allowableValues) {
        @SuppressWarnings("unchecked")
        List<EnumVarMap> enumVars = (List<EnumVarMap>) allowableValues.get(ENUM_VARS);
        return enumVars;
    }

    /**
     * Get the {@value EnumVarMap#ENUM_VALUES} from the allowableValues map.
     * @param allowableValues The allowableValues map
     * @return the list of enumValues
     */
    public static List<Object> getEnumValues(Map<String, Object> allowableValues) {
        @SuppressWarnings("unchecked")
        List<Object> enumValues = (List<Object>) allowableValues.get(ENUM_VALUES);
        return enumValues;
    }

    /**
     * Get the {@value EnumVarMap#ENUM_VALUES} from the allowableValues map.
     * @param allowableValues The allowableValues map
     * @return the list of enumValues
     */
    public static List<String> getEnumValuesAsString(Map<String, Object> allowableValues) {
        @SuppressWarnings("unchecked")
        List<String> enumValues = (List<String>) allowableValues.get(ENUM_VALUES);
        return enumValues;
    }

    /**
     * Bungie has defined their own enum extension to describe enum values.
     * See <a href="https://github.com/Bungie-net/api?tab=readme-ov-file">documentation</a> and the section with
     * {@code x-enum-values} for more information.
     *
     * @param xEnumValues The {@code x-enum-values} Bungie extension map
     * @return A list of {@code enumVars} extracted from the {@code x-enum-values} extension
     */
    public static List<Map<String, String>> getBungieEnumValues(List<Map<String, Object>> xEnumValues) {
        List<Map<String, String>> enumVars = new ArrayList<>();
        for (Map<String, Object> enumValue : xEnumValues) {
            Map<String, String> enumVar = new HashMap<>();
            enumVar.put(ENUM_NAME, (String) enumValue.get(BUNGIE_ENUM_IDENTIFIER));
            if (enumValue.containsKey(BUNGIE_ENUM_NUMERIC_VALUE) && enumValue.get(BUNGIE_ENUM_NUMERIC_VALUE) != null) {
                enumVar.put(ENUM_VALUE, enumValue.get(BUNGIE_ENUM_NUMERIC_VALUE).toString());
            }
            if (enumValue.containsKey(BUNGIE_ENUM_DESCRIPTION) && enumValue.get(BUNGIE_ENUM_DESCRIPTION) != null) {
                enumVar.put(ENUM_DESCRIPTION, enumValue.get(BUNGIE_ENUM_DESCRIPTION).toString());
            }
            enumVars.add(enumVar);
        }
        return enumVars;
    }

    /**
     * Simplifies a composed schema (oneOf/anyOf) where all sub-schemas are enums
     * to a single enum schema containing all the values.
     *
     * @param schema       Schema to modify
     * @param subSchemas   List of sub-schemas to check
     * @param composedType Type of composed schema ("oneOf" or "anyOf")
     * @param openAPI      The OpenAPI object to resolve references
     * @return Simplified schema
     */
    public static Schema simplifyComposedSchemaWithEnums(Schema schema,
                                                         List<Object> subSchemas,
                                                         String composedType,
                                                         OpenAPI openAPI) {
        Map<Object, EnumExtensions> enumExtensions = new LinkedHashMap<>();

        if (schema.getTypes() != null && schema.getTypes().size() > 1) {
            // we cannot handle enums with multiple types
            return schema;
        }

        if (subSchemas.size() < 2) {
            //do not process if there's less than 2 sub-schemas. It will be normalized later, and this prevents
            //named enum schemas from being converted to inline enum schemas
            return schema;
        }
        String schemaType = ModelUtils.getType(schema);

        for (Object item : subSchemas) {
            if (!(item instanceof Schema)) {
                return schema;
            }

            Schema subSchema = ModelUtils.getReferencedSchema(openAPI, (Schema) item);

            // Check if this sub-schema has an enum or const value (OAS 3.1 uses const for single-value enums)
            boolean definesEnum = ModelUtils.hasEnum(subSchema);
            if (!definesEnum && subSchema.getConst() == null) {
                return schema;
            }
            // If const is present but enum is not, treat const as a single enum value
            List<Object> subSchemaEnumValues = definesEnum
                    ? subSchema.getEnum()
                    : Arrays.asList(subSchema.getConst());

            // Ensure all sub-schemas have the same type (if type is specified)
            if (subSchema.getTypes() != null && subSchema.getTypes().size() > 1) {
                // we cannot handle enums with multiple types
                return schema;
            }
            String subSchemaType = ModelUtils.getType(subSchema);
            if (subSchemaType != null) {
                if (schemaType == null) {
                    schemaType = subSchemaType;
                } else if (!schemaType.equals(subSchema.getType())) {
                    return schema;
                }
            }
            enumExtensions.putAll(getEnumExtensions(subSchema, subSchemaEnumValues));
        }

        // Clear the composed schema type since we were able to successfully process all subSchemas
        if (ONE_OF.equals(composedType)) {
            schema.setOneOf(null);
        } else if (ANY_OF.equals(composedType)) {
            schema.setAnyOf(null);
        }
        return createSimplifiedEnumSchema(schema, enumExtensions, schemaType);
    }

    private static Map<Object, EnumExtensions> getEnumExtensions(Schema schema,
                                                                 List<Object> schemaEnumValues) {
        Map<Object, EnumExtensions> enumExtensions = new LinkedHashMap<>();
        boolean schemaDeprecated = Boolean.TRUE.equals(schema.getDeprecated());
        // Add all enum values from this sub-schema to our collection
        if (schemaEnumValues.size() == 1) {
            String description = schema.getTitle() == null ? "" : schema.getTitle();
            if (schema.getDescription() != null) {
                if (!description.isEmpty()) {
                    description += " - ";
                }
                description += schema.getDescription();
            }
            enumExtensions.put(schemaEnumValues.get(0), new EnumExtensions(description, schemaDeprecated));
        } else {
            for (Object enumValue : schemaEnumValues) {
                enumExtensions.put(enumValue, new EnumExtensions("", schemaDeprecated));
            }
        }
        return enumExtensions;
    }

    /**
     * Creates a simplified enum schema from collected enum values.
     *
     * @param schema     schema to modify
     * @param enums      Collected enum values
     * @param schemaType Consistent type across sub-schemas
     * @return Simplified enum schema
     */
    private static Schema createSimplifiedEnumSchema(Schema schema,
                                                     Map<Object, EnumExtensions> enums,
                                                     String schemaType) {
        if (ModelUtils.getType(schema) == null && schemaType != null) {
            //if type was specified in subschemas, keep it in the main schema
            ModelUtils.setType(schema, schemaType);
        }

        schema.setEnum(new ArrayList<>(enums.keySet()));
        List<String> enumDescriptions = enums.values().stream().map(EnumExtensions::getDescription).collect(Collectors.toList());
        List<Boolean> enumDeprecations = enums.values().stream().map(EnumExtensions::isDeprecated).collect(Collectors.toList());
        if (enumDescriptions.stream().anyMatch(e -> !e.isEmpty())) {
            //set x-enum-descriptions only if there's at least one non-empty description
            schema.addExtension(X_ENUM_DESCRIPTIONS, new ArrayList<>(enumDescriptions));
        }
        if (enumDeprecations.stream().anyMatch(Boolean.TRUE::equals)) {
            // preserve per-value deprecated flags from OAS 3.1 oneOf/anyOf + const sub-schemas
            schema.addExtension(X_ENUM_DEPRECATED, new ArrayList<>(enumDeprecations));
        }

        return schema;
    }

    private static class EnumExtensions {
        private final String description;
        private final boolean deprecated;

        public EnumExtensions(String description, boolean deprecated) {
            this.description = description;
            this.deprecated = deprecated;
        }

        public String getDescription() {
            return description;
        }

        public boolean isDeprecated() {
            return deprecated;
        }
    }

}
