package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.media.Schema;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.X_ENUM_DEPRECATED;
import static org.openapitools.codegen.CodegenConstants.X_ENUM_DESCRIPTIONS;

public class EnumUtils {

    public static Schema createSimplifiedEnumSchema(Schema originalSchema,
                                                     Map<Object, EnumExtensions> enums,
                                                     String schemaType) {
        if (ModelUtils.getType(originalSchema) == null && schemaType != null) {
            //if type was specified in subschemas, keep it in the main schema
            ModelUtils.setType(originalSchema, schemaType);
        }

        originalSchema.setEnum(new ArrayList<>(enums.keySet()));
        List<String> enumDescriptions = enums.values().stream().map(EnumExtensions::getDescription).collect(Collectors.toList());
        List<Boolean> enumDeprecations = enums.values().stream().map(EnumExtensions::isDeprecated).collect(Collectors.toList());
        if(enumDescriptions.stream().anyMatch(e -> !e.isEmpty())) {
            //set x-enum-descriptions only if there's at least one non-empty description
            originalSchema.addExtension(X_ENUM_DESCRIPTIONS, new ArrayList<>(enumDescriptions));
        }
        if (enumDeprecations.stream().anyMatch(Boolean.TRUE::equals)) {
            // preserve per-value deprecated flags from OAS 3.1 oneOf/anyOf + const sub-schemas
            originalSchema.addExtension(X_ENUM_DEPRECATED, new ArrayList<>(enumDeprecations));
        }

        return originalSchema;
    }

    public static class EnumExtensions {
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
