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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;

/**
 * Language-agnostic utility for detecting OpenAPI schemas that represent paginated responses
 * (Spring {@code PagedModel}-like DTOs) and should be substituted with
 * {@code org.springframework.data.web.PagedModel<T>} during code generation.
 *
 * <p>Two structural patterns are recognised:</p>
 * <ol>
 *   <li><b>Flat-object form</b> — a plain {@code type: object} with a {@code content} array
 *       property and a separate property whose {@code $ref} points to a pagination-metadata
 *       schema (e.g. {@code PageMetadata}).</li>
 *   <li><b>allOf form</b> — a schema composed via {@code allOf} where one entry is a
 *       {@code $ref} to the pagination-metadata schema and another is an inline object
 *       with a {@code content} array property.</li>
 * </ol>
 *
 * <p>Detection heuristics deliberately avoid name-based matching and only trigger when
 * structural conditions are all met, to minimise false positives.</p>
 *
 * <p>Used by both {@link SpringCodegen} and {@link KotlinSpringServerCodegen}.</p>
 */
public final class PagedModelScanUtils {

    /** Minimum number of recognised pagination field names that must be present in a schema
     *  for it to be classified as a pagination-metadata schema. */
    private static final int PAGINATION_FIELD_THRESHOLD = 2;

    private static final Set<String> PAGINATION_FIELD_NAMES = new HashSet<>(Arrays.asList(
            "size", "number", "page", "totalElements", "totalPages"
    ));

    private PagedModelScanUtils() {}

    // -------------------------------------------------------------------------
    // Data class
    // -------------------------------------------------------------------------

    /**
     * Carries the result of a single detected paged-model schema.
     *
     * @param schemaName     Name of the detected schema to suppress (e.g. {@code UserPage}).
     * @param itemSchemaName Simple name of the array item type (e.g. {@code User}).
     * @param metaSchemaName Name of the pagination-metadata schema to suppress
     *                       (e.g. {@code PageMetadata}), or {@code null} if it could not
     *                       be resolved to a named component.
     */
    public static final class DetectedPagedModel {
        public final String schemaName;
        public final String itemSchemaName;
        public final String metaSchemaName;

        public DetectedPagedModel(String schemaName, String itemSchemaName, String metaSchemaName) {
            this.schemaName = schemaName;
            this.itemSchemaName = itemSchemaName;
            this.metaSchemaName = metaSchemaName;
        }
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Scans all named schemas in {@code openAPI.components.schemas} and returns those that
     * match the paged-model pattern.
     *
     * @param openAPI the parsed OpenAPI document
     * @return map from schema name to {@link DetectedPagedModel}; empty if none found or
     *         if {@code components} / {@code schemas} is absent
     */
    public static Map<String, DetectedPagedModel> scanPagedModels(OpenAPI openAPI) {
        Map<String, DetectedPagedModel> result = new LinkedHashMap<>();
        if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null) {
            return result;
        }

        Map<String, Schema> allSchemas = openAPI.getComponents().getSchemas();

        for (Map.Entry<String, Schema> entry : allSchemas.entrySet()) {
            String name = entry.getKey();
            Schema<?> schema = entry.getValue();

            DetectedPagedModel detected = tryDetect(name, schema, openAPI);
            if (detected != null) {
                result.put(name, detected);
            }
        }
        return result;
    }

    /**
     * Returns {@code true} if the given schema looks like a pagination-metadata schema.
     *
     * <p>The heuristic checks that at least {@value #PAGINATION_FIELD_THRESHOLD} of the
     * well-known field names ({@code size}, {@code number}, {@code page},
     * {@code totalElements}, {@code totalPages}) are present as direct properties.</p>
     */
    public static boolean isPaginationSchema(Schema<?> schema) {
        if (schema == null || schema.getProperties() == null) {
            return false;
        }
        long matches = schema.getProperties().keySet().stream()
                .filter(PAGINATION_FIELD_NAMES::contains)
                .count();
        return matches >= PAGINATION_FIELD_THRESHOLD;
    }

    // -------------------------------------------------------------------------
    // Private detection helpers
    // -------------------------------------------------------------------------

    private static DetectedPagedModel tryDetect(String name, Schema<?> schema, OpenAPI openAPI) {
        // Try flat-object form first
        DetectedPagedModel flat = tryDetectFlatObject(name, schema, openAPI);
        if (flat != null) {
            return flat;
        }
        // Fall back to allOf form
        return tryDetectAllOf(name, schema, openAPI);
    }

    /**
     * Detects the flat-object form:
     * <pre>
     * UserPage:
     *   type: object
     *   properties:
     *     content:
     *       type: array
     *       items:
     *         $ref: '#/components/schemas/User'
     *     page:
     *       $ref: '#/components/schemas/PageMetadata'
     * </pre>
     */
    @SuppressWarnings("rawtypes")
    private static DetectedPagedModel tryDetectFlatObject(String name, Schema<?> schema, OpenAPI openAPI) {
        if (schema.getProperties() == null || schema.getAllOf() != null) {
            return null;
        }

        Map<String, Schema> props = schema.getProperties();

        // Must have a 'content' property
        Schema contentProp = props.get("content");
        if (contentProp == null) {
            return null;
        }

        // 'content' must be an array with items.$ref
        String itemRef = extractArrayItemRef(contentProp);
        if (itemRef == null) {
            return null;
        }

        // Must have at least one other property that $refs a pagination-metadata schema
        String metaSchemaName = null;
        for (Map.Entry<String, Schema> propEntry : props.entrySet()) {
            if ("content".equals(propEntry.getKey())) {
                continue;
            }
            Schema propSchema = propEntry.getValue();
            String ref = propSchema.get$ref();
            if (ref != null) {
                Schema<?> referencedSchema = ModelUtils.getReferencedSchema(openAPI, propSchema);
                if (referencedSchema != null && isPaginationSchema(referencedSchema)) {
                    metaSchemaName = extractSchemaNameFromRef(ref);
                    break;
                }
            }
        }

        if (metaSchemaName == null) {
            return null;
        }

        String itemSchemaName = extractSchemaNameFromRef(itemRef);
        return new DetectedPagedModel(name, itemSchemaName, metaSchemaName);
    }

    /**
     * Detects the allOf form:
     * <pre>
     * UserPage:
     *   allOf:
     *     - $ref: '#/components/schemas/PageMeta'
     *     - type: object
     *       properties:
     *         content:
     *           type: array
     *           items:
     *             $ref: '#/components/schemas/User'
     * </pre>
     */
    @SuppressWarnings("rawtypes")
    private static DetectedPagedModel tryDetectAllOf(String name, Schema<?> schema, OpenAPI openAPI) {
        if (schema.getAllOf() == null || schema.getAllOf().isEmpty()) {
            return null;
        }

        String metaSchemaName = null;
        String itemRef = null;

        for (Object entryObj : schema.getAllOf()) {
            if (!(entryObj instanceof Schema)) {
                continue;
            }
            Schema entry = (Schema) entryObj;

            if (entry.get$ref() != null) {
                // Candidate for pagination-metadata ref
                Schema<?> referenced = ModelUtils.getReferencedSchema(openAPI, entry);
                if (referenced != null && isPaginationSchema(referenced)) {
                    metaSchemaName = extractSchemaNameFromRef(entry.get$ref());
                }
            } else if (entry.getProperties() != null) {
                // Candidate for inline content schema
                Schema contentProp = (Schema) entry.getProperties().get("content");
                if (contentProp != null) {
                    itemRef = extractArrayItemRef(contentProp);
                }
            }
        }

        if (metaSchemaName == null || itemRef == null) {
            return null;
        }

        String itemSchemaName = extractSchemaNameFromRef(itemRef);
        return new DetectedPagedModel(name, itemSchemaName, metaSchemaName);
    }

    // -------------------------------------------------------------------------
    // Utility helpers
    // -------------------------------------------------------------------------

    /**
     * Returns the {@code $ref} of the array items if the schema is an array with an
     * items {@code $ref}, or {@code null} otherwise.
     */
    @SuppressWarnings("rawtypes")
    private static String extractArrayItemRef(Schema<?> schema) {
        if (schema == null) {
            return null;
        }
        if (!"array".equals(schema.getType()) || schema.getItems() == null) {
            return null;
        }
        return schema.getItems().get$ref();
    }

    /**
     * Extracts the simple schema name from a {@code $ref} string such as
     * {@code #/components/schemas/User} → {@code User}.
     */
    static String extractSchemaNameFromRef(String ref) {
        if (ref == null) {
            return null;
        }
        int slash = ref.lastIndexOf('/');
        return slash >= 0 ? ref.substring(slash + 1) : ref;
    }
}
