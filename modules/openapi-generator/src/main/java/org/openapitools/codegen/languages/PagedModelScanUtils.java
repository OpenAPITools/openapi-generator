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
import java.util.function.UnaryOperator;

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
     * <p>Two name variants are stored for each schema:</p>
     * <ul>
     *   <li><b>transformed</b> ({@code schemaName} / {@code metaSchemaName}) — the model name
     *       after the generator's {@code toModelName()} has been applied.  These are the names
     *       that appear in codegen-operation imports and {@code CodegenModel.imports}, so they
     *       must be used for import removal / import-presence checks.</li>
     *   <li><b>raw</b> ({@code rawSchemaName} / {@code rawMetaSchemaName}) — the original
     *       OpenAPI component-schema name.  {@code DefaultGenerator} keys {@code allProcessedModels}
     *       (the {@code objs} map passed to {@code postProcessAllModels}) by the <em>raw</em>
     *       schema name, so these values must be used for {@code objs.remove()} calls.</li>
     * </ul>
     *
     * <p>When {@link #scanPagedModels(OpenAPI)} is used (no transform), the raw and transformed
     * names are identical.  When {@link #scanPagedModels(OpenAPI, UnaryOperator)} is used, they
     * may differ (e.g. {@code rawSchemaName="UserPage"}, {@code schemaName="UserPageDto"}).</p>
     *
     * @param schemaName        Transformed model name of the detected paged schema.
     * @param itemSchemaName    Raw item schema name (always raw; callers apply
     *                          {@code toModelName()} at the point of use).
     * @param metaSchemaName    Transformed model name of the pagination-metadata schema,
     *                          or {@code null} if unresolved.
     * @param rawSchemaName     Raw OpenAPI schema name of the paged schema (for {@code objs.remove}).
     * @param rawMetaSchemaName Raw OpenAPI schema name of the pagination-metadata schema
     *                          (for {@code objs.remove}), or {@code null} if unresolved.
     */
    public static final class DetectedPagedModel {
        /** Transformed model name — use for import removal / import-presence checks. */
        public final String schemaName;
        public final String itemSchemaName;
        /** Transformed meta model name — use for import-presence checks. */
        public final String metaSchemaName;
        /** Raw OpenAPI schema name — use for {@code objs.remove()} in {@code postProcessAllModels}. */
        public final String rawSchemaName;
        /** Raw OpenAPI meta schema name — use for {@code objs.remove()} in {@code postProcessAllModels}. */
        public final String rawMetaSchemaName;

        /**
         * Convenience constructor used when no name transform is active (raw == transformed).
         */
        public DetectedPagedModel(String schemaName, String itemSchemaName, String metaSchemaName) {
            this(schemaName, itemSchemaName, metaSchemaName, schemaName, metaSchemaName);
        }

        DetectedPagedModel(String schemaName, String itemSchemaName, String metaSchemaName,
                           String rawSchemaName, String rawMetaSchemaName) {
            this.schemaName = schemaName;
            this.itemSchemaName = itemSchemaName;
            this.metaSchemaName = metaSchemaName;
            this.rawSchemaName = rawSchemaName;
            this.rawMetaSchemaName = rawMetaSchemaName;
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
     * Convenience overload that scans for paged-model schemas and immediately re-keys the
     * resulting map by applying {@code toModelName} to every schema name.
     *
     * <p>Generator classes must use this overload (passing {@code this::toModelName}) so that
     * the registry keys match the model-name-processed values used at lookup time
     * (e.g. {@code codegenOperation.returnBaseType}, {@code objs} keys).  This ensures
     * correctness when {@code modelNameSuffix}, {@code modelNamePrefix}, {@code schemaMapping},
     * or {@code modelNameMapping} are active.</p>
     *
     * <p>{@code itemSchemaName} inside each {@link DetectedPagedModel} is intentionally left as
     * the raw spec name because every call site already passes it through {@code toModelName()}
     * at the point of use.</p>
     *
     * @param openAPI     the parsed OpenAPI document
     * @param toModelName name-transformation function supplied by the generator
     *                    (typically {@code this::toModelName})
     * @return map from transformed schema name to {@link DetectedPagedModel}
     */
    public static Map<String, DetectedPagedModel> scanPagedModels(
            OpenAPI openAPI, UnaryOperator<String> toModelName) {
        Map<String, DetectedPagedModel> raw = scanPagedModels(openAPI);
        if (raw.isEmpty()) {
            return raw;
        }
        Map<String, DetectedPagedModel> result = new LinkedHashMap<>();
        for (Map.Entry<String, DetectedPagedModel> entry : raw.entrySet()) {
            DetectedPagedModel d = entry.getValue();
            String rawKey = entry.getKey();
            String newKey = toModelName.apply(rawKey);
            String rawMeta = d.metaSchemaName;
            String newMeta = rawMeta != null ? toModelName.apply(rawMeta) : null;
            result.put(newKey, new DetectedPagedModel(newKey, d.itemSchemaName, newMeta, rawKey, rawMeta));
        }
        return result;
    }

    /**
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
