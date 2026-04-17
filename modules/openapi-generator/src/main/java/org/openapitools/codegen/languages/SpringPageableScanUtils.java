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

import com.fasterxml.jackson.databind.node.ArrayNode;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Language-agnostic utility methods for scanning OpenAPI specs for Spring Pageable-related
 * features: sort enum validation, pageable defaults, and pageable constraints (max page/size).
 *
 * <p>Used by both kotlin {@link KotlinSpringServerCodegen} and java {@link SpringCodegen} to share
 * scan logic. Only the mustache templates and their registration remain language-specific.</p>
 */
public final class SpringPageableScanUtils {

    private SpringPageableScanUtils() {}

    // -------------------------------------------------------------------------
    // Data classes
    // -------------------------------------------------------------------------

    /** Carries a parsed sort field and its direction (always "ASC" or "DESC") from the spec default. */
    public static final class SortFieldDefault {
        public final String field;
        public final String direction;

        public SortFieldDefault(String field, String direction) {
            this.field = field;
            this.direction = direction;
        }
    }

    /** Carries parsed default values for page, size, and sort fields from a pageable operation. */
    public static final class PageableDefaultsData {
        public final Integer page;
        public final Integer size;
        public final List<SortFieldDefault> sortDefaults;

        public PageableDefaultsData(Integer page, Integer size, List<SortFieldDefault> sortDefaults) {
            this.page = page;
            this.size = size;
            this.sortDefaults = sortDefaults;
        }

        public boolean hasAny() {
            return page != null || size != null || !sortDefaults.isEmpty();
        }
    }

    /**
     * Carries max constraints for page number and page size from a pageable operation.
     * {@code -1} means no constraint specified (no {@code maximum:} in the spec).
     */
    public static final class PageableConstraintsData {
        /** Maximum allowed page number, or {@code -1} if unconstrained. */
        public final int maxPage;
        /** Maximum allowed page size, or {@code -1} if unconstrained. */
        public final int maxSize;

        public PageableConstraintsData(int maxPage, int maxSize) {
            this.maxPage = maxPage;
            this.maxSize = maxSize;
        }

        public boolean hasAny() {
            return maxPage >= 0 || maxSize >= 0;
        }
    }

    // -------------------------------------------------------------------------
    // Scan methods
    // -------------------------------------------------------------------------

    /**
     * Returns {@code true} if the given operation will have a Pageable parameter injected —
     * either because it has {@code x-spring-paginated: true} explicitly, or because
     * {@code autoXSpringPaginated} is enabled and the operation has all three default
     * pagination query parameters (page, size, sort).
     */
    public static boolean willBePageable(Operation operation, boolean autoXSpringPaginated) {
        if (operation.getExtensions() != null) {
            Object paginated = operation.getExtensions().get("x-spring-paginated");
            if (Boolean.FALSE.equals(paginated)) {
                return false;
            }
            if (Boolean.TRUE.equals(paginated)) {
                return true;
            }
        }
        if (autoXSpringPaginated && operation.getParameters() != null) {
            Set<String> paramNames = operation.getParameters().stream()
                    .map(Parameter::getName)
                    .collect(Collectors.toSet());
            return paramNames.containsAll(Arrays.asList("page", "size", "sort"));
        }
        return false;
    }

    /**
     * Scans all pageable operations for a {@code sort} parameter with enum values.
     *
     * @return map from operationId to list of allowed sort strings (e.g. {@code ["id,asc", "id,desc"]})
     */
    public static Map<String, List<String>> scanSortValidationEnums(
            OpenAPI openAPI, boolean autoXSpringPaginated) {
        Map<String, List<String>> result = new LinkedHashMap<>();
        if (openAPI.getPaths() == null) {
            return result;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            for (Operation operation : pathEntry.getValue().readOperations()) {
                String operationId = operation.getOperationId();
                if (operationId == null || !willBePageable(operation, autoXSpringPaginated)) {
                    continue;
                }
                if (operation.getParameters() == null) {
                    continue;
                }
                for (Parameter param : operation.getParameters()) {
                    if (!"sort".equals(param.getName())) {
                        continue;
                    }
                    Schema<?> schema = param.getSchema();
                    if (schema == null) {
                        continue;
                    }
                    if (schema.get$ref() != null) {
                        schema = ModelUtils.getReferencedSchema(openAPI, schema);
                    }
                    if (schema == null) {
                        continue;
                    }
                    // If the top-level schema is an array, the enum lives on its items
                    Schema<?> enumSchema = schema;
                    if (schema.getItems() != null) {
                        enumSchema = schema.getItems();
                        if (enumSchema.get$ref() != null) {
                            enumSchema = ModelUtils.getReferencedSchema(openAPI, enumSchema);
                        }
                    }
                    if (enumSchema == null || enumSchema.getEnum() == null || enumSchema.getEnum().isEmpty()) {
                        continue;
                    }
                    List<String> enumValues = enumSchema.getEnum().stream()
                            .map(Object::toString)
                            .collect(Collectors.toList());
                    result.put(operationId, enumValues);
                }
            }
        }
        return result;
    }

    /**
     * Scans all pageable operations for default values on {@code page}, {@code size},
     * and {@code sort} parameters.
     *
     * @return map from operationId to {@link PageableDefaultsData} (only operations with at
     *         least one default are included)
     */
    public static Map<String, PageableDefaultsData> scanPageableDefaults(
            OpenAPI openAPI, boolean autoXSpringPaginated) {
        Map<String, PageableDefaultsData> result = new LinkedHashMap<>();
        if (openAPI.getPaths() == null) {
            return result;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            for (Operation operation : pathEntry.getValue().readOperations()) {
                String operationId = operation.getOperationId();
                if (operationId == null || !willBePageable(operation, autoXSpringPaginated)) {
                    continue;
                }
                if (operation.getParameters() == null) {
                    continue;
                }
                Integer pageDefault = null;
                Integer sizeDefault = null;
                List<SortFieldDefault> sortDefaults = new ArrayList<>();

                for (Parameter param : operation.getParameters()) {
                    Schema<?> schema = param.getSchema();
                    if (schema == null) {
                        continue;
                    }
                    if (schema.get$ref() != null) {
                        schema = ModelUtils.getReferencedSchema(openAPI, schema);
                    }
                    if (schema == null || schema.getDefault() == null) {
                        continue;
                    }
                    Object defaultValue = schema.getDefault();
                    switch (param.getName()) {
                        case "page":
                            if (defaultValue instanceof Number) {
                                pageDefault = ((Number) defaultValue).intValue();
                            }
                            break;
                        case "size":
                            if (defaultValue instanceof Number) {
                                sizeDefault = ((Number) defaultValue).intValue();
                            }
                            break;
                        case "sort":
                            List<String> sortValues = new ArrayList<>();
                            if (defaultValue instanceof String) {
                                sortValues.add((String) defaultValue);
                            } else if (defaultValue instanceof ArrayNode) {
                                ((ArrayNode) defaultValue).forEach(node -> sortValues.add(node.asText()));
                            } else if (defaultValue instanceof List) {
                                for (Object item : (List<?>) defaultValue) {
                                    sortValues.add(item.toString());
                                }
                            }
                            for (String sortStr : sortValues) {
                                String[] parts = sortStr.split(",", 2);
                                String field = parts[0].trim();
                                String direction = parts.length > 1 ? parts[1].trim().toUpperCase(Locale.ROOT) : "ASC";
                                sortDefaults.add(new SortFieldDefault(field, direction));
                            }
                            break;
                        default:
                            break;
                    }
                }

                PageableDefaultsData data = new PageableDefaultsData(pageDefault, sizeDefault, sortDefaults);
                if (data.hasAny()) {
                    result.put(operationId, data);
                }
            }
        }
        return result;
    }

    /**
     * Scans all pageable operations for {@code maximum:} constraints on {@code page} and
     * {@code size} parameters.
     *
     * @return map from operationId to {@link PageableConstraintsData} (only operations with
     *         at least one {@code maximum:} constraint are included)
     */
    public static Map<String, PageableConstraintsData> scanPageableConstraints(
            OpenAPI openAPI, boolean autoXSpringPaginated) {
        Map<String, PageableConstraintsData> result = new LinkedHashMap<>();
        if (openAPI.getPaths() == null) {
            return result;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            for (Operation operation : pathEntry.getValue().readOperations()) {
                String operationId = operation.getOperationId();
                if (operationId == null || !willBePageable(operation, autoXSpringPaginated)) {
                    continue;
                }
                if (operation.getParameters() == null) {
                    continue;
                }
                int maxPage = -1;
                int maxSize = -1;
                for (Parameter param : operation.getParameters()) {
                    Schema<?> schema = param.getSchema();
                    if (schema == null) {
                        continue;
                    }
                    if (schema.get$ref() != null) {
                        schema = ModelUtils.getReferencedSchema(openAPI, schema);
                    }
                    if (schema == null || schema.getMaximum() == null) {
                        continue;
                    }
                    int maximum = schema.getMaximum().intValue();
                    switch (param.getName()) {
                        case "page":
                            maxPage = maximum;
                            break;
                        case "size":
                            maxSize = maximum;
                            break;
                        default:
                            break;
                    }
                }
                PageableConstraintsData data = new PageableConstraintsData(maxPage, maxSize);
                if (data.hasAny()) {
                    result.put(operationId, data);
                }
            }
        }
        return result;
    }
}
