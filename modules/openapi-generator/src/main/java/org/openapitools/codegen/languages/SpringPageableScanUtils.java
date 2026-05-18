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
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Utility class for scanning OpenAPI specs for Spring Pageable-related features:
 * sort enum validation, pageable defaults, and pageable constraints (max page/size).
 *
 * <p>Can be used as a static utility or instantiated to hold the scan results
 * ({@link #sortValidationEnums}, {@link #pageableDefaultsRegistry},
 * {@link #pageableConstraintsRegistry}) so that callers do not need to maintain
 * those maps themselves. Call {@link #scanAll(OpenAPI, boolean)} once in
 * {@code preprocessOpenAPI} to populate them, then access the fields directly.</p>
 *
 * <p>Used by both kotlin {@link KotlinSpringServerCodegen} and java {@link SpringCodegen} to share
 * scan and annotation-building logic. Only the mustache templates and their registration remain
 * language-specific.</p>
 */
public class SpringPageableScanUtils {

    public static final String PAGE = "page";
    public static final String SIZE = "size";
    public static final String SORT = "sort";

    /**
     * The three Spring Data Web query-parameter names that together signal a
     * {@link org.springframework.data.domain.Pageable} operation:
     * {@code page}, {@code size}, and {@code sort}.
     *
     * <p>Use this constant instead of repeating {@code Arrays.asList("page", "size", "sort")}
     * inline so that all callers stay in sync automatically.</p>
     */
    public static final List<String> DEFAULT_PAGEABLE_QUERY_PARAMS =
            Collections.unmodifiableList(Arrays.asList(PAGE, SIZE, SORT));

    // -------------------------------------------------------------------------
    // Instance state (populated by scanAll)
    // -------------------------------------------------------------------------

    /** Map from operationId to allowed sort values; populated by {@link #scanAll}. */
    public Map<String, List<String>> sortValidationEnums = new HashMap<>();

    /** Map from operationId to pageable defaults; populated by {@link #scanAll}. */
    public Map<String, PageableDefaultsData> pageableDefaultsRegistry = new HashMap<>();

    /** Map from operationId to pageable constraints; populated by {@link #scanAll}. */
    public Map<String, PageableConstraintsData> pageableConstraintsRegistry = new HashMap<>();

    public SpringPageableScanUtils() {}

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
     * Carries max and min constraints for page number and page size from a pageable operation.
     * {@code -1} means no constraint specified (no {@code maximum:}/{@code minimum:} in the spec).
     */
    public static final class PageableConstraintsData {
        /** Maximum allowed page number, or {@code -1} if unconstrained. */
        public final int maxPage;
        /** Maximum allowed page size, or {@code -1} if unconstrained. */
        public final int maxSize;
        /** Minimum allowed page number, or {@code -1} if unconstrained. */
        public final int minPage;
        /** Minimum allowed page size, or {@code -1} if unconstrained. */
        public final int minSize;

        public PageableConstraintsData(int maxPage, int maxSize, int minPage, int minSize) {
            this.maxPage = maxPage;
            this.maxSize = maxSize;
            this.minPage = minPage;
            this.minSize = minSize;
        }

        public boolean hasAny() {
            return maxPage >= 0 || maxSize >= 0 || minPage >= 0 || minSize >= 0;
        }
    }

    // -------------------------------------------------------------------------
    // Instance methods
    // -------------------------------------------------------------------------

    /**
     * Populates {@link #sortValidationEnums}, {@link #pageableDefaultsRegistry}, and
     * {@link #pageableConstraintsRegistry} by scanning the given OpenAPI document.
     *
     * <p>Call this once from {@code preprocessOpenAPI} (guarded by your library check),
     * then read the public map fields in {@code fromOperation}.</p>
     *
     * @param openAPI              the OpenAPI document to scan
     * @param autoXSpringPaginated whether auto-detection of pageable operations is enabled
     */
    public void scanAll(OpenAPI openAPI, boolean autoXSpringPaginated) {
        sortValidationEnums = scanSortValidationEnums(openAPI, autoXSpringPaginated);
        pageableDefaultsRegistry = scanPageableDefaults(openAPI, autoXSpringPaginated);
        pageableConstraintsRegistry = scanPageableConstraints(openAPI, autoXSpringPaginated);
    }

    /**
     * Instance variant of {@link #applyPageableAnnotations(CodegenOperation, boolean, boolean,
     * Map, boolean, Map, Map, AnnotationSyntax)} that uses the maps populated by
     * {@link #scanAll(OpenAPI, boolean)}.
     */
    public void applyPageableAnnotations(
            CodegenOperation codegenOperation,
            boolean generatePageableConstraintValidation,
            boolean useBeanValidation,
            boolean generateSortValidation,
            AnnotationSyntax syntax) {
        applyPageableAnnotations(codegenOperation,
                generatePageableConstraintValidation, useBeanValidation, pageableConstraintsRegistry,
                generateSortValidation, sortValidationEnums,
                pageableDefaultsRegistry, syntax);
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
        Boolean xSpringPaginated = getXSpringPaginated(operation);
        if (xSpringPaginated != null) {
            return xSpringPaginated;
        }
        if (!autoXSpringPaginated || operation.getParameters() == null) {
            return false;
        }
        Set<String> paramNames = operation.getParameters().stream()
                .map(Parameter::getName)
                .collect(Collectors.toSet());
        return paramNames.containsAll(DEFAULT_PAGEABLE_QUERY_PARAMS);
    }

    /**
     * Returns the resolved `x-spring-paginated` flag.
     *
     * @return `Boolean.TRUE`/`Boolean.FALSE` when explicitly set, otherwise `null`
     */
    public static Boolean getXSpringPaginated(Operation operation) {
        if (operation.getExtensions() == null) {
            return null;
        }
        Object xSpringPaginated = operation.getExtensions().get("x-spring-paginated");
        if (Boolean.FALSE.equals(xSpringPaginated)) {
            return false;
        }
        if (Boolean.TRUE.equals(xSpringPaginated)) {
            return true;
        }
        return null;
    }

    /**
     * Auto-detects Pageable pagination query parameters and, when detected, mutates the
     * operation by setting {@code x-spring-paginated: true} on its vendor extensions.
     *
     * <p>Detection is delegated to {@link #willBePageable(Operation, boolean)}. If the
     * operation is already explicitly flagged ({@code x-spring-paginated: true/false})
     * this method is a read-only pass-through — it returns the explicit value without
     * mutating extensions. Only auto-detected operations (those whose flag was
     * {@code null}) have the extension written.</p>
     *
     * <p>This method centralises the "detect + mutate" logic shared by both
     * {@link SpringCodegen} and {@link KotlinSpringServerCodegen} inside their
     * {@code fromOperation} overrides. It must be called <em>before</em>
     * {@code super.fromOperation()} so that the base codegen can pick up the extension
     * when populating {@code CodegenOperation.vendorExtensions}.</p>
     *
     * @param operation            the raw OpenAPI {@link Operation} to inspect (and possibly mutate)
     * @param autoXSpringPaginated whether auto-detection is enabled for this generator
     * @return {@code true} if the operation is (or was just marked as) paginated
     */
    public static boolean applyAutoXSpringPaginatedIfNeeded(
            Operation operation, boolean autoXSpringPaginated) {
        if (!willBePageable(operation, autoXSpringPaginated)) {
            return false;
        }
        if (getXSpringPaginated(operation) == null) {
            if (operation.getExtensions() == null) {
                operation.setExtensions(new HashMap<>());
            }
            operation.getExtensions().put("x-spring-paginated", Boolean.TRUE);
        }
        return true;
    }

    /**
     * Removes the three Spring Data Web default pagination query parameters ({@code page},
     * {@code size}, {@code sort}) from the given codegen operation's parameter lists.
     *
     * <p>When an operation is marked with {@code x-spring-paginated}, Spring injects a single
     * {@link org.springframework.data.domain.Pageable} parameter that internally handles
     * {@code page}, {@code size}, and {@code sort}. The individual query parameters must
     * therefore be removed from the generated method signature.</p>
     *
     * <p>Callers are responsible for ensuring this is only invoked for server-side library
     * configurations (spring-boot) where Pageable injection actually takes place.</p>
     *
     * @param codegenOperation the operation whose parameter lists should be pruned
     */
    public static void removePageableQueryParams(CodegenOperation codegenOperation) {
        codegenOperation.queryParams.removeIf(p -> DEFAULT_PAGEABLE_QUERY_PARAMS.contains(p.baseName));
        codegenOperation.allParams.removeIf(p -> p.isQueryParam && DEFAULT_PAGEABLE_QUERY_PARAMS.contains(p.baseName));
    }

    // -------------------------------------------------------------------------
    // Annotation syntax
    // -------------------------------------------------------------------------

    /**
     * Target language annotation syntax for pageable annotations.
     *
     * <p>Java and Kotlin differ in how array-valued annotation attributes are written:</p>
     * <ul>
     *   <li>Java uses curly braces: {@code @ValidSort(allowedValues = {"a,asc"})}</li>
     *   <li>Kotlin uses square brackets: {@code @ValidSort(allowedValues = ["a,asc"])}</li>
     * </ul>
     *
     * <p>Additionally, repeated {@code @SortDefault} items inside
     * {@code @SortDefault.SortDefaults} differ:
     * Java prefixes each with {@code @} and wraps the list in {@code {}},
     * whereas Kotlin omits the {@code @} and passes the items without extra wrapping.</p>
     */
    public enum AnnotationSyntax {
        JAVA,
        KOTLIN;

        /**
         * Formats {@code content} as an array literal for this language:
         * {@code {content}} for Java, {@code [content]} for Kotlin.
         */
        public String arrayLiteral(String content) {
            return this == JAVA ? "{" + content + "}" : "[" + content + "]";
        }

        /**
         * Formats a single {@code @SortDefault} annotation item.
         * Java: {@code @SortDefault(innerContent)}, Kotlin: {@code SortDefault(innerContent)}.
         */
        public String sortDefaultItem(String innerContent) {
            return (this == JAVA ? "@" : "") + "SortDefault(" + innerContent + ")";
        }

        /**
         * Formats the argument list inside {@code @SortDefault.SortDefaults(...)}.
         * Java wraps with {@code {}}: {@code @SortDefault.SortDefaults({item1, item2})}.
         * Kotlin passes items directly: {@code @SortDefault.SortDefaults(item1, item2)}.
         */
        public String sortDefaultsArgs(List<String> items) {
            String joined = String.join(", ", items);
            return this == JAVA ? "{" + joined + "}" : joined;
        }
    }

    /**
     * Builds and attaches the pageable-parameter annotations ({@code @ValidPageable},
     * {@code @ValidSort}, {@code @PageableDefault}, {@code @SortDefault.SortDefaults})
     * to the given codegen operation.
     *
     * <p>The annotations and their imports are only added when the relevant feature flags
     * ({@code generatePageableConstraintValidation}, {@code generateSortValidation}) are
     * enabled and a matching entry exists in the corresponding registry for this
     * operation. The result is stored under the
     * {@code x-pageable-extra-annotation} vendor extension.</p>
     *
     * <p>Language-specific differences in annotation array syntax (Java {@code {...}} vs
     * Kotlin {@code [...]}) are handled by the {@code syntax} parameter.
     * SpringDoc-specific import additions remain in each calling codegen class since
     * they differ between Java ({@code ParameterObject}) and Kotlin
     * ({@code PageableAsQueryParam} + {@code x-operation-extra-annotation} mutation).</p>
     *
     * @param codegenOperation                     the operation to annotate
     * @param generatePageableConstraintValidation whether to emit {@code @ValidPageable}
     * @param useBeanValidation                    whether bean validation is active
     * @param pageableConstraintsRegistry          per-operationId constraint data
     * @param generateSortValidation               whether to emit {@code @ValidSort}
     * @param sortValidationEnums                  per-operationId allowed sort values
     * @param pageableDefaultsRegistry             per-operationId default page/size/sort data
     * @param syntax                               target language annotation syntax
     */
    public static void applyPageableAnnotations(
            CodegenOperation codegenOperation,
            boolean generatePageableConstraintValidation,
            boolean useBeanValidation,
            Map<String, PageableConstraintsData> pageableConstraintsRegistry,
            boolean generateSortValidation,
            Map<String, List<String>> sortValidationEnums,
            Map<String, PageableDefaultsData> pageableDefaultsRegistry,
            AnnotationSyntax syntax) {

        String operationId = codegenOperation.operationId;
        List<String> pageableAnnotations = new ArrayList<>();

        if (generatePageableConstraintValidation && useBeanValidation
                && pageableConstraintsRegistry.containsKey(operationId)) {
            PageableConstraintsData constraints = pageableConstraintsRegistry.get(operationId);
            List<String> attrs = new ArrayList<>();
            if (constraints.maxSize >= 0) attrs.add("maxSize = " + constraints.maxSize);
            if (constraints.maxPage >= 0) attrs.add("maxPage = " + constraints.maxPage);
            if (constraints.minSize >= 0) attrs.add("minSize = " + constraints.minSize);
            if (constraints.minPage >= 0) attrs.add("minPage = " + constraints.minPage);
            pageableAnnotations.add("@ValidPageable(" + String.join(", ", attrs) + ")");
            codegenOperation.imports.add("ValidPageable");
        }

        if (generateSortValidation && useBeanValidation && sortValidationEnums.containsKey(operationId)) {
            List<String> allowedSortValues = sortValidationEnums.get(operationId);
            String allowedValuesStr = allowedSortValues.stream()
                    .map(v -> "\"" + v.replace("\\", "\\\\").replace("\"", "\\\"") + "\"")
                    .collect(Collectors.joining(", "));
            pageableAnnotations.add("@ValidSort(allowedValues = " + syntax.arrayLiteral(allowedValuesStr) + ")");
            codegenOperation.imports.add("ValidSort");
        }

        if (pageableDefaultsRegistry.containsKey(operationId)) {
            PageableDefaultsData defaults = pageableDefaultsRegistry.get(operationId);
            if (defaults.page != null || defaults.size != null) {
                List<String> attrs = new ArrayList<>();
                if (defaults.page != null) attrs.add("page = " + defaults.page);
                if (defaults.size != null) attrs.add("size = " + defaults.size);
                pageableAnnotations.add("@PageableDefault(" + String.join(", ", attrs) + ")");
                codegenOperation.imports.add("PageableDefault");
            }
            if (!defaults.sortDefaults.isEmpty()) {
                List<String> sortEntries = defaults.sortDefaults.stream()
                        .map(sf -> syntax.sortDefaultItem(
                                "sort = " + syntax.arrayLiteral("\"" + sf.field + "\"")
                                + ", direction = Sort.Direction." + sf.direction))
                        .collect(Collectors.toList());
                pageableAnnotations.add("@SortDefault.SortDefaults(" + syntax.sortDefaultsArgs(sortEntries) + ")");
                codegenOperation.imports.add("SortDefault");
                codegenOperation.imports.add("Sort");
            }
        }

        if (!pageableAnnotations.isEmpty()) {
            codegenOperation.vendorExtensions.put("x-pageable-extra-annotation", pageableAnnotations);
        }
    }

    /**
     * Applies SpringDoc-specific annotation handling for a pageable operation.
     *
     * <p>When {@code isSpringDoc} is {@code true}:</p>
     * <ul>
     *   <li><b>Java</b>: adds the {@code ParameterObject} import, which instructs SpringDoc
     *       to expose the individual pageable query parameters in the OpenAPI UI.</li>
     *   <li><b>Kotlin</b>: adds the {@code PageableAsQueryParam} import and prepends
     *       {@code @PageableAsQueryParam} to the operation's
     *       {@code x-operation-extra-annotation} vendor extension so the Mustache template
     *       renders it on the generated controller method.</li>
     * </ul>
     *
     * <p>When {@code isSpringDoc} is {@code false} this method is a no-op.</p>
     *
     * @param codegenOperation the operation to annotate
     * @param syntax           target language annotation syntax
     * @param isSpringDoc      whether the active documentation provider is SpringDoc
     */
    public static void applySpringDocPageableAnnotation(
            CodegenOperation codegenOperation, AnnotationSyntax syntax, boolean isSpringDoc) {
        if (!isSpringDoc) {
            return;
        }
        if (syntax == AnnotationSyntax.JAVA) {
            codegenOperation.imports.add("ParameterObject");
        } else {
            codegenOperation.imports.add("PageableAsQueryParam");
            Object existingAnnotation = codegenOperation.vendorExtensions.get("x-operation-extra-annotation");
            List<String> existing = getObjectAsStringList(existingAnnotation);
            List<String> updated = new ArrayList<>();
            updated.add("@PageableAsQueryParam");
            updated.addAll(existing);
            codegenOperation.vendorExtensions.put("x-operation-extra-annotation", updated);
        }
    }

    @SuppressWarnings("unchecked")
    private static List<String> getObjectAsStringList(Object object) {
        if (object instanceof List) {
            return (List<String>) object;
        } else if (object instanceof String) {
            return Collections.singletonList((String) object);
        }
        return new ArrayList<>();
    }

    // -------------------------------------------------------------------------
    // Scan methods
    // -------------------------------------------------------------------------

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
                if (operationId == null
                        || operation.getParameters() == null
                        || !willBePageable(operation, autoXSpringPaginated)) {
                    continue;
                }
                for (Parameter param : operation.getParameters()) {
                    if (!SORT.equals(param.getName())) {
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
                    if (ModelUtils.isArraySchema(schema)) {
                        enumSchema = schema.getItems();
                        if (enumSchema != null && enumSchema.get$ref() != null) {
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
     * least one default are included)
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
                if (operationId == null
                        || !willBePageable(operation, autoXSpringPaginated)
                        || operation.getParameters() == null) {
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
                    Object defaultValue = ModelUtils.resolveDefault(openAPI, schema);
                    if (defaultValue == null) {
                        continue;
                    }
                    switch (param.getName()) {
                        case PAGE:
                            if (defaultValue instanceof Number) {
                                pageDefault = ((Number) defaultValue).intValue();
                            }
                            break;
                        case SIZE:
                            if (defaultValue instanceof Number) {
                                sizeDefault = ((Number) defaultValue).intValue();
                            }
                            break;
                        case SORT:
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
     * Scans all pageable operations for {@code maximum:} and {@code minimum:} constraints on
     * {@code page} and {@code size} parameters. Values are resolved through {@code allOf} and
     * {@code $ref} schemas so that constraints defined on shared component schemas are honoured.
     *
     * @return map from operationId to {@link PageableConstraintsData} (only operations with
     * at least one constraint are included)
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
                if (operationId == null
                        || !willBePageable(operation, autoXSpringPaginated)
                        || operation.getParameters() == null) {
                    continue;
                }
                int maxPage = -1;
                int maxSize = -1;
                int minPage = -1;
                int minSize = -1;
                for (Parameter param : operation.getParameters()) {
                    Schema<?> schema = param.getSchema();
                    if (schema == null) {
                        continue;
                    }
                    ModelUtils.ResolvedMaxBound maxBound = ModelUtils.resolveMaximumBound(openAPI, schema);
                    ModelUtils.ResolvedMinBound minBound = ModelUtils.resolveMinimumBound(openAPI, schema);
                    switch (param.getName()) {
                        case PAGE:
                            if (maxBound != null) {
                                maxPage = toIntInclusiveMax(maxBound);
                            }
                            if (minBound != null) {
                                minPage = toIntInclusiveMin(minBound);
                            }
                            break;
                        case SIZE:
                            if (maxBound != null) {
                                maxSize = toIntInclusiveMax(maxBound);
                            }
                            if (minBound != null) {
                                minSize = toIntInclusiveMin(minBound);
                            }
                            break;
                        default:
                            break;
                    }
                }
                PageableConstraintsData data = new PageableConstraintsData(maxPage, maxSize, minPage, minSize);
                if (data.hasAny()) {
                    result.put(operationId, data);
                }
            }
        }
        return result;
    }

    private static Integer toIntInclusiveMax(ModelUtils.ResolvedMaxBound maxBound) {
        if (maxBound == null) {
            return null;
        }
        return maxBound.exclusive ? maxBound.maxBound.intValue() - 1 : maxBound.maxBound.intValue();
    }

    private static Integer toIntInclusiveMin(ModelUtils.ResolvedMinBound minBound) {
        if (minBound == null) {
            return null;
        }
        return minBound.exclusive ? minBound.minBound.intValue() + 1 : minBound.minBound.intValue();
    }


}
