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
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.parameters.Parameter;
import lombok.Getter;
import lombok.Setter;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Stateful delegate that centralises all Spring Pageable / PagedModel logic shared by
 * {@link SpringCodegen} (java-spring) and {@link KotlinSpringServerCodegen} (kotlin-spring).
 *
 * <p>Because those two generators extend different base classes
 * ({@code AbstractJavaCodegen} and {@code AbstractKotlinCodegen}), a shared abstract class
 * is not possible. Instead, each generator holds one instance of this class and delegates
 * the three lifecycle phases ({@code preprocessOpenAPI}, {@code fromOperation},
 * {@code postProcessAllModels}) to it via the inner {@link Context} interface.</p>
 *
 * <p>Language-specific variations (file extension, annotation-array brackets, HTTP-interface
 * library name) are passed as parameters at call sites, keeping this class free of any
 * language-specific logic.</p>
 *
 * <h2>Relationship to {@link GenericSubstitutionSupport}</h2>
 * <p>Substitution and suppression of paged-model schemas is delegated to
 * {@link GenericSubstitutionSupport}, which is the single code path shared with the
 * name-based {@code genericPatterns} feature. {@link #contributeToGenericSubstitution}
 * converts each structurally-detected {@link PagedModelScanUtils.DetectedPagedModel} into a
 * {@link GenericSchemaScanUtils.GenericInstance} and registers it (alongside the raw name
 * of the companion {@code PageMetadata}-style schema) so downstream return-type substitution,
 * property substitution, and meta-schema suppression all happen uniformly.</p>
 *
 * <p>Wiring order in the generator's {@code preprocessOpenAPI}:</p>
 * <ol>
 *   <li>{@link #preprocessOpenAPI} (this class) — scans pageable features.</li>
 *   <li>{@link #contributeToGenericSubstitution} — feeds detected paged models into the
 *       generic substitution registry.</li>
 *   <li>{@link GenericSubstitutionSupport#preprocessOpenAPI} — runs tier-1/tier-2 generic
 *       pattern scanning (vendor extensions override pre-scanned entries; tier-2 skips them).</li>
 * </ol>
 */
public final class SpringPageableSupport {

    private final Logger LOGGER = LoggerFactory.getLogger(SpringPageableSupport.class);

    // -------------------------------------------------------------------------
    // Context interface — implemented by each generator to expose its internals
    // -------------------------------------------------------------------------

    /**
     * Narrow callback interface that gives {@link SpringPageableSupport} read/write access
     * to the generator's configuration without requiring a specific base class.
     */
    public interface Context {
        /** Returns the active library name (e.g. {@code "spring-boot"}). */
        String getLibrary();

        /** Returns the config package, e.g. {@code "org.openapitools.configuration"}. */
        String getConfigPackage();

        /** Returns the source folder path used to locate generated files. */
        String getSourceFolder();

        /** Returns whether bean-validation annotations are enabled. */
        boolean isUseBeanValidation();

        /** Returns the active annotation library. */
        AnnotationLibrary getAnnotationLibrary();

        /** Converts an unqualified schema name to a codegen model name. */
        String toModelName(String name);

        /**
         * Returns the generator's mutable {@code importMapping} map.
         * Callers may add entries directly.
         */
        Map<String, String> importMapping();

        /**
         * Returns the generator's mutable {@code supportingFiles} list.
         * Callers may add entries directly.
         */
        List<SupportingFile> supportingFiles();

        /**
         * Called when {@code x-spring-paginated} is active and Spring-doc is the documentation
         * provider. Each language adds its own Springdoc Pageable annotation to the operation.
         *
         * <p>Java adds {@code ParameterObject}; Kotlin adds {@code @PageableAsQueryParam} to
         * {@code x-operation-extra-annotation}.</p>
         */
        void applySpringdocPageableAnnotation(CodegenOperation op);
    }

    // -------------------------------------------------------------------------
    // Feature flags (mirrored from each generator via setters)
    // -------------------------------------------------------------------------

    @Getter @Setter private boolean autoXSpringPaginated = false;
    @Getter @Setter private boolean generateSortValidation = false;
    @Getter @Setter private boolean generatePageableConstraintValidation = false;
    @Getter @Setter private boolean substituteGenericPagedModel = false;

    // -------------------------------------------------------------------------
    // State registries (populated in preprocessOpenAPI, consumed later)
    // -------------------------------------------------------------------------

    /** operationId → allowed sort values for {@code @ValidSort} generation */
    private Map<String, List<String>> sortValidationEnums = new HashMap<>();

    /** operationId → page/size/sort defaults for {@code @PageableDefault}/{@code @SortDefault} generation */
    private Map<String, SpringPageableScanUtils.PageableDefaultsData> pageableDefaultsRegistry = new HashMap<>();

    /** operationId → max page/size constraints for {@code @ValidPageable} generation */
    private Map<String, SpringPageableScanUtils.PageableConstraintsData> pageableConstraintsRegistry = new HashMap<>();

    /** schemaName → detected paged-model info, for return-type substitution and schema suppression */
    private Map<String, PagedModelScanUtils.DetectedPagedModel> pagedModelRegistry = new HashMap<>();

    /** Simple class name derived from importMapping; defaults to {@code "PagedModel"} */
    @Getter private String pagedModelClassName = "PagedModel";

    // -------------------------------------------------------------------------
    // Lifecycle methods
    // -------------------------------------------------------------------------

    /**
     * Scans the OpenAPI spec for pageable features and configures supporting files and
     * import mappings accordingly.
     *
     * <p>Call this from your generator's {@code preprocessOpenAPI} override.</p>
     *
     * @param openAPI              the OpenAPI model
     * @param ctx                  callback access to the generator's state
     * @param httpInterfaceLibrary the library name used for the HTTP-interface variant
     *                             (e.g. {@code "spring-http-interface"} for Java,
     *                             {@code "spring-declarative-http-interface"} for Kotlin)
     * @param fileExtension        language file extension without dot (e.g. {@code "java"} or {@code "kt"})
     */
    public void preprocessOpenAPI(OpenAPI openAPI, Context ctx,
                                  String httpInterfaceLibrary, String fileExtension) {
        String library = ctx.getLibrary();
        String configPath = (ctx.getSourceFolder() + File.separator + ctx.getConfigPackage())
                .replace(".", File.separator);

        if (SpringCodegen.SPRING_BOOT.equals(library) && generateSortValidation && ctx.isUseBeanValidation()) {
            sortValidationEnums = SpringPageableScanUtils.scanSortValidationEnums(openAPI, autoXSpringPaginated);
            if (!sortValidationEnums.isEmpty()) {
                ctx.importMapping().putIfAbsent("ValidSort", ctx.getConfigPackage() + ".ValidSort");
                ctx.supportingFiles().add(new SupportingFile("validSort.mustache",
                        configPath, "ValidSort." + fileExtension));
            }
        }

        if (SpringCodegen.SPRING_BOOT.equals(library)) {
            pageableDefaultsRegistry = SpringPageableScanUtils.scanPageableDefaults(openAPI, autoXSpringPaginated);
            if (!pageableDefaultsRegistry.isEmpty()) {
                ctx.importMapping().putIfAbsent("PageableDefault", "org.springframework.data.web.PageableDefault");
                ctx.importMapping().putIfAbsent("SortDefault", "org.springframework.data.web.SortDefault");
                ctx.importMapping().putIfAbsent("Sort", "org.springframework.data.domain.Sort");
            }
        }

        if (SpringCodegen.SPRING_BOOT.equals(library) && generatePageableConstraintValidation && ctx.isUseBeanValidation()) {
            pageableConstraintsRegistry = SpringPageableScanUtils.scanPageableConstraints(openAPI, autoXSpringPaginated);
            if (!pageableConstraintsRegistry.isEmpty()) {
                ctx.importMapping().putIfAbsent("ValidPageable", ctx.getConfigPackage() + ".ValidPageable");
                ctx.supportingFiles().add(new SupportingFile("validPageable.mustache",
                        configPath, "ValidPageable." + fileExtension));
            }
        }

        if ((SpringCodegen.SPRING_BOOT.equals(library) || httpInterfaceLibrary.equals(library)
                || SpringCodegen.SPRING_CLOUD_LIBRARY.equals(library))
                && substituteGenericPagedModel) {
            pagedModelRegistry = PagedModelScanUtils.scanPagedModels(openAPI);
            if (!pagedModelRegistry.isEmpty()) {
                boolean customMapping = ctx.importMapping().containsKey("PagedModel");
                ctx.importMapping().putIfAbsent("PagedModel", ctx.getConfigPackage() + ".PagedModel");
                if (!customMapping) {
                    ctx.supportingFiles().add(new SupportingFile("pagedModel.mustache",
                            configPath, "PagedModel." + fileExtension));
                }
                String fqn = ctx.importMapping().get("PagedModel");
                pagedModelClassName = fqn.substring(fqn.lastIndexOf('.') + 1);
                if (!pagedModelClassName.equals("PagedModel")) {
                    ctx.importMapping().putIfAbsent(pagedModelClassName, fqn);
                }
                LOGGER.info("substituteGenericPagedModel: detected {} paged-model schema(s): {}",
                        pagedModelRegistry.size(), pagedModelRegistry.keySet());
            }
        }
    }

    /**
     * Contributes structurally-detected paged-model schemas to {@link GenericSubstitutionSupport}
     * as pre-scanned {@link GenericSchemaScanUtils.GenericInstance}s.
     *
     * <p>Call this in the generator's {@code preprocessOpenAPI}, <em>after</em>
     * {@link #preprocessOpenAPI} (this class) and <em>before</em>
     * {@link GenericSubstitutionSupport#preprocessOpenAPI}. That ordering ensures:</p>
     * <ul>
     *   <li>Pre-scanned pageable instances are in the registry before tier-1 vendor-extension
     *       scanning — vendor extensions therefore override structural detection.</li>
     *   <li>The pre-scanned schemas are included in the {@code tier1Names} exclusion set, so
     *       tier-2 pattern scanning will not produce duplicate entries for them.</li>
     *   <li>The {@link GenericSubstitutionSupport} re-keying step (via {@code toModelName()})
     *       applies to the pre-scanned entries too, making them resilient to
     *       {@code modelNameSuffix} / {@code modelNamePrefix}.</li>
     * </ul>
     *
     * <p>No-op when {@code substituteGenericPagedModel} is disabled or no paged models
     * were detected.</p>
     *
     * @param genericSupport the {@link GenericSubstitutionSupport} delegate to populate
     * @param ctx            callback access to the generator's state
     */
    public void contributeToGenericSubstitution(GenericSubstitutionSupport genericSupport,
                                                Context ctx) {
        if (!substituteGenericPagedModel || pagedModelRegistry.isEmpty()) {
            return;
        }
        String pagedModelFqn = ctx.importMapping().get(pagedModelClassName);
        for (PagedModelScanUtils.DetectedPagedModel d : pagedModelRegistry.values()) {
            Map<String, String> typeArgs = new LinkedHashMap<>();
            typeArgs.put("content", d.itemSchemaName); // raw name; toModelName() applied by GenericSubstitutionSupport
            Map<String, String> slotTypeParams = new LinkedHashMap<>();
            slotTypeParams.put("content", "T");
            GenericSchemaScanUtils.GenericInstance inst = new GenericSchemaScanUtils.GenericInstance(
                    d.rawSchemaName,        // raw spec name; re-keyed by preprocessOpenAPI
                    pagedModelClassName,    // e.g. "PagedModel" or custom class simple name
                    pagedModelFqn,          // FQN already registered in importMapping
                    false,                  // Mode A — external / supporting-file class
                    typeArgs,
                    slotTypeParams,
                    "content",              // slot property name
                    false,                  // slotIsArray is only consumed by Mode B class generation
                    Collections.emptyList()
            );
            genericSupport.addPreScannedInstance(inst, d.rawMetaSchemaName);
        }
    }

    /**
     * Auto-detects pagination parameters ({@code page}, {@code size}, {@code sort}) on the
     * raw operation and marks it with {@code x-spring-paginated: true} if eligible.
     *
     * <p>Must be called <em>before</em> {@code super.fromOperation()} so that the extension
     * is copied to {@code codegenOperation.vendorExtensions} by the base class.</p>
     *
     * <p>Respects a manual {@code x-spring-paginated: false} override in the spec.</p>
     *
     * @param operation the raw OpenAPI operation
     * @param library   the active library name
     */
    public void autoDetectPagination(Operation operation, String library) {
        if (!SpringCodegen.SPRING_BOOT.equals(library) || !autoXSpringPaginated) {
            return;
        }
        if (operation.getExtensions() != null
                && Boolean.FALSE.equals(operation.getExtensions().get("x-spring-paginated"))) {
            return;
        }
        if (operation.getParameters() != null) {
            Set<String> paramNames = operation.getParameters().stream()
                    .map(Parameter::getName)
                    .collect(Collectors.toSet());
            if (paramNames.containsAll(Arrays.asList("page", "size", "sort"))) {
                if (operation.getExtensions() == null) {
                    operation.setExtensions(new HashMap<>());
                }
                operation.getExtensions().put("x-spring-paginated", Boolean.TRUE);
            }
        }
    }

    /**
     * Processes a {@code x-spring-paginated} operation: adds Pageable imports, builds
     * pageable parameter annotations, and removes the {@code page}/{@code size}/{@code sort}
     * query parameters.
     *
     * <p>Must be called <em>after</em> {@code super.fromOperation()} so that
     * {@code codegenOperation.vendorExtensions} is populated. This method is a no-op when
     * {@code x-spring-paginated} is not present or the library is not {@code spring-boot}.</p>
     *
     * @param codegenOperation the codegen operation to annotate
     * @param ctx              callback access to the generator's state
     * @param arrayOpen        opening bracket for annotation arrays:
     *                         {@code "{"} for Java, {@code "["} for Kotlin
     * @param arrayClose       closing bracket for annotation arrays:
     *                         {@code "}"} for Java, {@code "]"} for Kotlin
     */
    public void processPageableAnnotations(CodegenOperation codegenOperation, Context ctx,
                                           String arrayOpen, String arrayClose) {
        if (!SpringCodegen.SPRING_BOOT.equals(ctx.getLibrary())) {
            return;
        }
        if (!Boolean.TRUE.equals(codegenOperation.vendorExtensions.get("x-spring-paginated"))) {
            return;
        }

        ctx.importMapping().putIfAbsent("Pageable", "org.springframework.data.domain.Pageable");
        codegenOperation.imports.add("Pageable");
        ctx.applySpringdocPageableAnnotation(codegenOperation);

        List<String> defaultPageableQueryParams = Arrays.asList("page", "size", "sort");
        codegenOperation.queryParams.removeIf(p -> defaultPageableQueryParams.contains(p.baseName));
        codegenOperation.allParams.removeIf(p -> p.isQueryParam && defaultPageableQueryParams.contains(p.baseName));

        List<String> pageableAnnotations = new ArrayList<>();

        if (generatePageableConstraintValidation && ctx.isUseBeanValidation()
                && pageableConstraintsRegistry.containsKey(codegenOperation.operationId)) {
            SpringPageableScanUtils.PageableConstraintsData constraints =
                    pageableConstraintsRegistry.get(codegenOperation.operationId);
            List<String> attrs = new ArrayList<>();
            if (constraints.maxSize >= 0) attrs.add("maxSize = " + constraints.maxSize);
            if (constraints.maxPage >= 0) attrs.add("maxPage = " + constraints.maxPage);
            pageableAnnotations.add("@ValidPageable(" + String.join(", ", attrs) + ")");
            codegenOperation.imports.add("ValidPageable");
        }

        if (generateSortValidation && ctx.isUseBeanValidation()
                && sortValidationEnums.containsKey(codegenOperation.operationId)) {
            List<String> allowedSortValues = sortValidationEnums.get(codegenOperation.operationId);
            String allowedValuesStr = allowedSortValues.stream()
                    .map(v -> "\"" + v.replace("\\", "\\\\").replace("\"", "\\\"") + "\"")
                    .collect(Collectors.joining(", "));
            pageableAnnotations.add("@ValidSort(allowedValues = " + arrayOpen + allowedValuesStr + arrayClose + ")");
            codegenOperation.imports.add("ValidSort");
        }

        if (pageableDefaultsRegistry.containsKey(codegenOperation.operationId)) {
            SpringPageableScanUtils.PageableDefaultsData defaults =
                    pageableDefaultsRegistry.get(codegenOperation.operationId);
            if (defaults.page != null || defaults.size != null) {
                List<String> attrs = new ArrayList<>();
                if (defaults.page != null) attrs.add("page = " + defaults.page);
                if (defaults.size != null) attrs.add("size = " + defaults.size);
                pageableAnnotations.add("@PageableDefault(" + String.join(", ", attrs) + ")");
                codegenOperation.imports.add("PageableDefault");
            }
            if (!defaults.sortDefaults.isEmpty()) {
                // Java uses @SortDefault(sort = {...}); Kotlin uses SortDefault(sort = [...])
                String sortAnnotationPrefix = "{".equals(arrayOpen) ? "@SortDefault" : "SortDefault";
                List<String> sortEntries = defaults.sortDefaults.stream()
                        .map(sf -> sortAnnotationPrefix + "(sort = " + arrayOpen + "\"" + sf.field + "\""
                                + arrayClose + ", direction = Sort.Direction." + sf.direction + ")")
                        .collect(Collectors.toList());
                pageableAnnotations.add("@SortDefault.SortDefaults("
                        + ("{".equals(arrayOpen) ? arrayOpen + String.join(", ", sortEntries) + arrayClose
                                                 : String.join(", ", sortEntries))
                        + ")");
                codegenOperation.imports.add("SortDefault");
                codegenOperation.imports.add("Sort");
            }
        }

        if (!pageableAnnotations.isEmpty()) {
            codegenOperation.vendorExtensions.put("x-pageable-extra-annotation", pageableAnnotations);
        }
    }
}
