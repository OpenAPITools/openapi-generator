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
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.DocumentationProviderFeatures.AnnotationLibrary;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

/**
 * Stateful delegate that centralises all generic-schema substitution logic usable by
 * any Spring or other typed generator.
 *
 * <p>This class is the runtime counterpart to {@link GenericSchemaScanUtils}: the scanner
 * is pure/stateless; this class holds the scan results, drives file generation, and hooks
 * into the three code-generation lifecycle phases.</p>
 *
 * <h2>Usage</h2>
 * <ol>
 *   <li>Create one instance per generator run and store it as a field.</li>
 *   <li>Call {@link #addPattern} in {@code processOpts()} for each configured pattern.</li>
 *   <li>Call {@link #preprocessOpenAPI} from the generator's {@code preprocessOpenAPI} override.</li>
 *   <li>Call {@link #substituteReturnType} from the generator's {@code fromOperation} override.</li>
 *   <li>Call {@link #suppressGenericSchemas} from {@code postProcessAllModels}.</li>
 * </ol>
 *
 * <h2>Mode A vs Mode B</h2>
 * <ul>
 *   <li><b>Mode A</b> ({@code genericClass} is a FQN): only an import-mapping entry is
 *       added; no source file is generated.</li>
 *   <li><b>Mode B</b> ({@code genericClass} is a simple name): a {@code SupportingFile}
 *       entry is registered during {@code preprocessOpenAPI} using the
 *       {@code genericClass.mustache} template. The {@link #prepareSupportingFile} hook
 *       injects per-class bundle data so each class renders with its own properties.
 *       Generators that use this class must override
 *       {@link org.openapitools.codegen.CodegenConfig#prepareSupportingFile} and
 *       delegate to this method.</li>
 * </ul>
 *
 * <h2>Relationship to {@link SpringPageableSupport}</h2>
 * <p>This class is the single substitution / suppression code path for both
 * <em>name-based</em> generic patterns (this class' own {@code genericPatterns} config) and
 * <em>structurally-detected</em> paged-model schemas (contributed by
 * {@link SpringPageableSupport} when {@code substituteGenericPagedModel} is enabled):</p>
 * <ul>
 *   <li><b>{@code genericPatterns}</b> uses name-based pattern matching (suffix / prefix /
 *       vendor extensions). It can target any generic class with any number of type parameters
 *       ({@code slots}), but relies on schemas following a naming convention.</li>
 *   <li><b>{@code substituteGenericPagedModel}</b> uses structural detection via
 *       {@link PagedModelScanUtils}. {@link SpringPageableSupport#contributeToGenericSubstitution}
 *       converts each detected paged model into a {@link GenericSchemaScanUtils.GenericInstance}
 *       and registers it here via {@link #addPreScannedInstance}, along with the raw name of
 *       the companion metadata schema (e.g. {@code PageMetadata}) so that schema can be
 *       suppressed alongside the main schema when no longer referenced.</li>
 * </ul>
 *
 * <p>Precedence inside {@link #preprocessOpenAPI}: vendor-extension (tier 1) overrides
 * pre-scanned pageable, which in turn overrides configured patterns (tier 2). Suppression of
 * companion meta-schemas (e.g. {@code PageMetadata}) is gated on every associated main schema
 * having been successfully suppressed in the same pass, so a {@code schemaMapping}-protected
 * sibling will keep its meta-schema alive.</p>
 */
public final class GenericSubstitutionSupport {

    private static final Logger LOGGER = LoggerFactory.getLogger(GenericSubstitutionSupport.class);

    // =========================================================================
    // Context interface
    // =========================================================================

    /**
     * Narrow callback interface that gives {@link GenericSubstitutionSupport} read/write access
     * to the generator's configuration without requiring a specific base class.
     */
    public interface Context {
        /** Returns the config package, e.g. {@code "org.openapitools.configuration"}. */
        String getConfigPackage();

        /** Returns the source folder path (e.g. {@code "src/main/java"}). */
        String getSourceFolder();

        /**
         * Returns the active annotation library.
         */
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
         * Mode B classes are registered here as {@link SupportingFile} entries during
         * {@link #preprocessOpenAPI}.
         */
        List<SupportingFile> supportingFiles();

        /**
         * Returns the file extension for generated source files, without the leading dot.
         * {@code "java"} for Java; {@code "kt"} for Kotlin.
         */
        String fileExtension();

        /**
         * Converts a simple class name to a fully-qualified model import path.
         * Typically returns {@code modelPackage() + "." + className}.
         * Used to build FQN entries when syncing {@code ModelsMap.imports} after
         * property-type substitution.
         */
        String toModelImport(String className);

        /**
         * Returns the generator's schema mapping (schema name → external class name/FQN).
         * Used to skip generic substitution for schemas that are explicitly schema-mapped,
         * since the user's intent is to use an external class, not generate or substitute it.
         */
        Map<String, String> schemaMapping();
    }

    // =========================================================================
    // Configuration and state
    // =========================================================================

    private final List<GenericPatternConfig> patterns = new ArrayList<>();
    private boolean discoverGenericPatterns = false;

    /**
     * Map from concrete schema name (e.g. {@code "UserResponse"}) to its detected
     * {@link GenericSchemaScanUtils.GenericInstance}. Populated during
     * {@link #preprocessOpenAPI} and consumed in later lifecycle phases.
     */
    private final Map<String, GenericSchemaScanUtils.GenericInstance> instanceRegistry =
            new LinkedHashMap<>();

    /**
     * Maps raw companion meta-schema name to the set of raw main schema names that
     * reference it, contributed by structural-detection delegates via
     * {@link #addPreScannedInstance}. E.g. {@code "PageMetadata" → {"UserPage", "OrderPage"}}
     * when both paged models share one metadata schema.
     * <p>A meta-schema is suppressed in {@link #suppressGenericSchemas} only when <em>all</em>
     * associated main schemas were actually removed in the same pass, so a
     * {@code schemaMapping}-protected sibling keeps its meta-schema alive.</p>
     */
    private final Map<String, Set<String>> extraSuppressedMetaSchemas = new LinkedHashMap<>();

    /**
     * Bundle data for each Mode B class, keyed by simple class name (e.g. {@code "ApiResponse"}).
     * Built during {@link #preprocessOpenAPI} and injected into the template bundle by
     * {@link #prepareSupportingFile}.
     */
    private final Map<String, Map<String, Object>> modeBBundleData = new LinkedHashMap<>();

    // =========================================================================
    // Configuration setters
    // =========================================================================

    public void addPattern(GenericPatternConfig cfg) {
        patterns.add(cfg);
    }

    public void setDiscoverGenericPatterns(boolean v) {
        this.discoverGenericPatterns = v;
    }

    /**
     * Adds a pre-scanned generic instance contributed by a structural-detection delegate
     * (e.g. {@link SpringPageableSupport} for paged-model schemas).
     *
     * <p>Pre-scanned instances are added to the registry <em>before</em>
     * {@link #preprocessOpenAPI} scans tier-1 (vendor extensions) and tier-2 (configured
     * patterns). Tier-1 vendor-extension declarations therefore take precedence over
     * pre-scanned instances (overwrite them). Tier-2 pattern scanning skips schemas already
     * present in the registry (via the {@code tier1Names} exclusion set).</p>
     *
     * <p>Call this from the structural-detection delegate's
     * {@code contributeToGenericSubstitution} method, <em>before</em> calling
     * {@link #preprocessOpenAPI}.</p>
     *
     * @param inst              the detected generic instance; {@code inst.schemaName} must be
     *                          the raw OpenAPI schema name (re-keying via {@code toModelName()}
     *                          is performed automatically in {@link #preprocessOpenAPI})
     * @param rawMetaSchemaName raw OpenAPI name of a companion schema to suppress alongside
     *                          the main schema (e.g. {@code "PageMetadata"}), or {@code null}
     */
    public void addPreScannedInstance(GenericSchemaScanUtils.GenericInstance inst,
                                      String rawMetaSchemaName) {
        instanceRegistry.put(inst.schemaName, inst);
        if (rawMetaSchemaName != null) {
            extraSuppressedMetaSchemas
                    .computeIfAbsent(rawMetaSchemaName, k -> new LinkedHashSet<>())
                    .add(inst.schemaName);
        }
    }

    // =========================================================================
    // Lifecycle 1: preprocessOpenAPI
    // =========================================================================

    /**
     * Scans the OpenAPI spec for generic patterns, registers import mappings, and (for
     * Mode B) writes generated class source files to the output folder.
     *
     * <p>Call this from the generator's {@code preprocessOpenAPI} override, <em>after</em>
     * calling {@code super.preprocessOpenAPI(openAPI)}.</p>
     */
    public void preprocessOpenAPI(OpenAPI openAPI, Context ctx) {
        if (openAPI == null) return;

        // --- Tier 1: vendor extensions ---
        List<GenericSchemaScanUtils.GenericInstance> tier1 =
                GenericSchemaScanUtils.scanVendorExtensions(openAPI);
        for (GenericSchemaScanUtils.GenericInstance inst : tier1) {
            instanceRegistry.put(inst.schemaName, inst);
        }

        // --- Tier 2: configured patterns ---
        if (!patterns.isEmpty()) {
            Set<String> tier1Names = new HashSet<>(instanceRegistry.keySet());
            List<GenericSchemaScanUtils.GenericInstance> tier2 =
                    GenericSchemaScanUtils.scanWithPatterns(openAPI, patterns, tier1Names);
            for (GenericSchemaScanUtils.GenericInstance inst : tier2) {
                instanceRegistry.put(inst.schemaName, inst);
            }
        }

        // --- Tier 3: discovery (logging only) ---
        if (discoverGenericPatterns) {
            Set<String> alreadyHandled = new HashSet<>(instanceRegistry.keySet());
            List<GenericSchemaScanUtils.ClusterSuggestion> suggestions =
                    GenericSchemaScanUtils.discoverClusters(openAPI, alreadyHandled);
            for (GenericSchemaScanUtils.ClusterSuggestion suggestion : suggestions) {
                LOGGER.info("[discoverGenericPatterns] Potential generic pattern detected:\n"
                        + "  Schemas: {}\n"
                        + "  Varying slot: '{}' → {}\n"
                        + "  Suggested config:\n    {}",
                        String.join(", ", suggestion.schemaNames),
                        suggestion.varyingSlotProperty,
                        suggestion.varyingTypes,
                        suggestion.suggestedConfig.replace("\n", "\n    "));
            }
        }

        if (instanceRegistry.isEmpty()) {
            return;
        }

        // Re-key the registry by applying toModelName() to every raw spec schema name.
        // This ensures that lookups by op.returnBaseType (already transformed) and
        // objs keys (also transformed) succeed when modelNameSuffix / modelNamePrefix /
        // schemaMapping / modelNameMapping are active.
        Map<String, GenericSchemaScanUtils.GenericInstance> reKeyed = new LinkedHashMap<>();
        for (Map.Entry<String, GenericSchemaScanUtils.GenericInstance> entry : instanceRegistry.entrySet()) {
            String transformedKey = ctx.toModelName(entry.getKey());
            GenericSchemaScanUtils.GenericInstance previous = reKeyed.put(transformedKey, entry.getValue());
            if (previous != null) {
                LOGGER.warn("GenericSubstitutionSupport: schema names '{}' and '{}' both map to "
                                + "the transformed model name '{}' (via toModelName / nameMapping / "
                                + "modelNameMapping). Only '{}' will be substituted; '{}' will be ignored.",
                        previous.schemaName, entry.getValue().schemaName,
                        transformedKey, entry.getValue().schemaName, previous.schemaName);
            }
        }
        instanceRegistry.clear();
        instanceRegistry.putAll(reKeyed);

        // Gap B: filter out instances whose raw spec schema name is in schemaMapping.
        // When a user has schemaMapping: { UserResponse: com.example.UserResponse } they intend
        // to replace the schema with an external class — generic substitution must not override that.
        Map<String, String> schemaMappings = ctx.schemaMapping();
        if (!schemaMappings.isEmpty()) {
            instanceRegistry.entrySet().removeIf(entry -> {
                GenericSchemaScanUtils.GenericInstance inst = entry.getValue();
                if (schemaMappings.containsKey(inst.schemaName)) {
                    LOGGER.warn("GenericSubstitutionSupport: skipping generic instance '{}' — " +
                            "its raw schema name '{}' is present in schemaMapping; " +
                            "schemaMapping takes precedence over genericPatterns.",
                            entry.getKey(), inst.schemaName);
                    return true;
                }
                return false;
            });
        }

        LOGGER.info("GenericSubstitutionSupport: detected {} generic schema instance(s): {}",
                instanceRegistry.size(), instanceRegistry.keySet());

        // --- Register imports and (Mode B) generate class files ---
        String ext = ctx.fileExtension();
        String configPath = (ctx.getSourceFolder() + File.separator + ctx.getConfigPackage())
                .replace(".", File.separator);

        // Track which generic class names we've already processed (Mode A or Mode B)
        Map<String, GenericSchemaScanUtils.GenericInstance> processedGenericClasses = new LinkedHashMap<>();

        for (GenericSchemaScanUtils.GenericInstance inst : instanceRegistry.values()) {
            String className = inst.genericClassName;

            if (processedGenericClasses.containsKey(className)) {
                // Already registered this generic class
                continue;
            }
            processedGenericClasses.put(className, inst);

            if (inst.generateClass) {
                // Mode B: register a mustache-based supporting file and add import mapping
                String fqn = ctx.getConfigPackage() + "." + className;

                // Gap A: warn if there is a pre-existing importMapping entry that points elsewhere.
                // putIfAbsent below will keep the user's mapping, but the Mode B file is still
                // generated at configPackage — the two will be out of sync.
                String existingMapping = ctx.importMapping().get(className);
                if (existingMapping != null && !existingMapping.equals(fqn)) {
                    LOGGER.warn("GenericSubstitutionSupport: Mode B class '{}' conflicts with " +
                            "a pre-existing importMapping entry: importMapping maps '{}' → '{}', " +
                            "but the generated file will be at '{}.{}'. " +
                            "Generated imports will reference '{}' while the file lives elsewhere. " +
                            "Consider removing the conflicting importMapping entry.",
                            className, className, existingMapping, configPath, ext, existingMapping);
                }
                ctx.importMapping().putIfAbsent(className, fqn);

                if (!modeBBundleData.containsKey(className)) {
                    modeBBundleData.put(className, buildBundleData(inst));
                    ctx.supportingFiles().add(new SupportingFile("genericClass.mustache",
                            configPath, className + "." + ext));
                    LOGGER.info("GenericSubstitutionSupport: registered Mode B '{}' → {}.{}",
                            className, configPath, ext);
                }
            } else {
                // Mode A: FQN provided — add to importMapping only
                ctx.importMapping().putIfAbsent(className, inst.genericClassFqn);
                LOGGER.info("GenericSubstitutionSupport: Mode A class '{}' → importMapping: {}",
                        className, inst.genericClassFqn);
            }
        }
    }

    // =========================================================================
    // Lifecycle 2: substituteReturnType (called from fromOperation)
    // =========================================================================

    /**
     * Replaces the operation's return type with the generic form when the return base type
     * matches a detected generic schema instance.
     *
     * <p>Example: operation returning {@code UserResponse} becomes {@code ApiResponse<User>}.
     * If a type argument is itself a generic instance (e.g. {@code UserResponse} inside
     * {@code Page<T>}), the expansion is applied recursively:
     * {@code UserResponsePage → Page<ApiResponse<User>>}.</p>
     *
     * <p>Call this from {@code fromOperation} <em>after</em> calling
     * {@code super.fromOperation(…)}.</p>
     */
    public void substituteReturnType(CodegenOperation op, Context ctx) {
        if (instanceRegistry.isEmpty() || op.returnBaseType == null) {
            return;
        }
        GenericSchemaScanUtils.GenericInstance inst = instanceRegistry.get(op.returnBaseType);
        if (inst == null) {
            return;
        }

        String oldType = op.returnType;
        String expansion = buildGenericTypeName(inst, ctx, new HashSet<>());
        String newType;
        if (op.returnContainer != null && oldType != null) {
            // Preserve the outer container — e.g. List<UserResponse> → List<ApiResponse<User>>.
            // The matched base type appears as the inner type token inside the container.
            newType = oldType.replace(op.returnBaseType, expansion);
        } else {
            newType = expansion;
            op.returnContainer = null; // generic wrapper is not a container
        }

        op.returnType = newType;
        op.returnBaseType = inst.genericClassName;

        collectImportsToAdd(inst, ctx, op.imports, new HashSet<>());
        if (ctx.getAnnotationLibrary() == AnnotationLibrary.NONE) {
            // Remove wrapper schema imports (recursively: any nested generic instance is also suppressed).
            // op.imports holds toModelName()-processed names, matching the registry keys.
            Set<String> toRemove = new LinkedHashSet<>();
            collectSuppressedImports(inst, ctx, toRemove, new HashSet<>());
            op.imports.removeAll(toRemove);
        }

        LOGGER.info("GenericSubstitutionSupport: operation '{}': replacing return type '{}' with '{}'",
                op.operationId, oldType, newType);
    }

    // =========================================================================
    // Lifecycle 3: suppressGenericSchemas (called from postProcessAllModels)
    // =========================================================================

    /**
     * Substitutes generic-instance schema references in model properties and then removes
     * concrete generic-instance schemas (e.g. {@code UserResponse}, {@code PetResponse})
     * from the model map when {@code annotationLibrary=none}.
     *
     * <p>Property substitution is performed first so that any model referencing a
     * suppressed wrapper schema (e.g. {@code OrderDetails.userResult: UserResponse})
     * has its property type replaced ({@code ApiResponse<User>}) before the wrapper
     * class is removed.  This prevents compile errors in the generated code.</p>
     *
     * <p>A safety check prevents suppression when another model still references the
     * wrapper schema as a parent class ({@code extends UserResponse}) or via an
     * unsubstituted property, logging a warning in that case.</p>
     *
     * <p>When annotation libraries are active, {@code @ApiResponse} and {@code @Schema}
     * annotations in the generated code reference concrete schema classes, so they must
     * be kept (neither property substitution nor suppression is performed).</p>
     *
     * @param objs model map as received by {@code postProcessAllModels}
     * @param ctx  callback access to the generator's state
     * @return the (possibly mutated) model map
     */
    public Map<String, ModelsMap> suppressGenericSchemas(Map<String, ModelsMap> objs, Context ctx) {
        if (instanceRegistry.isEmpty() && extraSuppressedMetaSchemas.isEmpty()) {
            return objs;
        }
        if (ctx.getAnnotationLibrary() != AnnotationLibrary.NONE) {
            LOGGER.info("GenericSubstitutionSupport: keeping generic-instance schemas "
                    + "(annotationLibrary={}) — @ApiResponse annotations reference them",
                    ctx.getAnnotationLibrary().toCliOptValue());
            return objs;
        }

        substitutePropertyTypes(objs, ctx);

        // Track which raw schema names were actually removed (gates meta-schema suppression below).
        Set<String> suppressedRawNames = new HashSet<>();

        for (Map.Entry<String, GenericSchemaScanUtils.GenericInstance> entry
                : instanceRegistry.entrySet()) {
            GenericSchemaScanUtils.GenericInstance inst = entry.getValue();
            String transformedKey = entry.getKey(); // toModelName()-processed registry key

            // Safety check: skip suppression if any model still references this schema
            // via model.parent (inheritance) or an unsubstituted property baseType.
            if (isStillReferenced(transformedKey, objs)) {
                LOGGER.warn("GenericSubstitutionSupport: NOT suppressing '{}' — still referenced "
                        + "by another model (inheritance or unsubstituted property). "
                        + "The concrete class will be kept in the output.",
                        inst.schemaName);
                continue;
            }

            // objs is keyed by the raw OpenAPI schema name (DefaultGenerator uses spec keys as-is).
            // inst.schemaName is the raw spec name (toModelName() only affects the registry key).
            if (objs.remove(inst.schemaName) != null) {
                suppressedRawNames.add(inst.schemaName);
                LOGGER.info("GenericSubstitutionSupport: suppressing model '{}' → {}",
                        inst.schemaName, buildGenericTypeName(inst, ctx, new HashSet<>()));
            }
        }

        // Suppress companion meta-schemas contributed by pre-scan delegates (e.g.
        // substituteGenericPagedModel). A meta-schema is only suppressed when ALL of its
        // associated main schemas were actually removed in the loop above — if any sibling
        // (e.g. one protected by schemaMapping) is still present, the meta-schema stays.
        for (Map.Entry<String, Set<String>> metaEntry : extraSuppressedMetaSchemas.entrySet()) {
            String rawMeta = metaEntry.getKey();
            Set<String> rawMains = metaEntry.getValue();
            if (!suppressedRawNames.containsAll(rawMains)) {
                // At least one associated main was kept — keep the meta schema too.
                continue;
            }
            String transformedMeta = ctx.toModelName(rawMeta);
            if (isStillReferenced(transformedMeta, objs)) {
                LOGGER.info("GenericSubstitutionSupport: keeping companion meta-schema '{}'"
                        + " — still referenced by remaining models", rawMeta);
            } else if (objs.remove(rawMeta) != null) {
                LOGGER.info("GenericSubstitutionSupport: suppressing companion meta-schema '{}'"
                        + " — no longer referenced after main schema suppression", rawMeta);
            }
        }
        return objs;
    }

    // =========================================================================
    // prepareSupportingFile — per-file bundle injection
    // =========================================================================

    /**
     * Injects per-file data into the shared template bundle before each Mode B supporting
     * file is rendered.
     *
     * <p>Call this from the generator's {@code prepareSupportingFile} override.</p>
     *
     * @param bundle  the shared data bundle; will have {@code "genericClassDef"} added for Mode B files
     * @param support the supporting file about to be rendered
     */
    public void prepareSupportingFile(Map<String, Object> bundle, SupportingFile support) {
        if (!"genericClass.mustache".equals(support.getTemplateFile())) {
            return;
        }
        String dest = support.getDestinationFilename();
        int dot = dest.lastIndexOf('.');
        if (dot < 0) return;
        String className = dest.substring(0, dot);
        // Always write the key — using null clears any stale value from a previous Mode B
        // file render. Mustache's section helpers treat null as falsey so the template's
        // {{#genericClassDef}}…{{/genericClassDef}} block is correctly skipped.
        bundle.put("genericClassDef", modeBBundleData.get(className));
    }

    // =========================================================================
    // Mode B template bundle data
    // =========================================================================

    /**
     * Builds the data map injected into the bundle for a Mode B class template.
     * Contains {@code className}, {@code needsList}, and a {@code properties} list.
     */
    private Map<String, Object> buildBundleData(GenericSchemaScanUtils.GenericInstance instance) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("className", instance.genericClassName);

        boolean needsList = instance.properties.stream().anyMatch(p -> p.isArray);
        data.put("needsList", needsList ? Boolean.TRUE : null);

        // Build ordered typeParams list for template class declaration: [{typeParam:"T",isLast:true}, ...]
        List<String> distinctTypeParams = new ArrayList<>(new LinkedHashSet<>(instance.slotTypeParams.values()));
        List<Map<String, Object>> typeParamList = new ArrayList<>();
        for (int i = 0; i < distinctTypeParams.size(); i++) {
            Map<String, Object> tp = new LinkedHashMap<>();
            tp.put("typeParam", distinctTypeParams.get(i));
            tp.put("isLast", i == distinctTypeParams.size() - 1 ? Boolean.TRUE : null);
            typeParamList.add(tp);
        }
        data.put("typeParams", typeParamList);

        List<Map<String, Object>> propMaps = new ArrayList<>();
        for (GenericSchemaScanUtils.GenericProperty prop : instance.properties) {
            Map<String, Object> pm = new LinkedHashMap<>();
            pm.put("name", prop.name);
            pm.put("capitalName", capitalize(prop.name));
            pm.put("javaType", toJavaType(prop));
            pm.put("kotlinType", toKotlinType(prop));
            pm.put("required", prop.required ? Boolean.TRUE : null);
            propMaps.add(pm);
        }
        data.put("properties", propMaps);
        return data;
    }

    // =========================================================================
    // Type mapping helpers
    // =========================================================================

    private static String toJavaType(GenericSchemaScanUtils.GenericProperty prop) {
        if (prop.typeParam != null) {
            return prop.isArray ? "List<" + prop.typeParam + ">" : prop.typeParam;
        }
        switch (prop.openApiType) {
            case "$ref":   return prop.refTarget != null ? prop.refTarget : "Object";
            case "string": return "String";
            case "integer":
                return "int64".equals(prop.format) ? "Long" : "Integer";
            case "number":
                return "float".equals(prop.format) ? "Float" : "Double";
            case "boolean": return "Boolean";
            case "array":
                return prop.refTarget != null ? "List<" + prop.refTarget + ">" : "List<Object>";
            default: return "Object";
        }
    }

    private static String toKotlinType(GenericSchemaScanUtils.GenericProperty prop) {
        if (prop.typeParam != null) {
            return prop.isArray ? "List<" + prop.typeParam + ">" : prop.typeParam;
        }
        switch (prop.openApiType) {
            case "$ref":   return prop.refTarget != null ? prop.refTarget : "Any";
            case "string": return "String";
            case "integer":
                return "int64".equals(prop.format) ? "Long" : "Int";
            case "number":
                return "float".equals(prop.format) ? "Float" : "Double";
            case "boolean": return "Boolean";
            case "array":
                return prop.refTarget != null ? "List<" + prop.refTarget + ">" : "List<Any>";
            default: return "Any";
        }
    }

    // =========================================================================
    // Utilities
    // =========================================================================

    private static String capitalize(String s) {
        if (s == null || s.isEmpty()) return s;
        return Character.toUpperCase(s.charAt(0)) + s.substring(1);
    }

    // =========================================================================
    // Recursive generic type helpers
    // =========================================================================

    /**
     * Builds the fully-expanded generic type string for the given instance, recursing
     * into any type argument that is itself a registry entry.
     *
     * <p>Examples:
     * <ul>
     *   <li>{@code UserResponse → ApiResponse<User>}</li>
     *   <li>{@code UserResponsePage (Page&lt;T&gt; where T=UserResponse)
     *       → Page&lt;ApiResponse&lt;User&gt;&gt;}</li>
     * </ul>
     *
     * @param inst    the generic instance to expand
     * @param ctx     generator context (for {@code toModelName})
     * @param visited raw schema names already being expanded (cycle guard)
     * @return e.g. {@code "ApiResponse<User>"} or {@code "Page<ApiResponse<User>>"}
     */
    private String buildGenericTypeName(GenericSchemaScanUtils.GenericInstance inst,
                                        Context ctx, Set<String> visited) {
        if (!visited.add(inst.schemaName)) {
            // Cycle detected — fall back to plain class name to avoid infinite recursion
            return inst.genericClassName;
        }
        StringBuilder sb = new StringBuilder(inst.genericClassName).append("<");
        boolean first = true;
        for (String slotProp : inst.slotTypeParams.keySet()) {
            if (!first) sb.append(", ");
            first = false;
            String rawTypeArg = inst.typeArgs.get(slotProp);
            String transformedTypeArg = ctx.toModelName(rawTypeArg);
            GenericSchemaScanUtils.GenericInstance nestedInst = instanceRegistry.get(transformedTypeArg);
            if (nestedInst != null) {
                sb.append(buildGenericTypeName(nestedInst, ctx, visited));
            } else {
                sb.append(transformedTypeArg);
            }
        }
        sb.append(">");
        return sb.toString();
    }

    /**
     * Recursively collects all import names that need to be <em>added</em> when
     * substituting this instance: the generic class itself and all leaf type-arg names.
     *
     * <p>E.g. for {@code Page<ApiResponse<User>>}: adds {@code Page}, {@code ApiResponse},
     * {@code User}.</p>
     */
    private void collectImportsToAdd(GenericSchemaScanUtils.GenericInstance inst,
                                     Context ctx, Set<String> result, Set<String> visited) {
        if (!visited.add(inst.schemaName)) return;
        result.add(inst.genericClassName);
        for (String rawTypeArg : inst.typeArgs.values()) {
            String transformed = ctx.toModelName(rawTypeArg);
            GenericSchemaScanUtils.GenericInstance nestedInst = instanceRegistry.get(transformed);
            if (nestedInst != null) {
                collectImportsToAdd(nestedInst, ctx, result, visited);
            } else {
                result.add(transformed);
            }
        }
    }

    /**
     * Recursively collects the <em>transformed</em> schema names that should be
     * <em>removed</em> from imports after substitution (the wrapper schemas that are
     * being suppressed).
     *
     * <p>E.g. for {@code Page<ApiResponse<User>>}: collects the transformed name of
     * {@code UserResponsePage} and the transformed name of {@code UserResponse}.</p>
     */
    private void collectSuppressedImports(GenericSchemaScanUtils.GenericInstance inst,
                                          Context ctx, Set<String> result, Set<String> visited) {
        if (!visited.add(inst.schemaName)) return;
        result.add(ctx.toModelName(inst.schemaName));
        for (String rawTypeArg : inst.typeArgs.values()) {
            String transformed = ctx.toModelName(rawTypeArg);
            GenericSchemaScanUtils.GenericInstance nestedInst = instanceRegistry.get(transformed);
            if (nestedInst != null) {
                collectSuppressedImports(nestedInst, ctx, result, visited);
            }
        }
    }

    // =========================================================================
    // Property-level substitution
    // =========================================================================

    /**
     * Substitutes generic-instance schema references in all model properties.
     *
     * <p>Iterates all unique property instances across {@code vars}, {@code requiredVars},
     * {@code optionalVars}, and {@code allVars} (Kotlin Spring templates render from
     * {@code requiredVars}/{@code optionalVars}; Java Spring from {@code vars};
     * {@code CodegenModel.removeAllDuplicatedProperty()} clones each list independently so
     * the lists hold different instances). When a property's {@code baseType} or
     * {@code complexType} matches a registry key, its type strings are rewritten to the
     * fully-expanded generic form and the model's import sets are updated accordingly.</p>
     *
     * <p>Only called when {@code annotationLibrary=none} (already gated by the caller).</p>
     */
    private void substitutePropertyTypes(Map<String, ModelsMap> objs, Context ctx) {
        for (ModelsMap modelsMap : objs.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();

                // Collect all unique property instances across all property lists.
                // Kotlin Spring templates render from requiredVars/optionalVars, Java Spring from vars.
                // CodegenModel.removeAllDuplicatedProperty() clones each list independently, so vars
                // and optionalVars/requiredVars hold DIFFERENT instances for the same property.
                // Using IdentityHashMap ensures each physical instance is processed exactly once.
                Set<CodegenProperty> allProps = Collections.newSetFromMap(new IdentityHashMap<>());
                allProps.addAll(model.vars);
                allProps.addAll(model.requiredVars);
                allProps.addAll(model.optionalVars);
                allProps.addAll(model.allVars);

                Set<String> removedFromImports = new HashSet<>();
                Set<String> addedToImports = new HashSet<>();

                for (CodegenProperty prop : allProps) {
                    // For plain model-ref properties the instance name is in baseType.
                    // For array/container properties baseType = "List" and the item type is in complexType.
                    String lookupKey = prop.baseType;
                    GenericSchemaScanUtils.GenericInstance inst =
                            lookupKey != null ? instanceRegistry.get(lookupKey) : null;
                    final boolean usingComplexTypeFallback;
                    if (inst == null && prop.complexType != null
                            && !prop.complexType.equals(prop.baseType)) {
                        lookupKey = prop.complexType;
                        inst = instanceRegistry.get(lookupKey);
                        usingComplexTypeFallback = inst != null;
                    } else {
                        usingComplexTypeFallback = false;
                    }
                    if (inst == null) continue;

                    String newGenericType = buildGenericTypeName(inst, ctx, new HashSet<>());

                    // Replace all occurrences of the old type in the type strings.
                    // Handles plain "UserResponse", "List<UserResponse>", nullable "UserResponse?" etc.
                    //
                    // Substring-collision invariant: this relies on lookupKey not being a substring
                    // of any other registry key whose property might land in the same dataType
                    // (e.g. registering both "User" and "UserPage" as generic instances would
                    // corrupt a dataType of "UserPage"). In practice schema names are full identifiers
                    // — there is no realistic OpenAPI spec where two distinct generic-instance schema
                    // names exhibit this prefix relationship and reference each other in one property.
                    prop.dataType = prop.dataType.replace(lookupKey, newGenericType);
                    if (prop.datatypeWithEnum != null) {
                        prop.datatypeWithEnum = prop.datatypeWithEnum.replace(lookupKey, newGenericType);
                    }
                    // Update baseType only when it was the matched key (not for array container types).
                    if (!usingComplexTypeFallback) {
                        prop.baseType = inst.genericClassName;
                    }
                    if (usingComplexTypeFallback || lookupKey.equals(prop.complexType)) {
                        prop.complexType = inst.genericClassName;
                        // Also update prop.items for array/map properties:
                        // templates like pojo.mustache use items.datatypeWithEnum for addXxxItem methods.
                        if (prop.items != null) {
                            prop.items.dataType = prop.items.dataType.replace(lookupKey, newGenericType);
                            if (prop.items.datatypeWithEnum != null) {
                                prop.items.datatypeWithEnum =
                                        prop.items.datatypeWithEnum.replace(lookupKey, newGenericType);
                            }
                            if (lookupKey.equals(prop.items.baseType)) {
                                prop.items.baseType = inst.genericClassName;
                            }
                            if (lookupKey.equals(prop.items.complexType)) {
                                prop.items.complexType = inst.genericClassName;
                            }
                        }
                    }

                    // Update model-level imports Set<String> and track changes for List sync below.
                    if (model.imports.remove(lookupKey)) {
                        removedFromImports.add(lookupKey);
                    }
                    Set<String> toAdd = new HashSet<>();
                    collectImportsToAdd(inst, ctx, toAdd, new HashSet<>());
                    for (String cn : toAdd) {
                        if (model.imports.add(cn)) {
                            addedToImports.add(cn);
                        }
                    }

                    LOGGER.info("GenericSubstitutionSupport: model '{}' property '{}': "
                                    + "substituted '{}' → '{}'",
                            model.name, prop.name, lookupKey, newGenericType);
                }

                // Synchronize ModelsMap.imports (List<Map<String,String>>) with the updated
                // model.imports Set. DefaultGenerator builds this List before postProcessAllModels,
                // so it must be updated here to ensure templates see the correct import statements.
                if (!removedFromImports.isEmpty() || !addedToImports.isEmpty()) {
                    syncModelsMapImports(modelsMap, removedFromImports, addedToImports, ctx);
                }
            }
        }
    }

    /**
     * Synchronizes the pre-built {@code ModelsMap.imports} List with changes made to
     * {@code model.imports} during property-type substitution.
     *
     * <p>Removes FQN entries whose simple class name is in {@code removed} and adds
     * FQN entries for simple class names in {@code added} that are not yet present.</p>
     */
    private void syncModelsMapImports(ModelsMap modelsMap, Set<String> removed,
                                      Set<String> added, Context ctx) {
        List<Map<String, String>> importsList = modelsMap.getImports();
        if (importsList == null) return;

        // Remove entries for types that were substituted away.
        for (String simpleName : removed) {
            final String suffix = "." + simpleName;
            importsList.removeIf(imp -> {
                String fqn = imp.get("import");
                return fqn != null && (fqn.endsWith(suffix) || fqn.equals(simpleName));
            });
        }

        // Add entries for newly required types.
        Set<String> existingFqns = new HashSet<>();
        for (Map<String, String> imp : importsList) {
            String fqn = imp.get("import");
            if (fqn != null) existingFqns.add(fqn);
        }
        for (String simpleName : added) {
            String fqn = ctx.importMapping().get(simpleName);
            if (fqn == null) fqn = ctx.toModelImport(simpleName);
            if (fqn != null && !existingFqns.contains(fqn)) {
                Map<String, String> entry = new HashMap<>();
                entry.put("import", fqn);
                importsList.add(entry);
                existingFqns.add(fqn);
            }
        }
    }

    /**
     * Returns {@code true} if any model in {@code objs} still references the given
     * transformed instance name — either via a property {@code baseType} / {@code complexType}
     * that was not substituted (checked across {@code vars}, {@code requiredVars},
     * {@code optionalVars}, and {@code allVars} since {@code removeAllDuplicatedProperty()}
     * gives each list independent property instances), via {@code model.parent}
     * (allOf inheritance), or via the model's imports set.
     *
     * <p>This is used as a suppression safety check to avoid deleting a class that is
     * still needed (e.g. as a base class for another model).</p>
     */
    private boolean isStillReferenced(String transformedKey, Map<String, ModelsMap> objs) {
        for (ModelsMap modelsMap : objs.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (transformedKey.equals(model.parent)) {
                    return true;
                }
                if (model.imports != null && model.imports.contains(transformedKey)) {
                    return true;
                }
                Set<CodegenProperty> allProps = Collections.newSetFromMap(new IdentityHashMap<>());
                allProps.addAll(model.vars);
                allProps.addAll(model.requiredVars);
                allProps.addAll(model.optionalVars);
                allProps.addAll(model.allVars);
                for (CodegenProperty prop : allProps) {
                    if (transformedKey.equals(prop.baseType)
                            || transformedKey.equals(prop.complexType)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns the number of detected generic instances (for testing).
     */
    public int instanceCount() {
        return instanceRegistry.size();
    }

    /**
     * Returns the instance registry (for testing / inspection).
     */
    public Map<String, GenericSchemaScanUtils.GenericInstance> getInstanceRegistry() {
        return Collections.unmodifiableMap(instanceRegistry);
    }
}
