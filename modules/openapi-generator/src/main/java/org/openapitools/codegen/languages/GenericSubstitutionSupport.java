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
import org.openapitools.codegen.CodegenOperation;
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
 * <p>This class and {@link SpringPageableSupport} both perform return-type substitution
 * for generic wrapper schemas, but they are <em>complementary</em>, not redundant:</p>
 * <ul>
 *   <li><b>This class</b> ({@code genericPatterns} config) uses <em>name-based pattern
 *       matching</em> (suffix / prefix / vendor extensions). It can target any generic class
 *       with any number of type parameters ({@code slots}), but relies on schemas following a
 *       naming convention. It suppresses the matched wrapper schema but not any companion
 *       metadata schemas.</li>
 *   <li>{@link SpringPageableSupport} ({@code substituteGenericPagedModel} flag) uses
 *       <em>structural detection</em>: it identifies paged-model schemas by shape, requires no
 *       naming convention, and additionally suppresses the companion {@code PageMetadata}-style
 *       schema. It is specialised for the Spring {@code PagedModel<T>} use case.</li>
 * </ul>
 *
 * <p>When both features are active on the same spec, {@link SpringPageableSupport} runs first
 * inside {@code fromOperation}. If it replaces a return type, this class will not find the
 * original schema name in its registry (because {@code returnBaseType} has already changed),
 * so double-substitution cannot occur.</p>
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
            reKeyed.put(transformedKey, entry.getValue());
        }
        instanceRegistry.clear();
        instanceRegistry.putAll(reKeyed);

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
     * <p>Example: operation returning {@code UserResponse} becomes {@code ApiResponse<User>}.</p>
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

        // Build type args string from all slots in order
        StringBuilder typeArgsBuilder = new StringBuilder();
        for (String slotProp : inst.slotTypeParams.keySet()) {
            if (typeArgsBuilder.length() > 0) typeArgsBuilder.append(", ");
            typeArgsBuilder.append(ctx.toModelName(inst.typeArgs.get(slotProp)));
        }
        String newType = inst.genericClassName + "<" + typeArgsBuilder + ">";

        op.returnType = newType;
        op.returnBaseType = inst.genericClassName;
        op.returnContainer = null; // generic wrapper is not a container

        op.imports.add(inst.genericClassName);
        for (String resolvedSchema : inst.typeArgs.values()) {
            op.imports.add(ctx.toModelName(resolvedSchema));
        }
        if (ctx.getAnnotationLibrary() == AnnotationLibrary.NONE) {
            // Remove the wrapper schema import using its transformed name (matching op.imports
            // entries, which are already toModelName()-processed by super.fromOperation).
            op.imports.remove(ctx.toModelName(inst.schemaName));
        }

        LOGGER.info("GenericSubstitutionSupport: operation '{}': replacing return type '{}' with '{}'",
                op.operationId, oldType, newType);
    }

    // =========================================================================
    // Lifecycle 3: suppressGenericSchemas (called from postProcessAllModels)
    // =========================================================================

    /**
     * Removes concrete generic-instance schemas (e.g. {@code UserResponse},
     * {@code PetResponse}) from the model map when {@code annotationLibrary=none}.
     *
     * <p>When annotation libraries are active, {@code @ApiResponse} and {@code @Schema}
     * annotations in the generated code reference concrete schema classes, so they must
     * be kept. Only when {@code annotationLibrary=none} is it safe to suppress them.</p>
     *
     * @param objs model map as received by {@code postProcessAllModels}
     * @param ctx  callback access to the generator's state
     * @return the (possibly mutated) model map
     */
    public Map<String, ModelsMap> suppressGenericSchemas(Map<String, ModelsMap> objs, Context ctx) {
        if (instanceRegistry.isEmpty()) {
            return objs;
        }
        if (ctx.getAnnotationLibrary() != AnnotationLibrary.NONE) {
            LOGGER.info("GenericSubstitutionSupport: keeping generic-instance schemas "
                    + "(annotationLibrary={}) — @ApiResponse annotations reference them",
                    ctx.getAnnotationLibrary().toCliOptValue());
            return objs;
        }

        for (Map.Entry<String, GenericSchemaScanUtils.GenericInstance> entry
                : instanceRegistry.entrySet()) {
            GenericSchemaScanUtils.GenericInstance inst = entry.getValue();
            // objs is keyed by the raw OpenAPI schema name (DefaultGenerator uses spec keys as-is).
            // inst.schemaName is the raw spec name (toModelName() only affects the registry key).
            if (objs.remove(inst.schemaName) != null) {
                LOGGER.info("GenericSubstitutionSupport: suppressing model '{}' → {}",
                        inst.schemaName, inst.genericClassName + "<" + inst.typeArgs.values() + ">");
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
        Map<String, Object> classData = modeBBundleData.get(className);
        if (classData != null) {
            bundle.put("genericClassDef", classData);
        }
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
