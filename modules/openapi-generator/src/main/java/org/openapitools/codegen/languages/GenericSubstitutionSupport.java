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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

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
 *   <li><b>Mode B</b> ({@code genericClass} is a simple name): a {@code .java} or
 *       {@code .kt} source file is written directly to the output folder during
 *       {@code preprocessOpenAPI}. The generated class has a single type parameter
 *       {@code <T>} and mirrors the non-slot properties of the first matched schema.</li>
 * </ul>
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
         * Returns the root output folder for this generator run.
         * Used as the base path when writing Mode B source files directly.
         */
        String outputFolder();

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
         * Not used by this class currently, exposed for future extensibility.
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

    /** Tracks which Mode B class names have already had their source file written. */
    private final Set<String> generatedModeB = new HashSet<>();

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
                // Mode B: generate source file and add import mapping
                String fullPath = ctx.outputFolder() + File.separator + configPath
                        + File.separator + className + "." + ext;
                String fqn = ctx.getConfigPackage() + "." + className;
                ctx.importMapping().putIfAbsent(className, fqn);

                if (!generatedModeB.contains(className)) {
                    generatedModeB.add(className);
                    String source = "kt".equals(ext)
                            ? buildKotlinSource(inst, ctx.getConfigPackage())
                            : buildJavaSource(inst, ctx.getConfigPackage());
                    writeSourceFile(fullPath, source);
                    LOGGER.info("GenericSubstitutionSupport: generated Mode B class '{}' at {}",
                            className, fullPath);
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
        String typeArg = ctx.toModelName(inst.firstTypeArg());
        String newType = inst.genericClassName + "<" + typeArg + ">";

        op.returnType = newType;
        op.returnBaseType = inst.genericClassName;
        op.returnContainer = null; // generic wrapper is not a container

        op.imports.add(inst.genericClassName);
        op.imports.add(typeArg);
        if (ctx.getAnnotationLibrary() == AnnotationLibrary.NONE) {
            op.imports.remove(inst.schemaName);
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
            String schemaName = entry.getKey();
            GenericSchemaScanUtils.GenericInstance inst = entry.getValue();
            if (objs.remove(schemaName) != null) {
                LOGGER.info("GenericSubstitutionSupport: suppressing model '{}' → {}{}",
                        schemaName, inst.genericClassName, "<" + inst.firstTypeArg() + ">");
            }
        }
        return objs;
    }

    // =========================================================================
    // Mode B source generation — Java
    // =========================================================================

    /**
     * Generates a Java POJO source for the generic class described by {@code instance}.
     * The class has a single type parameter {@code <T>}.
     *
     * <p>Slot properties are typed {@code T} (or {@code List<T>} for array slots);
     * fixed properties use their resolved Java types.</p>
     */
    String buildJavaSource(GenericSchemaScanUtils.GenericInstance instance, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(packageName).append(";\n\n");

        // Determine imports
        boolean needsList = instance.properties.stream().anyMatch(p -> p.isArray);
        if (needsList) {
            sb.append("import java.util.List;\n");
            sb.append("\n");
        }

        sb.append("/**\n");
        sb.append(" * Generic class generated by openapi-generator from schema pattern '")
          .append(instance.genericClassName).append("'.\n");
        sb.append(" * Type parameter {@code T} is the varying domain type.\n");
        sb.append(" */\n");
        sb.append("public class ").append(instance.genericClassName).append("<T> {\n\n");

        // Fields
        for (GenericSchemaScanUtils.GenericProperty prop : instance.properties) {
            String javaType = toJavaType(prop);
            sb.append("    private ").append(javaType).append(" ").append(prop.name).append(";\n");
        }
        sb.append("\n");

        // No-args constructor
        sb.append("    public ").append(instance.genericClassName).append("() {}\n\n");

        // Getters and setters
        for (GenericSchemaScanUtils.GenericProperty prop : instance.properties) {
            String javaType = toJavaType(prop);
            String capitalName = capitalize(prop.name);
            sb.append("    public ").append(javaType).append(" get").append(capitalName)
              .append("() { return ").append(prop.name).append("; }\n\n");
            sb.append("    public ").append(instance.genericClassName).append("<T> set")
              .append(capitalName).append("(").append(javaType).append(" ").append(prop.name)
              .append(") { this.").append(prop.name).append(" = ").append(prop.name)
              .append("; return this; }\n\n");
        }

        sb.append("}\n");
        return sb.toString();
    }

    // =========================================================================
    // Mode B source generation — Kotlin
    // =========================================================================

    /**
     * Generates a Kotlin data-class source for the generic class described by
     * {@code instance}. The class has a single type parameter {@code <T>}.
     */
    String buildKotlinSource(GenericSchemaScanUtils.GenericInstance instance, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("package ").append(packageName).append("\n\n");

        sb.append("/**\n");
        sb.append(" * Generic class generated by openapi-generator from schema pattern '")
          .append(instance.genericClassName).append("'.\n");
        sb.append(" * Type parameter [T] is the varying domain type.\n");
        sb.append(" */\n");
        sb.append("data class ").append(instance.genericClassName).append("<T>(\n");

        List<GenericSchemaScanUtils.GenericProperty> props = instance.properties;
        for (int i = 0; i < props.size(); i++) {
            GenericSchemaScanUtils.GenericProperty prop = props.get(i);
            String kotlinType = toKotlinType(prop);
            boolean isLast = (i == props.size() - 1);
            sb.append("    val ").append(prop.name).append(": ").append(kotlinType);
            if (!prop.required) {
                sb.append("? = null");
            }
            if (!isLast) sb.append(",");
            sb.append("\n");
        }
        sb.append(")\n");
        return sb.toString();
    }

    // =========================================================================
    // Type mapping helpers
    // =========================================================================

    private static String toJavaType(GenericSchemaScanUtils.GenericProperty prop) {
        if (prop.typeParam != null) {
            return prop.isArray ? "List<T>" : "T";
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
            return prop.isArray ? "List<T>" : "T";
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
    // File writing helper
    // =========================================================================

    private static void writeSourceFile(String fullPath, String content) {
        try {
            Path path = Paths.get(fullPath);
            Files.createDirectories(path.getParent());
            Files.write(path, content.getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            LOGGER.error("GenericSubstitutionSupport: failed to write Mode B source file '{}': {}",
                    fullPath, e.getMessage(), e);
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
