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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Language-agnostic utility for detecting OpenAPI schemas that can be replaced by a single
 * generic class ({@code ApiResponse<T>}, {@code Page<T>}, …) during code generation.
 *
 * <p>Three detection tiers are supported:</p>
 * <ol>
 *   <li><b>Tier 1 — Vendor extensions</b>: schema carries {@code x-generic-class} and
 *       {@code x-generic-args} extensions (requires spec modification).</li>
 *   <li><b>Tier 2 — Suffix / prefix patterns</b>: schemas whose name matches a configured
 *       suffix or prefix pattern (requires only generator config, not spec changes).</li>
 *   <li><b>Tier 3 — Structural clustering</b>: schemas with the same structure except for
 *       one varying {@code $ref} property are clustered and logged as suggestions.
 *       This tier never auto-applies substitution.</li>
 * </ol>
 *
 * <p>Used by {@link GenericSubstitutionSupport} which holds the stateful result
 * and coordinates code generation lifecycle hooks.</p>
 */
public final class GenericSchemaScanUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(GenericSchemaScanUtils.class);

    /** Type parameter name sequence used when auto-assigning names by position. */
    private static final String[] TYPE_PARAM_LETTERS = {"T", "E", "U", "V", "W"};

    private GenericSchemaScanUtils() {}

    /** Returns the type parameter name for the given 0-based slot index. */
    private static String typeParamLetter(int index) {
        return index < TYPE_PARAM_LETTERS.length ? TYPE_PARAM_LETTERS[index] : "T" + index;
    }

    // =========================================================================
    // Data classes
    // =========================================================================

    /**
     * Describes a single property of a schema, used when generating the generic class source
     * in Mode B.
     */
    public static final class GenericProperty {
        /** Property name. */
        public final String name;
        /**
         * OpenAPI schema type of this property: {@code "string"}, {@code "integer"},
         * {@code "number"}, {@code "boolean"}, {@code "$ref"}, {@code "array"}, or
         * {@code "object"}. Never {@code null}.
         */
        public final String openApiType;
        /**
         * For {@code $ref} properties: the simple schema name of the referenced schema
         * (e.g. {@code "User"}). For {@code array} properties with items {@code $ref}:
         * the simple name of the items schema. {@code null} for primitives.
         */
        public final String refTarget;
        /**
         * Name of the type parameter assigned to this property (e.g. {@code "T"}).
         * Non-null only for the slot property. When non-null this property's generated
         * Java/Kotlin type will be {@code T} (or {@code List<T>} if {@code isArray}).
         */
        public final String typeParam;
        /** OpenAPI format string (e.g. {@code "int64"}, {@code "date-time"}), may be {@code null}. */
        public final String format;
        /** {@code true} if this is an array property (type=array). */
        public final boolean isArray;
        /** {@code true} if the property is listed in the schema's {@code required} list. */
        public final boolean required;

        public GenericProperty(String name, String openApiType, String refTarget,
                               String typeParam, String format, boolean isArray, boolean required) {
            this.name = name;
            this.openApiType = openApiType;
            this.refTarget = refTarget;
            this.typeParam = typeParam;
            this.format = format;
            this.isArray = isArray;
            this.required = required;
        }
    }

    /**
     * Carries the result of detecting a single generic schema instance.
     *
     * <p>Used for both return-type substitution and (in Mode B) class file generation.</p>
     */
    public static final class GenericInstance {
        /** Concrete schema name (e.g. {@code "UserResponse"}). */
        public final String schemaName;
        /** Simple class name of the generic class (e.g. {@code "ApiResponse"}). */
        public final String genericClassName;
        /**
         * Fully-qualified name of the generic class, or {@code null} if Mode B
         * (class is to be generated in the config package).
         */
        public final String genericClassFqn;
        /**
         * {@code true} if the generic class does not yet exist and should be generated
         * (Mode B). {@code false} if it is an external class to be imported (Mode A).
         */
        public final boolean generateClass;
        /**
         * Maps slot property name to resolved type-argument schema name.
         * E.g. {@code {"data" -> "User"}} (single-param) or
         * {@code {"data" -> "User", "error" -> "ValidationError"}} (multi-param).
         */
        public final Map<String, String> typeArgs;
        /**
         * Maps slot property name to type parameter name (e.g. {@code "T"}, {@code "E"}).
         * Same key set and insertion order as {@link #typeArgs}.
         * E.g. {@code {"data" -> "T"}} or {@code {"data" -> "T", "error" -> "E"}}.
         */
        public final Map<String, String> slotTypeParams;
        /**
         * The name of the primary (first) slot property.
         */
        public final String slotProperty;
        /**
         * Whether the primary slot property is an array property, meaning the
         * generated type will be {@code List<T>} rather than {@code T}.
         */
        public final boolean slotIsArray;
        /**
         * All properties of the matched schema, with slot properties having their
         * respective {@code typeParam} set. Used for Mode B class generation.
         */
        public final List<GenericProperty> properties;

        public GenericInstance(String schemaName, String genericClassName, String genericClassFqn,
                               boolean generateClass, Map<String, String> typeArgs,
                               Map<String, String> slotTypeParams,
                               String slotProperty, boolean slotIsArray,
                               List<GenericProperty> properties) {
            this.schemaName = schemaName;
            this.genericClassName = genericClassName;
            this.genericClassFqn = genericClassFqn;
            this.generateClass = generateClass;
            this.typeArgs = Collections.unmodifiableMap(typeArgs);
            this.slotTypeParams = Collections.unmodifiableMap(slotTypeParams);
            this.slotProperty = slotProperty;
            this.slotIsArray = slotIsArray;
            this.properties = Collections.unmodifiableList(properties);
        }

        /**
         * Returns the type argument for the first (and usually only) slot.
         * E.g. {@code "User"} for a {@code UserResponse} matched by slot {@code "data"}.
         */
        public String firstTypeArg() {
            return typeArgs.values().iterator().next();
        }
    }

    /**
     * A suggestion produced by Tier 3 structural clustering.
     * Never auto-applied; only logged to guide the user in configuring Tier 2 patterns.
     */
    public static final class ClusterSuggestion {
        /** Names of schemas in the cluster (e.g. {@code ["LogEntry", "MetricsEntry"]}). */
        public final List<String> schemaNames;
        /** Property name that varies between cluster members (the candidate slot). */
        public final String varyingSlotProperty;
        /** $ref target names found across cluster members for the varying property. */
        public final List<String> varyingTypes;
        /** A ready-to-paste YAML snippet for a Tier 2 genericPatterns entry. */
        public final String suggestedConfig;

        public ClusterSuggestion(List<String> schemaNames, String varyingSlotProperty,
                                 List<String> varyingTypes, String suggestedConfig) {
            this.schemaNames = Collections.unmodifiableList(schemaNames);
            this.varyingSlotProperty = varyingSlotProperty;
            this.varyingTypes = Collections.unmodifiableList(varyingTypes);
            this.suggestedConfig = suggestedConfig;
        }
    }

    // =========================================================================
    // Tier 1 — Vendor extension scanning
    // =========================================================================

    /**
     * Scans all named schemas for {@code x-generic-class} / {@code x-generic-args} vendor
     * extensions (Tier 1) and returns one {@link GenericInstance} per decorated schema.
     *
     * @param openAPI the parsed OpenAPI document
     * @return list of detected instances; empty if none found
     */
    public static List<GenericInstance> scanVendorExtensions(OpenAPI openAPI) {
        List<GenericInstance> result = new ArrayList<>();
        if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null) {
            return result;
        }

        for (Map.Entry<String, Schema> entry : openAPI.getComponents().getSchemas().entrySet()) {
            String schemaName = entry.getKey();
            Schema<?> schema = entry.getValue();
            if (schema.getExtensions() == null) {
                continue;
            }

            Object classExt = schema.getExtensions().get("x-generic-class");
            if (!(classExt instanceof String) || ((String) classExt).isEmpty()) {
                continue;
            }

            String genericClassValue = (String) classExt;

            // Parse x-generic-args: should be a Map<String, String>
            Object argsExt = schema.getExtensions().get("x-generic-args");
            Map<String, String> typeArgs = new LinkedHashMap<>();
            if (argsExt instanceof Map) {
                for (Map.Entry<?, ?> argEntry : ((Map<?, ?>) argsExt).entrySet()) {
                    typeArgs.put(String.valueOf(argEntry.getKey()), String.valueOf(argEntry.getValue()));
                }
            }

            if (typeArgs.isEmpty()) {
                LOGGER.warn("GenericSchemaScanUtils: schema '{}' has x-generic-class '{}' but no "
                        + "x-generic-args — skipping", schemaName, genericClassValue);
                continue;
            }

            // Identify slot properties and assign type param names by position (T, E, U, ...)
            String slotProperty = typeArgs.keySet().iterator().next();
            boolean slotIsArray = false;
            Map<String, String> slotTypeParams = new LinkedHashMap<>();
            Map<String, Schema> props = resolveProperties(schema, openAPI);
            int tpIndex = 0;
            for (String propName : typeArgs.keySet()) {
                slotTypeParams.put(propName, typeParamLetter(tpIndex++));
            }
            if (props != null) {
                Schema<?> slotSchema = (Schema<?>) props.get(slotProperty);
                if (slotSchema != null && "array".equals(slotSchema.getType())) {
                    slotIsArray = true;
                }
            }

            boolean isFqn = genericClassValue.contains(".");
            String genericClassName = isFqn
                    ? genericClassValue.substring(genericClassValue.lastIndexOf('.') + 1)
                    : genericClassValue;

            List<GenericProperty> properties = buildProperties(schema, openAPI, slotTypeParams);

            result.add(new GenericInstance(
                    schemaName, genericClassName,
                    isFqn ? genericClassValue : null,
                    !isFqn,
                    typeArgs, slotTypeParams, slotProperty, slotIsArray, properties));

            LOGGER.debug("GenericSchemaScanUtils Tier1: schema '{}' → {}{}",
                    schemaName, genericClassName,
                    typeArgs.entrySet().stream()
                            .map(e -> "<" + e.getValue() + ">")
                            .collect(Collectors.joining()));
        }
        return result;
    }

    // =========================================================================
    // Tier 2 — Config suffix / prefix pattern scanning
    // =========================================================================

    /**
     * Scans all named schemas against the provided {@link GenericPatternConfig} list
     * (Tier 2) and returns one {@link GenericInstance} per matched schema.
     *
     * <p>A schema matches a pattern when:
     * <ul>
     *   <li>its name ends with {@link GenericPatternConfig#suffix} (case-sensitive), or</li>
     *   <li>its name starts with {@link GenericPatternConfig#prefix} (case-sensitive),</li>
     * </ul>
     * AND the schema has the expected slot property ({@link GenericPatternConfig#slot}) or
     * slotArray property ({@link GenericPatternConfig#slotArray}) with a {@code $ref} or
     * array-of-{@code $ref} type respectively.</p>
     *
     * <p>Schemas already matched by Tier 1 (i.e. in {@code tier1SchemaNames}) are skipped.</p>
     *
     * @param openAPI         the parsed OpenAPI document
     * @param patterns        list of patterns to match against
     * @param tier1SchemaNames schema names already handled by Tier 1 (excluded from matching)
     * @return list of detected instances; empty if none found or no patterns provided
     */
    public static List<GenericInstance> scanWithPatterns(OpenAPI openAPI,
                                                          List<GenericPatternConfig> patterns,
                                                          Set<String> tier1SchemaNames) {
        List<GenericInstance> result = new ArrayList<>();
        if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null
                || patterns == null || patterns.isEmpty()) {
            return result;
        }

        for (Map.Entry<String, Schema> schemaEntry : openAPI.getComponents().getSchemas().entrySet()) {
            String schemaName = schemaEntry.getKey();
            if (tier1SchemaNames.contains(schemaName)) {
                continue;
            }
            Schema<?> schema = schemaEntry.getValue();

            for (GenericPatternConfig pattern : patterns) {
                if (pattern.genericClass == null || pattern.genericClass.isEmpty()) {
                    LOGGER.warn("GenericSchemaScanUtils Tier2: pattern has no genericClass — skipping: {}",
                            pattern);
                    continue;
                }
                if (!matchesPattern(schemaName, pattern)) {
                    continue;
                }

                // Determine slots: use pattern.slots if set, else normalize slot/slotArray
                Map<String, String> effectiveSlots = null;
                if (pattern.slots != null && !pattern.slots.isEmpty()) {
                    effectiveSlots = pattern.slots;
                } else if (pattern.slot != null && !pattern.slot.isEmpty()) {
                    effectiveSlots = Collections.singletonMap(pattern.slot, "T");
                } else if (pattern.slotArray != null && !pattern.slotArray.isEmpty()) {
                    effectiveSlots = Collections.singletonMap(pattern.slotArray, "T");
                }

                if (effectiveSlots == null) {
                    LOGGER.warn("GenericSchemaScanUtils Tier2: pattern has no slot/slotArray/slots — skipping: {}",
                            pattern);
                    continue;
                }

                Map<String, Schema> props = resolveProperties(schema, openAPI);

                // Resolve each configured slot to its type arg schema name
                Map<String, String> typeArgs = new LinkedHashMap<>();
                Map<String, String> slotTypeParams = new LinkedHashMap<>();
                String primarySlotName = null;
                boolean primarySlotIsArray = false;
                boolean allSlotsFound = true;

                for (Map.Entry<String, String> slotEntry : effectiveSlots.entrySet()) {
                    String slotPropName = slotEntry.getKey();
                    String typeParamName = slotEntry.getValue();

                    // Try $ref slot first
                    String ref = findRefInProperties(props, slotPropName, openAPI);
                    if (ref != null) {
                        typeArgs.put(slotPropName, extractSchemaNameFromRef(ref));
                        slotTypeParams.put(slotPropName, typeParamName);
                        if (primarySlotName == null) {
                            primarySlotName = slotPropName;
                            primarySlotIsArray = false;
                        }
                        continue;
                    }

                    // Try array slot
                    String arrayRef = findArrayItemRefInProperties(props, schema, slotPropName, openAPI);
                    if (arrayRef != null) {
                        typeArgs.put(slotPropName, extractSchemaNameFromRef(arrayRef));
                        slotTypeParams.put(slotPropName, typeParamName);
                        if (primarySlotName == null) {
                            primarySlotName = slotPropName;
                            primarySlotIsArray = true;
                        }
                        continue;
                    }

                    LOGGER.debug("GenericSchemaScanUtils Tier2: schema '{}' matched pattern '{}' by name "
                            + "but slot '{}' not found or not a $ref — skipping",
                            schemaName, pattern, slotPropName);
                    allSlotsFound = false;
                    break;
                }

                if (!allSlotsFound || primarySlotName == null) {
                    continue;
                }

                boolean isFqn = pattern.genericClass.contains(".");
                String genericClassName = isFqn
                        ? pattern.genericClass.substring(pattern.genericClass.lastIndexOf('.') + 1)
                        : pattern.genericClass;

                List<GenericProperty> properties = buildProperties(schema, openAPI, slotTypeParams);

                result.add(new GenericInstance(
                        schemaName, genericClassName,
                        isFqn ? pattern.genericClass : null,
                        !isFqn,
                        typeArgs, slotTypeParams, primarySlotName, primarySlotIsArray, properties));

                LOGGER.debug("GenericSchemaScanUtils Tier2: schema '{}' matched pattern '{}' → {}<{}>",
                        schemaName, pattern.suffix != null ? ("suffix=" + pattern.suffix) : ("prefix=" + pattern.prefix),
                        genericClassName, typeArgs.values());
                break; // first matching pattern wins
            }
        }
        return result;
    }

    // =========================================================================
    // Tier 3 — Structural cluster discovery
    // =========================================================================

    /**
     * Scans all named schemas looking for structural clusters: groups of 2 or more schemas
     * that have the same property names and types except for exactly one {@code $ref} property
     * which varies across members.
     *
     * <p>This is <b>discovery-only</b>: no substitution is performed. The suggestions are
     * returned (and typically logged by the caller) to help the user configure Tier 2
     * patterns.</p>
     *
     * @param openAPI             the parsed OpenAPI document
     * @param excludedSchemaNames schema names to exclude (e.g. already handled by Tier 1/2)
     * @return list of cluster suggestions; empty if none found
     */
    public static List<ClusterSuggestion> discoverClusters(OpenAPI openAPI,
                                                             Set<String> excludedSchemaNames) {
        List<ClusterSuggestion> result = new ArrayList<>();
        if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null) {
            return result;
        }

        Map<String, Schema> allSchemas = openAPI.getComponents().getSchemas();

        // Build fingerprint → list of schema names
        Map<String, List<String>> byFingerprint = new LinkedHashMap<>();
        for (Map.Entry<String, Schema> entry : allSchemas.entrySet()) {
            String name = entry.getKey();
            if (excludedSchemaNames.contains(name)) {
                continue;
            }
            Schema<?> schema = entry.getValue();
            String fp = buildStructuralFingerprint(schema);
            if (fp == null) {
                continue; // allOf or no properties
            }
            byFingerprint.computeIfAbsent(fp, k -> new ArrayList<>()).add(name);
        }

        // For each group of 2+, look for the varying $ref property
        for (Map.Entry<String, List<String>> fpEntry : byFingerprint.entrySet()) {
            List<String> names = fpEntry.getValue();
            if (names.size() < 2) {
                continue;
            }

            // Find the property whose $ref target differs across all members
            String varyingProp = findVaryingRefProperty(names, allSchemas);
            if (varyingProp == null) {
                continue;
            }

            // Collect all varying types
            List<String> varyingTypes = new ArrayList<>();
            for (String name : names) {
                Schema<?> schema = allSchemas.get(name);
                if (schema.getProperties() == null) continue;
                Schema<?> prop = (Schema<?>) schema.getProperties().get(varyingProp);
                if (prop != null && prop.get$ref() != null) {
                    varyingTypes.add(extractSchemaNameFromRef(prop.get$ref()));
                }
            }

            // Determine the most likely common suffix for the suggestion
            String suggestedSuffix = commonSuffix(names);
            String suggestedConfig = buildSuggestedConfig(suggestedSuffix, varyingProp, names);

            result.add(new ClusterSuggestion(new ArrayList<>(names), varyingProp,
                    varyingTypes, suggestedConfig));
        }
        return result;
    }

    // =========================================================================
    // Private helpers — pattern matching
    // =========================================================================

    static boolean matchesPattern(String schemaName, GenericPatternConfig pattern) {
        if (pattern.suffix != null && !pattern.suffix.isEmpty()) {
            return schemaName.endsWith(pattern.suffix) && schemaName.length() > pattern.suffix.length();
        }
        if (pattern.prefix != null && !pattern.prefix.isEmpty()) {
            return schemaName.startsWith(pattern.prefix) && schemaName.length() > pattern.prefix.length();
        }
        return false;
    }

    // =========================================================================
    // Private helpers — property resolution
    // =========================================================================

    /**
     * Returns the merged properties of a schema, handling both flat-object and allOf forms.
     * Returns {@code null} if the schema has no resolvable properties.
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    static Map<String, Schema> resolveProperties(Schema<?> schema, OpenAPI openAPI) {
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            return (Map<String, Schema>) schema.getProperties();
        }
        // Handle allOf: merge inline object properties from allOf entries
        if (schema.getAllOf() != null) {
            Map<String, Schema> merged = new LinkedHashMap<>();
            for (Object entryObj : schema.getAllOf()) {
                if (!(entryObj instanceof Schema)) continue;
                Schema entry = (Schema) entryObj;
                if (entry.getProperties() != null) {
                    merged.putAll(entry.getProperties());
                }
            }
            return merged.isEmpty() ? null : merged;
        }
        return null;
    }

    /**
     * Finds the {@code $ref} string of a property in the resolved properties.
     * Returns {@code null} if the property is absent or is not a {@code $ref}.
     */
    @SuppressWarnings("rawtypes")
    private static String findRefInProperties(Map<String, Schema> props, String propName,
                                              OpenAPI openAPI) {
        if (props == null) return null;
        Schema<?> prop = (Schema<?>) props.get(propName);
        if (prop == null) return null;
        if (prop.get$ref() != null) return prop.get$ref();
        // Might be inlined — check via ModelUtils to handle $ref resolution
        Schema<?> resolved = ModelUtils.getReferencedSchema(openAPI, prop);
        return resolved != null && resolved != prop ? prop.get$ref() : null;
    }

    /**
     * Finds the {@code $ref} of array items for the given property name.
     * Searches both the top-level (flat-object) and allOf inline objects.
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    private static String findArrayItemRefInProperties(Map<String, Schema> props, Schema<?> schema,
                                                       String slotName, OpenAPI openAPI) {
        // Top-level properties
        if (props != null) {
            Schema<?> slotProp = (Schema<?>) props.get(slotName);
            if (slotProp != null) {
                String ref = extractArrayItemRef(slotProp);
                if (ref != null) return ref;
            }
        }
        // allOf inline entries
        if (schema.getAllOf() != null) {
            for (Object entryObj : schema.getAllOf()) {
                if (!(entryObj instanceof Schema)) continue;
                Schema entry = (Schema) entryObj;
                if (entry.getProperties() == null) continue;
                Schema<?> slotProp = (Schema<?>) entry.getProperties().get(slotName);
                if (slotProp != null) {
                    String ref = extractArrayItemRef(slotProp);
                    if (ref != null) return ref;
                }
            }
        }
        return null;
    }

    @SuppressWarnings("rawtypes")
    private static String extractArrayItemRef(Schema<?> schema) {
        if (schema == null) return null;
        if (!"array".equals(schema.getType())) return null;
        Schema items = schema.getItems();
        return items != null ? items.get$ref() : null;
    }

    // =========================================================================
    // Private helpers — property list building for class generation
    // =========================================================================

    /**
     * Builds the full property list for a matched schema, marking slot properties with
     * their respective type parameters from {@code slotTypeParams}.
     * Array-ness is auto-detected from each property schema type.
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    private static List<GenericProperty> buildProperties(Schema<?> schema, OpenAPI openAPI,
                                                          Map<String, String> slotTypeParams) {
        List<GenericProperty> result = new ArrayList<>();
        Set<String> required = schema.getRequired() != null
                ? new HashSet<>(schema.getRequired()) : Collections.emptySet();

        Map<String, Schema> props = resolveProperties(schema, openAPI);
        if (props == null) return result;

        for (Map.Entry<String, Schema> entry : props.entrySet()) {
            String name = entry.getKey();
            Schema<?> propSchema = (Schema<?>) entry.getValue();
            boolean isRequired = required.contains(name);

            String typeParam = slotTypeParams.get(name);
            if (typeParam != null) {
                // Slot property — auto-detect array-ness
                boolean isSlotArray = "array".equals(propSchema.getType());
                String format = isSlotArray && propSchema.getItems() != null
                        ? propSchema.getItems().getFormat() : propSchema.getFormat();
                result.add(new GenericProperty(name, isSlotArray ? "array" : "$ref",
                        null, typeParam, format, isSlotArray, isRequired));
            } else {
                result.add(buildNonSlotProperty(name, propSchema, isRequired, openAPI));
            }
        }
        return result;
    }

    @SuppressWarnings("rawtypes")
    private static GenericProperty buildNonSlotProperty(String name, Schema<?> propSchema,
                                                         boolean required, OpenAPI openAPI) {
        if (propSchema.get$ref() != null) {
            String target = extractSchemaNameFromRef(propSchema.get$ref());
            return new GenericProperty(name, "$ref", target, null,
                    propSchema.getFormat(), false, required);
        }
        if ("array".equals(propSchema.getType())) {
            String itemRef = null;
            if (propSchema.getItems() != null && propSchema.getItems().get$ref() != null) {
                itemRef = extractSchemaNameFromRef(propSchema.getItems().get$ref());
            }
            String itemType = itemRef != null ? itemRef
                    : (propSchema.getItems() != null ? propSchema.getItems().getType() : "Object");
            return new GenericProperty(name, "array", itemType, null,
                    propSchema.getFormat(), true, required);
        }
        String type = propSchema.getType() != null ? propSchema.getType() : "object";
        return new GenericProperty(name, type, null, null, propSchema.getFormat(), false, required);
    }

    // =========================================================================
    // Private helpers — structural fingerprinting (Tier 3)
    // =========================================================================

    /**
     * Builds a canonical structural fingerprint for a flat-object schema.
     * Returns {@code null} if the schema is an allOf or has no properties.
     *
     * <p>The fingerprint encodes each property as {@code "name:typeDescriptor"} where
     * the type descriptor for {@code $ref} properties is just {@code "$ref"} (ignoring
     * the target). This allows grouping of structurally identical schemas that differ only
     * in their {@code $ref} targets.</p>
     */
    @SuppressWarnings("rawtypes")
    static String buildStructuralFingerprint(Schema<?> schema) {
        if (schema.getAllOf() != null || schema.getProperties() == null
                || schema.getProperties().isEmpty()) {
            return null;
        }
        return schema.getProperties().entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(e -> e.getKey() + ":" + propertyTypeDescriptor((Schema<?>) e.getValue()))
                .collect(Collectors.joining("|"));
    }

    @SuppressWarnings("rawtypes")
    private static String propertyTypeDescriptor(Schema<?> prop) {
        if (prop == null) return "null";
        if (prop.get$ref() != null) return "$ref";
        if ("array".equals(prop.getType())) {
            if (prop.getItems() != null) {
                if (prop.getItems().get$ref() != null) return "array[$ref]";
                return "array[" + prop.getItems().getType() + "]";
            }
            return "array[?]";
        }
        return prop.getType() != null ? prop.getType() : "object";
    }

    /**
     * Finds the name of the property whose {@code $ref} target varies across all schemas
     * in the group (all other {@code $ref} properties must be identical).
     * Returns {@code null} if no such unique varying property exists.
     */
    @SuppressWarnings("rawtypes")
    private static String findVaryingRefProperty(List<String> schemaNames,
                                                  Map<String, Schema> allSchemas) {
        if (schemaNames.isEmpty()) return null;
        Schema<?> first = allSchemas.get(schemaNames.get(0));
        if (first.getProperties() == null) return null;

        List<String> candidates = new ArrayList<>();
        for (Map.Entry<?, ?> entry : first.getProperties().entrySet()) {
            Schema<?> prop = (Schema<?>) entry.getValue();
            if (prop.get$ref() == null) continue;
            candidates.add((String) entry.getKey());
        }

        String varyingProp = null;
        for (String candidate : candidates) {
            Set<String> refs = new HashSet<>();
            for (String name : schemaNames) {
                Schema<?> schema = allSchemas.get(name);
                if (schema.getProperties() == null) { refs.add(null); break; }
                Schema<?> prop = (Schema<?>) schema.getProperties().get(candidate);
                refs.add(prop != null ? prop.get$ref() : null);
            }
            if (refs.size() == schemaNames.size()) {
                // All members have this property, all different
                if (varyingProp != null) {
                    return null; // more than one varying property — not a simple generic
                }
                varyingProp = candidate;
            }
        }
        return varyingProp;
    }

    private static String commonSuffix(List<String> names) {
        if (names.isEmpty()) return "";
        String first = names.get(0);
        for (int len = 1; len <= first.length(); len++) {
            String suffix = first.substring(first.length() - len);
            boolean allMatch = names.stream().allMatch(n ->
                    n.endsWith(suffix) && n.length() > suffix.length());
            if (!allMatch) {
                return first.substring(first.length() - len + 1);
            }
        }
        return first;
    }

    private static String buildSuggestedConfig(String suggestedSuffix, String slotProperty,
                                                List<String> schemaNames) {
        boolean isArray = false; // Tier 3 only handles $ref properties
        String slotKey = isArray ? "slotArray" : "slot";
        return "genericPatterns:\n"
                + "  - suffix: " + suggestedSuffix + "\n"
                + "    genericClass: <your.package." + suggestedSuffix + ">\n"
                + "    " + slotKey + ": " + slotProperty + "\n"
                + "  # Schemas matched: " + String.join(", ", schemaNames);
    }

    // =========================================================================
    // Package-level utility — used by GenericSubstitutionSupport and PagedModelScanUtils
    // =========================================================================

    /**
     * Extracts the simple schema name from a {@code $ref} string such as
     * {@code #/components/schemas/User} → {@code User}, or
     * {@code ./external.yaml#/components/schemas/Order} → {@code Order}.
     */
    static String extractSchemaNameFromRef(String ref) {
        if (ref == null) return null;
        int slash = ref.lastIndexOf('/');
        return slash >= 0 ? ref.substring(slash + 1) : ref;
    }
}
