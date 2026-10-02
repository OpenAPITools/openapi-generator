package org.openapitools.codegen.languages;


import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenDiscriminator;
import org.openapitools.codegen.utils.ModelUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Ordered composition lowering for the cpp-boost-beast client: builds the
 * per-schema {@link CompositionDescriptor}s (branch scan surfaces, validator
 * ids, null capability, discriminator) before model processing, computes
 * recursive allOf intersections into synthetic schemas, and lowers composed
 * branch sets to C++ storage types (std::optional / std::variant /
 * CompositionBranchValue / boost::json::value) per the ordered rules.
 *
 * <p>The descriptor records ({@link CompositionDescriptor} and friends) are
 * public nested types so tests and template-facing maps keep using the same
 * accessor surface. The class is deliberately stateless: every method takes
 * its inputs as parameters, incl. the parsed {@code openAPI} document and the
 * component-schema index; the codegen keeps the compositionDescriptors /
 * allOfIntersections indexes and the model-phase consumers
 * (processComposedModelFromDescriptor, fromModel, template maps) — those
 * mutate CodegenModel state and stay on the generator.
 *
 * <p>Branch surfaces are scanned by
 * {@link Oas31SchemaSurfaceAssertionScanner#scanSurfaceAssertions}; exceptions
 * (UnsupportedSchemaAssertionException, AllOfRequiredUnsatisfiableException)
 * live on the generator and are referenced through it.
 */
public final class Oas31CompositionLowering {

    private Oas31CompositionLowering() {
    }

    /**
     * Describes a composed schema (oneOf, anyOf, allOf) with its branches,
     * preserving the original keyword and branch order after normalization.
     */
    public static final class CompositionDescriptor {
        private final String schemaName;
        private final String schemaLocation;
        private final String keyword;
        private final List<CompositionBranchDescriptor> branches;
        private final DiscriminatorDescriptor discriminator;

        public CompositionDescriptor(String schemaName, String schemaLocation,
                                     String keyword,
                                     List<CompositionBranchDescriptor> branches,
                                     DiscriminatorDescriptor discriminator) {
            this.schemaName = schemaName;
            this.schemaLocation = schemaLocation;
            this.keyword = keyword;
            this.branches = Collections.unmodifiableList(
                    new ArrayList<>(branches));
            this.discriminator = discriminator;
        }

        public String getSchemaName() { return schemaName; }
        public String getSchemaLocation() { return schemaLocation; }
        public String getKeyword() { return keyword; }
        public List<CompositionBranchDescriptor> getBranches() { return branches; }
        public DiscriminatorDescriptor getDiscriminator() { return discriminator; }
        public boolean hasDiscriminator() { return discriminator != null; }

        /** Converts this descriptor to a template-safe map for Mustache. */
        public Map<String, Object> toTemplateMap() {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("schema-name", schemaName);
            map.put("schema-location", schemaLocation);
            map.put("keyword", keyword);
            List<Map<String, Object>> branchMaps = new ArrayList<>();
            for (CompositionBranchDescriptor branch : branches) {
                branchMaps.add(branch.toTemplateMap());
            }
            map.put("branches", branchMaps);
            if (discriminator != null) {
                map.put("discriminator-property-name", discriminator.getPropertyName());
                map.put("discriminator-mapping", discriminator.getMapping());
            }
            return map;
        }
    }

    /**
     * Describes an optional discriminator on a composed schema.
     */
    public static final class DiscriminatorDescriptor {
        private final String propertyName;
        private final Map<String, String> mapping;

        public DiscriminatorDescriptor(String propertyName, Map<String, String> mapping) {
            this.propertyName = propertyName;
            this.mapping = mapping != null
                    ? Collections.unmodifiableMap(new LinkedHashMap<>(mapping))
                    : Collections.emptyMap();
        }

        public String getPropertyName() { return propertyName; }
        public Map<String, String> getMapping() { return mapping; }
    }

    /**
     * Describes a single branch within a composed schema.
     * Captures branch index, resolved schema reference, C++ storage type,
     * validator identity, null capability, assertion metadata, and
     * validation parameter values.
     *
     * <p>{@code storageCppType} is populated after storage selection.
     * {@code validatorId} identifies the generated {@code validate_<id>()} function.
     *
     * <p>Validation parameters ({@code validateParams}) carry the actual
     * assertion values (min, max, minLength, etc.) from the source schema
     * so Mustache templates can generate per-branch validators without
     * re-scanning the schema tree.
     */
    public static final class CompositionBranchDescriptor {
        private final int branchIndex;
        private final String sourceSchemaRef;
        private final String resolvedSchemaName;
        /** C++ storage type selected after descriptor construction. */
        private final String storageCppType;
        /** Stable generated validator identity. */
        private final String validatorId;
        private final NullCapability nullCapability;
        private final List<String> supportedAssertions;
        private final List<String> unsupportedAssertions;
        /**
         * Validation parameter values for Mustache template consumption.
         * Keys: "validation-type", "validation-enum-values",
         * "validation-min", "validation-max", "validation-exclusive-min",
         * "validation-exclusive-max", "validation-multiple-of",
         * "validation-min-length", "validation-max-length",
         * "validation-pattern", "validation-min-items",
         * "validation-max-items", "validation-unique-items",
         * "validation-min-properties", "validation-max-properties",
         * "validation-required".
         * Values are Objects (String, Number, Boolean, List<String>).
         */
        private final Map<String, Object> validateParams;

        public enum NullCapability { NEVER, ALWAYS, CONDITIONAL }

        public CompositionBranchDescriptor(int branchIndex, String sourceSchemaRef,
                                           String resolvedSchemaName, String storageCppType,
                                           String validatorId, NullCapability nullCapability,
                                           List<String> supportedAssertions,
                                           List<String> unsupportedAssertions,
                                           Map<String, Object> validateParams) {
            this.branchIndex = branchIndex;
            this.sourceSchemaRef = sourceSchemaRef;
            this.resolvedSchemaName = resolvedSchemaName;
            this.storageCppType = storageCppType;
            this.validatorId = validatorId;
            this.nullCapability = nullCapability;
            this.supportedAssertions = supportedAssertions != null
                    ? Collections.unmodifiableList(new ArrayList<>(supportedAssertions))
                    : Collections.emptyList();
            this.unsupportedAssertions = unsupportedAssertions != null
                    ? Collections.unmodifiableList(new ArrayList<>(unsupportedAssertions))
                    : Collections.emptyList();
            this.validateParams = validateParams != null
                    ? Collections.unmodifiableMap(new LinkedHashMap<>(validateParams))
                    : Collections.emptyMap();
        }

        public int getBranchIndex() { return branchIndex; }
        public String getSourceSchemaRef() { return sourceSchemaRef; }
        public String getResolvedSchemaName() { return resolvedSchemaName; }
        public String getStorageCppType() { return storageCppType; }
        public String getValidatorId() { return validatorId; }
        public NullCapability getNullCapability() { return nullCapability; }
        public List<String> getSupportedAssertions() { return supportedAssertions; }
        public List<String> getUnsupportedAssertions() { return unsupportedAssertions; }
        public Map<String, Object> getValidateParams() { return validateParams; }

        /** Converts this branch descriptor to a template-safe map for Mustache. */
        public Map<String, Object> toTemplateMap() {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("branch-index", branchIndex);
            map.put("source-schema-ref", sourceSchemaRef);
            map.put("resolved-schema-name", resolvedSchemaName);
            map.put("storage-cpp-type", storageCppType);
            map.put("validator-id", validatorId);
            map.put("null-capability", nullCapability.name().toLowerCase(Locale.ROOT));
            map.put("has-supported-assertions", !supportedAssertions.isEmpty());
            map.put("supported-assertions", supportedAssertions);
            map.put("has-unsupported-assertions", !unsupportedAssertions.isEmpty());
            map.put("unsupported-assertions", unsupportedAssertions);
            // Emit validation parameters for template-driven generator functions
            for (Map.Entry<String, Object> vp : validateParams.entrySet()) {
                map.put(vp.getKey(), vp.getValue());
            }
            return map;
        }
    }


    /**
     * Result of recursively intersecting allOf contributor schemas.
     * Captures merged properties, union required, and satisfiability.
     * Used to build synthetic object schemas for storage model generation.
     */
    public static final class AllOfIntersection {
        private final Map<String, Schema> properties;
        private final Set<String> required;
        private final boolean isSatisfiable;
        private final String unsatisfiableReason;
        /** Map of property names whose intersection is empty (optional impossible). */
        private final Set<String> optionalImpossibleProperties;
        /** Intersected root-level type across all branches (null if absent). */
        private final String rootScalarType;
        /** Intersected root-level enum values across all branches. */
        private final List<Object> rootEnumValues;
        /** Whether all contributors constrain the root with an explicit const. */
        private final boolean rootHasConst;
        /** Intersected root-level const value across all branches. */
        private final Object rootConstValue;
        /** Pristine JSON for an explicit const, including JSON null. */
        private final String rootConstJson;
        /** Whether every root contributor permits JSON null. */
        private final boolean rootAllowsNull;
        /** Minimum numeric value (intersection takes the larger). */
        private final BigDecimal rootMinimum;
        /** Maximum numeric value (intersection takes the smaller). */
        private final BigDecimal rootMaximum;
        /** Exclusive minimum flag. */
        private final Boolean rootExclusiveMinimum;
        /** Exclusive maximum flag. */
        private final Boolean rootExclusiveMaximum;
        /** Numeric exclusive minimum selected for the synthetic schema. */
        private final BigDecimal rootExclusiveMinimumValue;
        /** Numeric exclusive maximum selected for the synthetic schema. */
        private final BigDecimal rootExclusiveMaximumValue;
        /** Strictest additionalProperties constraint across contributors. */
        private final Object additionalProperties;
        /** Minimum string length (intersection takes the larger). */
        private final Integer rootMinLength;
        /** Maximum string length (intersection takes the smaller). */
        private final Integer rootMaxLength;

        public AllOfIntersection(Map<String, Schema> properties, Set<String> required,
                                 boolean isSatisfiable, String unsatisfiableReason,
                                 Set<String> optionalImpossibleProperties) {
            this(properties, required, isSatisfiable, unsatisfiableReason,
                    optionalImpossibleProperties,
                    null, null, null, null, null, null, null,
                    null, null, null, null, null);
        }

        public AllOfIntersection(Map<String, Schema> properties, Set<String> required,
                                 boolean isSatisfiable, String unsatisfiableReason,
                                 Set<String> optionalImpossibleProperties,
                                 String rootScalarType, List<Object> rootEnumValues,
                                 Object rootConstValue,
                                 BigDecimal rootMinimum, BigDecimal rootMaximum,
                                 Boolean rootExclusiveMinimum, Boolean rootExclusiveMaximum,
                                 Integer rootMinLength, Integer rootMaxLength,
                                 BigDecimal rootExclusiveMinimumValue,
                                 BigDecimal rootExclusiveMaximumValue,
                                 Object additionalProperties) {
            this(properties, required, isSatisfiable, unsatisfiableReason,
                    optionalImpossibleProperties, rootScalarType, rootEnumValues,
                    rootConstValue != null, rootConstValue, null,
                    rootMinimum, rootMaximum, rootExclusiveMinimum, rootExclusiveMaximum,
                    rootMinLength, rootMaxLength, rootExclusiveMinimumValue,
                    rootExclusiveMaximumValue, additionalProperties, true);
        }

        public AllOfIntersection(Map<String, Schema> properties, Set<String> required,
                                 boolean isSatisfiable, String unsatisfiableReason,
                                 Set<String> optionalImpossibleProperties,
                                 String rootScalarType, List<Object> rootEnumValues,
                                 boolean rootHasConst, Object rootConstValue,
                                 String rootConstJson,
                                 BigDecimal rootMinimum, BigDecimal rootMaximum,
                                 Boolean rootExclusiveMinimum, Boolean rootExclusiveMaximum,
                                 Integer rootMinLength, Integer rootMaxLength,
                                 BigDecimal rootExclusiveMinimumValue,
                                 BigDecimal rootExclusiveMaximumValue,
                                 Object additionalProperties, boolean rootAllowsNull) {
            this.properties = properties != null
                    ? Collections.unmodifiableMap(new LinkedHashMap<>(properties))
                    : Collections.emptyMap();
            this.required = required != null
                    ? Collections.unmodifiableSet(new LinkedHashSet<>(required))
                    : Collections.emptySet();
            this.isSatisfiable = isSatisfiable;
            this.unsatisfiableReason = unsatisfiableReason;
            this.optionalImpossibleProperties = optionalImpossibleProperties != null
                    ? Collections.unmodifiableSet(new LinkedHashSet<>(optionalImpossibleProperties))
                    : Collections.emptySet();
            this.rootScalarType = rootScalarType;
            this.rootEnumValues = rootEnumValues != null
                    ? Collections.unmodifiableList(new ArrayList<>(rootEnumValues))
                    : null;
            this.rootHasConst = rootHasConst;
            this.rootConstValue = rootConstValue;
            this.rootConstJson = rootConstJson;
            this.rootAllowsNull = rootAllowsNull;
            this.rootMinimum = rootMinimum;
            this.rootMaximum = rootMaximum;
            this.rootExclusiveMinimum = rootExclusiveMinimum;
            this.rootExclusiveMaximum = rootExclusiveMaximum;
            this.rootMinLength = rootMinLength;
            this.rootMaxLength = rootMaxLength;
            this.rootExclusiveMinimumValue = rootExclusiveMinimumValue;
            this.rootExclusiveMaximumValue = rootExclusiveMaximumValue;
            this.additionalProperties = additionalProperties;
        }

        public Map<String, Schema> getProperties() { return properties; }
        public Set<String> getRequired() { return required; }
        public boolean isSatisfiable() { return isSatisfiable; }
        public String getUnsatisfiableReason() { return unsatisfiableReason; }
        public Set<String> getOptionalImpossibleProperties() { return optionalImpossibleProperties; }
        public String getRootScalarType() { return rootScalarType; }
        public List<Object> getRootEnumValues() { return rootEnumValues; }
        public boolean hasRootConst() { return rootHasConst; }
        public Object getRootConstValue() { return rootConstValue; }
        public String getRootConstJson() { return rootConstJson; }
        public boolean allowsRootNull() { return rootAllowsNull; }
        public BigDecimal getRootMinimum() { return rootMinimum; }
        public BigDecimal getRootMaximum() { return rootMaximum; }
        public Boolean getRootExclusiveMinimum() { return rootExclusiveMinimum; }
        public Boolean getRootExclusiveMaximum() { return rootExclusiveMaximum; }
        public Integer getRootMinLength() { return rootMinLength; }
        public Integer getRootMaxLength() { return rootMaxLength; }
        public BigDecimal getRootExclusiveMinimumValue() {
            return rootExclusiveMinimumValue;
        }
        public BigDecimal getRootExclusiveMaximumValue() {
            return rootExclusiveMaximumValue;
        }
        public Object getAdditionalProperties() { return additionalProperties; }
    }

    /**
     * Builds a CompositionDescriptor for a schema if it has oneOf, anyOf, or
     * allOf branches. Returns null for non-composed schemas.
     * Records JSON Pointer locations for diagnostic use.
     */
    static List<CompositionDescriptor> buildCompositionDescriptors(
            String schemaName, Schema schema, OpenAPI openAPI,
            Map<String, Schema> schemas) {
        if (schema == null) return Collections.emptyList();

        List<CompositionDescriptor> descriptors = new ArrayList<>();
        addCompositionDescriptor(descriptors, schemaName, schema, openAPI, schemas,
                "oneOf", schema.getOneOf());
        addCompositionDescriptor(descriptors, schemaName, schema, openAPI, schemas,
                "anyOf", schema.getAnyOf());
        addCompositionDescriptor(descriptors, schemaName, schema, openAPI, schemas,
                "allOf", schema.getAllOf());
        return descriptors;
    }

    private static void addCompositionDescriptor(
            List<CompositionDescriptor> descriptors, String schemaName, Schema schema,
            OpenAPI openAPI, Map<String, Schema> schemas, String keyword,
            List<Schema> branchSchemas) {
        if (branchSchemas != null && !branchSchemas.isEmpty()) {
            descriptors.add(buildCompositionDescriptor(
                    schemaName, schema, openAPI, schemas, keyword, branchSchemas));
        }
    }

    static CompositionDescriptor buildCompositionDescriptor(
            String schemaName, Schema schema, OpenAPI openAPI,
            Map<String, Schema> schemas) {
        List<CompositionDescriptor> descriptors =
                buildCompositionDescriptors(schemaName, schema, openAPI, schemas);
        return descriptors.isEmpty() ? null : descriptors.get(0);
    }

    private static CompositionDescriptor buildCompositionDescriptor(
            String schemaName, Schema schema, OpenAPI openAPI,
            Map<String, Schema> schemas, String keyword, List<Schema> branchSchemas) {

        String schemaLocation = "#/components/schemas/" + schemaName;
        List<CompositionBranchDescriptor> branches = new ArrayList<>();

        // Capture optional discriminator
        DiscriminatorDescriptor discriminatorDescriptor = null;
        if (schema.getDiscriminator() != null) {
            discriminatorDescriptor = new DiscriminatorDescriptor(
                    schema.getDiscriminator().getPropertyName(),
                    schema.getDiscriminator().getMapping());
        }

        for (int index = 0; index < branchSchemas.size(); index++) {
            Schema branchSchema = branchSchemas.get(index);
            String sourceRef = null;
            String resolvedName = null;
            CompositionBranchDescriptor.NullCapability nullCap =
                    CompositionBranchDescriptor.NullCapability.NEVER;
            List<String> supported = new ArrayList<>();
            List<String> unsupported = new ArrayList<>();
            Map<String, Object> validateParams = new LinkedHashMap<>();

            // Resolve the branch schema for assertion scanning
            Schema targetForAssertions = null;
            Schema referenceSchema = referenceSchemaOf(branchSchema);
            boolean normalizedRefWrapper = referenceSchema != null
                    && referenceSchema != branchSchema;
            if (referenceSchema != null) {
                sourceRef = referenceSchema.get$ref();
                String refName = ModelUtils.getSimpleRef(referenceSchema.get$ref());
                resolvedName = refName;
                // Preserve the local ref target for later IR resolution.
                validateParams.put("validation-ref", refName);
                // Detect null type via $ref to null schema
                if ("null".equals(refName)) {
                    nullCap = CompositionBranchDescriptor.NullCapability.ALWAYS;
                } else if (schemas.containsKey(refName)) {
                    Schema refTarget = schemas.get(refName);
                    if (ModelUtils.isNullTypeSchema(openAPI, refTarget)) {
                        nullCap = CompositionBranchDescriptor.NullCapability.ALWAYS;
                    } else {
                        if (Boolean.TRUE.equals(refTarget.getNullable())) {
                            nullCap = CompositionBranchDescriptor.NullCapability.CONDITIONAL;
                        }
                        targetForAssertions = refTarget;
                    }
                }
            } else if (branchSchema != null) {
                targetForAssertions = branchSchema;
                // Detect OAS 3.1 boolean value schemas (true/false literals)
                if (branchSchema.getBooleanSchemaValue() != null) {
                    resolvedName = "boolean-schema";
                } else {
                    resolvedName = branchSchema.getType();
                }
                if (resolvedName == null) {
                    if (branchSchema.getEnum() != null && !branchSchema.getEnum().isEmpty()) {
                        resolvedName = "enum";
                    } else {
                        resolvedName = "object";
                    }
                }
                if (ModelUtils.isNullType(branchSchema)) {
                    nullCap = CompositionBranchDescriptor.NullCapability.ALWAYS;
                } else if (Boolean.TRUE.equals(branchSchema.getNullable())) {
                    nullCap = CompositionBranchDescriptor.NullCapability.CONDITIONAL;
                }
            }

            // Scan the resolved target schema for assertion keywords.
            // Under JSON Schema 2020-12, a $ref target and its sibling keywords
            // both apply. Scan both surfaces and retain unsupported siblings for
            // fail-closed diagnostics.
            boolean refBranch = referenceSchema != null;
            if (targetForAssertions != null) {
                Oas31SchemaSurfaceAssertionScanner.scanSurfaceAssertions(targetForAssertions, openAPI,
                        supported, unsupported, validateParams, refBranch);
            }
            if (refBranch && referenceSchema != targetForAssertions) {
                // $ref with siblings: BOTH the resolved target and the ref's
                // own keyword set apply (2020-12). Ref-node applicator stays
                // branch-driven; sibling keywords are emitted inline.
                Oas31SchemaSurfaceAssertionScanner.scanSurfaceAssertions(referenceSchema, openAPI,
                        supported, unsupported, validateParams, false);
            }
            if (normalizedRefWrapper) {
                // OpenAPINormalizer moves a $ref beside annotations/assertions
                // into a singleton allOf. Scan the outer siblings as adjacent
                // keywords, then discard only that synthetic applicator.
                Oas31SchemaSurfaceAssertionScanner.scanSurfaceAssertions(branchSchema, openAPI,
                        supported, unsupported, validateParams, false);
                validateParams.remove("validation-allof-schemas");
            }

            // Dynamic-scope markers belong to the branch's own surface, never
            // the ref target, and flow into the branch's IR parameters.
            if (branchSchema != null) {
                java.util.Map ext = branchSchema.getExtensions();
                if (ext != null) {
                    Object dynres = ext.get("x-oas31-res");
                    if (dynres instanceof Number) {
                        validateParams.put("validation-dynamic-resource",
                                ((Number) dynres).intValue());
                    }
                    Object dynroot = ext.get("x-oas31-res-root");
                    if (Boolean.TRUE.equals(dynroot) || (dynroot instanceof Number
                            && ((Number) dynroot).intValue() != 0)) {
                        validateParams.put("validation-resource-root", Boolean.TRUE);
                    }
                    Object vinert = ext.get("x-oas31-vocab-inert");
                    if (Boolean.TRUE.equals(vinert)) {
                        validateParams.put("validation-vocab-inert", Boolean.TRUE);
                    }
                    Object dynref = ext.get("x-oas31-dynref");
                    if (dynref != null) {
                        validateParams.put("validation-dynamic-ref-anchor",
                                String.valueOf(dynref));
                    }
                }
                if (branchSchema.get$dynamicAnchor() != null
                        && !branchSchema.get$dynamicAnchor().isEmpty()) {
                    validateParams.put("validation-dynamic-anchor",
                            branchSchema.get$dynamicAnchor());
                }
            }

            // Generate a deterministic base name for branch validation dispatch.
            String validatorId = CppBoostBeastClientCodegen.toValidIdentifier(schemaName)
                    + "_branch_" + index;

            CompositionBranchDescriptor branch = new CompositionBranchDescriptor(
                    index, sourceRef, resolvedName, null, validatorId,
                    nullCap, supported, unsupported, validateParams);
            branches.add(branch);
        }

        return new CompositionDescriptor(
                schemaName, schemaLocation, keyword, branches,
                discriminatorDescriptor);
    }

    /**
     * Returns the direct reference schema, including the singleton-allOf shape
     * produced by {@link org.openapitools.codegen.OpenAPINormalizer} for a
     * {@code $ref} with sibling keywords.
     */
    static Schema referenceSchemaOf(Schema branchSchema) {
        if (branchSchema == null) return null;
        if (branchSchema.get$ref() != null) return branchSchema;
        List<Schema> allOf = branchSchema.getAllOf();
        if (allOf == null || allOf.size() != 1) return null;
        Schema candidate = allOf.get(0);
        return candidate != null && candidate.get$ref() != null ? candidate : null;
    }

    /**
     * Fails generation when a composition branch has an assertion without a
     * membership-preserving implementation.
     */
    static void validateDescriptorAssertions(CompositionDescriptor desc) {
        if (desc == null) return;
        for (CompositionBranchDescriptor branch : desc.getBranches()) {
            for (String unsupported : branch.getUnsupportedAssertions()) {
                throw new CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException(
                        desc.getSchemaLocation(), unsupported);
            }
        }
    }

    /** Tracks the strictest bound at one end of a numeric allOf range. */
    private static final class NumericBound {
        private BigDecimal value;
        private boolean exclusive;

        private void mergeLower(BigDecimal candidate, boolean candidateExclusive) {
            if (candidate == null) return;
            if (value == null || candidate.compareTo(value) > 0
                    || (candidate.compareTo(value) == 0 && candidateExclusive && !exclusive)) {
                value = candidate;
                exclusive = candidateExclusive;
            }
        }

        private void mergeUpper(BigDecimal candidate, boolean candidateExclusive) {
            if (candidate == null) return;
            if (value == null || candidate.compareTo(value) < 0
                    || (candidate.compareTo(value) == 0 && candidateExclusive && !exclusive)) {
                value = candidate;
                exclusive = candidateExclusive;
            }
        }
    }

    // ========================================================================
    // Recursive allOf intersection engine
    // ========================================================================

    /**
     * Computes the recursive intersection of all allOf contributors.
     * Resolves $ref-to-allOf chains recursively with cycle detection via the
     * visited set. Merges properties, unions required, and detects
     * unsatisfiable intersections.
     * <p>
     * For each property that appears in multiple contributors, their property
     * schemas are recursively intersected. If the intersection of a required
     * property is empty, the model is unsatisfiable. If the intersection of
     * an optional property is empty, the property is tagged as
     * optional-impossible (rejected when present, but does not invalidate
     * an otherwise valid object).
     *
     * @param schemaName the source schema name (for diagnostics)
     * @param schema     the allOf schema whose branches to intersect
     * @param openAPI    the parsed OpenAPI document
     * @param schemas    the component schemas index
     * @param visited    set of already-visited schema names (cycle guard)
     * @return the computed intersection, or null if no allOf branches
     * @throws AllOfRequiredUnsatisfiableException if a required intersection
     *         is empty and the model cannot be generated
     */
    static AllOfIntersection computeAllOfIntersection(
            String schemaName, Schema schema, OpenAPI openAPI,
            Map<String, Schema> schemas, Set<String> visited) {
        if (schema == null) return null;
        List<Schema> allOfBranches = schema.getAllOf();
        if (allOfBranches == null || allOfBranches.isEmpty()) return null;

        // Register at entry so recursive allOf references return a cycle sentinel.
        if (schemaName != null && visited.contains(schemaName)) {
            return new AllOfIntersection(
                    new LinkedHashMap<>(), new LinkedHashSet<>(),
                    true, null, new LinkedHashSet<>());
        }
        if (schemaName != null) {
            visited.add(schemaName);
        }

        Map<String, Schema> mergedProperties = new LinkedHashMap<>();
        Set<String> mergedRequired = new LinkedHashSet<>();
        Set<String> optionalImpossibleProperties = new LinkedHashSet<>();
        boolean satisfiable = true;
        String unsatisfiableReason = null;

        // Root-level scalar intersection tracking
        String rootScalarType = null;
        List<Object> rootEnumValues = null;
        Object rootConstValue = null;
        boolean rootHasConst = false;
        String rootConstJson = null;
        BigDecimal rootMinimum = null;
        BigDecimal rootMaximum = null;
        Boolean rootExclusiveMinimumObj = null;
        Boolean rootExclusiveMaximumObj = null;
        BigDecimal rootExclusiveMinimumValue = null;
        BigDecimal rootExclusiveMaximumValue = null;
        Object additionalProperties = null;
        NumericBound lowerBound = new NumericBound();
        NumericBound upperBound = new NumericBound();
        Integer rootMinLength = null;
        Integer rootMaxLength = null;
        boolean hasRootScalarConstraints = false;
        boolean rootEnumIntersected = false;
        boolean rootAllowsNull = true;

        for (int bi = 0; bi < allOfBranches.size(); bi++) {
            Schema branch = allOfBranches.get(bi);
            Schema resolvedBranch = resolveAllOfBranch(branch, openAPI, schemas, visited);
            if (resolvedBranch == null) continue;
            rootAllowsNull &= allowsNull(resolvedBranch, declaredTypes(resolvedBranch));

            // Detect nested allOf within the resolved branch and recurse.
            if (resolvedBranch.getAllOf() != null && !resolvedBranch.getAllOf().isEmpty()) {
                AllOfIntersection nested = computeAllOfIntersection(
                        schemaName + "_nested_" + bi, resolvedBranch, openAPI, schemas, visited);
                if (nested != null) {
                    rootAllowsNull &= nested.allowsRootNull();
                    mergeIntersectionIntoResult(mergedProperties, mergedRequired,
                            optionalImpossibleProperties, nested, openAPI, schemas);
                    if (!nested.isSatisfiable()) {
                        satisfiable = false;
                        unsatisfiableReason = nested.getUnsatisfiableReason();
                    }
                    // Propagate optional-impossible entries from nested
                    optionalImpossibleProperties.addAll(nested.getOptionalImpossibleProperties());
                    additionalProperties = intersectAdditionalProperties(
                            additionalProperties, nested.getAdditionalProperties(),
                            openAPI, schemas);

                    if (nested.getRootScalarType() != null) {
                        hasRootScalarConstraints = true;
                        String nestedRootType = nested.getRootScalarType();
                        if (rootScalarType == null) {
                            rootScalarType = nestedRootType;
                        } else if (!rootScalarType.equals(nestedRootType)) {
                            if (("integer".equals(nestedRootType)
                                    && "number".equals(rootScalarType))
                                    || ("number".equals(nestedRootType)
                                    && "integer".equals(rootScalarType))) {
                                rootScalarType = "integer";
                            } else {
                                satisfiable = false;
                                unsatisfiableReason = "Incompatible root types across allOf '"
                                        + schemaName + "' contributors: '" + rootScalarType
                                        + "' vs '" + nestedRootType + "'";
                            }
                        }
                    }
                    if (nested.getRootEnumValues() != null) {
                        hasRootScalarConstraints = true;
                        if (rootEnumValues == null) {
                            rootEnumValues = new ArrayList<>(nested.getRootEnumValues());
                            rootEnumIntersected = false;
                        } else {
                            rootEnumValues = intersectJsonValues(
                                    rootEnumValues, nested.getRootEnumValues());
                            rootEnumIntersected = true;
                        }
                    }
                    if (nested.hasRootConst()) {
                        hasRootScalarConstraints = true;
                        Object nestedConstValue = nested.getRootConstValue();
                        String nestedConstJson = nested.getRootConstJson();
                        if (!rootHasConst) {
                            rootHasConst = true;
                            rootConstValue = nestedConstValue;
                            rootConstJson = nestedConstJson;
                        } else if (!constValuesEqual(rootConstValue, rootConstJson,
                                nestedConstValue, nestedConstJson)) {
                            satisfiable = false;
                            unsatisfiableReason = "Incompatible const values across allOf '"
                                    + schemaName + "' contributors: '"
                                    + rootConstJson + "' vs '" + nestedConstJson + "'";
                        }
                    }
                    if (nested.getRootMinimum() != null) {
                        lowerBound.mergeLower(nested.getRootMinimum(),
                                Boolean.TRUE.equals(nested.getRootExclusiveMinimum()));
                        hasRootScalarConstraints = true;
                    }
                    if (nested.getRootMaximum() != null) {
                        upperBound.mergeUpper(nested.getRootMaximum(),
                                Boolean.TRUE.equals(nested.getRootExclusiveMaximum()));
                        hasRootScalarConstraints = true;
                    }
                    if (nested.getRootMinLength() != null) {
                        rootMinLength = tighterMinLen(rootMinLength, nested.getRootMinLength());
                        hasRootScalarConstraints = true;
                    }
                    if (nested.getRootMaxLength() != null) {
                        rootMaxLength = tighterMaxLen(rootMaxLength, nested.getRootMaxLength());
                        hasRootScalarConstraints = true;
                    }
                }
            }

            // Merge this contributor's properties into the result.
            // For properties that already exist (from a prior contributor),
            // recursively intersect the property schemas.
            if (resolvedBranch.getProperties() != null) {
                @SuppressWarnings("rawtypes")
                Map rawProps = resolvedBranch.getProperties();
                @SuppressWarnings("unchecked")
                Map<String, Schema> typedProps = rawProps;
                for (Map.Entry<String, Schema> propEntry
                        : typedProps.entrySet()) {
                    String propName = propEntry.getKey();
                    Schema propSchema = propEntry.getValue();
                    if (mergedProperties.containsKey(propName)) {
                        Schema existing = mergedProperties.get(propName);
                        Schema intersected = intersectPropertySchemas(
                                existing, propSchema, openAPI, schemas, new HashSet<>());
                        mergedProperties.put(propName, intersected);
                    } else {
                        mergedProperties.put(propName, propSchema);
                    }
                }
            }

            // Union required property sets
            if (resolvedBranch.getRequired() != null) {
                mergedRequired.addAll(resolvedBranch.getRequired());
            }

            // A false constraint closes the object; schema constraints are
            // intersected for the remaining open contributors.
            additionalProperties = intersectAdditionalProperties(
                    additionalProperties, resolvedBranch.getAdditionalProperties(),
                    openAPI, schemas);

            // Accumulate root-level scalar constraints from non-object branches
            // (branches that contribute no properties).
            if (resolvedBranch.getProperties() == null
                    || resolvedBranch.getProperties().isEmpty()) {
                // Intersect root-level type
                String branchType = resolvedBranch.getType();
                if (branchType != null) {
                    hasRootScalarConstraints = true;
                    if (rootScalarType == null) {
                        rootScalarType = branchType;
                    } else if (!rootScalarType.equals(branchType)) {
                        // Compatible numeric types
                        if ("integer".equals(branchType) && "number".equals(rootScalarType)) {
                            rootScalarType = "integer";
                        } else if ("number".equals(branchType) && "integer".equals(rootScalarType)) {
                            rootScalarType = "integer";
                        } else {
                            satisfiable = false;
                            unsatisfiableReason = "Incompatible root types across allOf '"
                                    + schemaName + "' contributors: '" + rootScalarType
                                    + "' vs '" + branchType + "'";
                        }
                    }
                }

                // Intersect root-level enum (intersection of all branch enum sets)
                List<Object> branchEnum = resolvedBranch.getEnum();
                if (branchEnum != null && !branchEnum.isEmpty()) {
                    hasRootScalarConstraints = true;
                    if (rootEnumValues == null) {
                        rootEnumValues = new ArrayList<>(branchEnum);
                        rootEnumIntersected = false;
                    } else {
                        rootEnumValues = intersectJsonValues(rootEnumValues, branchEnum);
                        rootEnumIntersected = true;
                    }
                }

                // Intersect root-level const (must match). swagger-parser uses
                // null both for an absent const and an explicit JSON null, so use
                boolean branchHasConst = hasConstConstraint(resolvedBranch);
                if (branchHasConst) {
                    hasRootScalarConstraints = true;
                    Object branchConst = resolvedBranch.getConst();
                    String branchConstJson = Oas31RawSpecRecovery.constJsonOf(resolvedBranch);
                    if (!rootHasConst) {
                        rootHasConst = true;
                        rootConstValue = branchConst;
                        rootConstJson = branchConstJson;
                    } else if (!constValuesEqual(rootConstValue, rootConstJson,
                            branchConst, branchConstJson)) {
                        satisfiable = false;
                        unsatisfiableReason = "Incompatible const values across allOf '"
                                + schemaName + "' contributors: '"
                                + rootConstJson + "' vs '" + branchConstJson + "'";
                    }
                }

                // Merge numeric bounds as value/exclusivity pairs so a smaller
                // exclusive bound cannot make a larger inclusive bound strict.
                if (resolvedBranch.getMinimum() != null) {
                    hasRootScalarConstraints = true;
                    lowerBound.mergeLower(resolvedBranch.getMinimum(),
                            Boolean.TRUE.equals(resolvedBranch.getExclusiveMinimum()));
                }
                if (resolvedBranch.getExclusiveMinimumValue() != null) {
                    hasRootScalarConstraints = true;
                    lowerBound.mergeLower(resolvedBranch.getExclusiveMinimumValue(), true);
                }
                if (resolvedBranch.getMaximum() != null) {
                    hasRootScalarConstraints = true;
                    upperBound.mergeUpper(resolvedBranch.getMaximum(),
                            Boolean.TRUE.equals(resolvedBranch.getExclusiveMaximum()));
                }
                if (resolvedBranch.getExclusiveMaximumValue() != null) {
                    hasRootScalarConstraints = true;
                    upperBound.mergeUpper(resolvedBranch.getExclusiveMaximumValue(), true);
                }

                // Intersect minLength / maxLength: tighter wins
                if (resolvedBranch.getMinLength() != null) {
                    hasRootScalarConstraints = true;
                    Integer branchMinLength = resolvedBranch.getMinLength();
                    if (rootMinLength == null || branchMinLength > rootMinLength) {
                        rootMinLength = branchMinLength;
                    }
                }
                if (resolvedBranch.getMaxLength() != null) {
                    hasRootScalarConstraints = true;
                    Integer branchMaxLength = resolvedBranch.getMaxLength();
                    if (rootMaxLength == null || branchMaxLength < rootMaxLength) {
                        rootMaxLength = branchMaxLength;
                    }
                }
            }
        }

        // Detect empty enum intersection: two or more branches both contributed
        // enum lists whose intersection is empty (e.g., [a,b] ∩ [c,d] = {}).
        if (rootEnumIntersected && rootEnumValues != null && rootEnumValues.isEmpty()) {
            satisfiable = false;
            unsatisfiableReason = "Empty enum intersection across allOf '"
                    + schemaName + "' contributors: no common enum values";
        }

        if (rootHasConst && !constSatisfiesConstraints(
                rootConstValue, rootConstJson,
                rootScalarType != null
                        ? Collections.singleton(rootScalarType) : Collections.emptySet(),
                rootAllowsNull, rootEnumValues)) {
            satisfiable = false;
            unsatisfiableReason = "Root const value in allOf '" + schemaName
                    + "' is excluded by a sibling type or enum constraint";
        }

        // Required properties must have non-empty intersections. The property
        // intersection helper records unsatisfiable schemas with an extension
        // marker, which is converted to a model-level generation error below.

        // Detect unsatisfiable required properties:
        // Scan merged properties for unsatisfiable markers.
        for (String propName : mergedRequired) {
            Schema propSchema = mergedProperties.get(propName);
            if (propSchema != null && Boolean.TRUE.equals(
                    propSchema.getExtensions() != null
                            ? propSchema.getExtensions().get("x-cpp-unsatisfiable")
                            : null)) {
                satisfiable = false;
                unsatisfiableReason = "Required property '" + propName
                        + "' in schema '" + schemaName
                        + "' has an empty intersection across allOf contributors. "
                        + "This property is required but cannot satisfy all "
                        + "contributor constraints simultaneously.";
            }
        }

        // Tag optional impossible properties: present in merged but marked
        // with the unsatisfiable flag and NOT in mergedRequired.
        for (Map.Entry<String, Schema> entry : mergedProperties.entrySet()) {
            String propName = entry.getKey();
            if (mergedRequired.contains(propName)) continue;
            Schema propSchema = entry.getValue();
            if (propSchema != null && Boolean.TRUE.equals(
                    propSchema.getExtensions() != null
                            ? propSchema.getExtensions().get("x-cpp-unsatisfiable")
                            : null)) {
                optionalImpossibleProperties.add(propName);
            }
        }

        rootMinimum = lowerBound.value;
        rootMaximum = upperBound.value;
        rootExclusiveMinimumObj = lowerBound.exclusive ? Boolean.TRUE : null;
        rootExclusiveMaximumObj = upperBound.exclusive ? Boolean.TRUE : null;
        rootExclusiveMinimumValue = lowerBound.exclusive ? lowerBound.value : null;
        rootExclusiveMaximumValue = upperBound.exclusive ? upperBound.value : null;

        // If no scalar constraints were accumulated but properties exist, null out root fields
        if (!hasRootScalarConstraints) {
            rootScalarType = null;
            rootEnumValues = null;
            rootConstValue = null;
            rootHasConst = false;
            rootConstJson = null;
            rootMinimum = null;
            rootMaximum = null;
            rootExclusiveMinimumObj = null;
            rootExclusiveMaximumObj = null;
            rootMinLength = null;
            rootMaxLength = null;
            rootExclusiveMinimumValue = null;
            rootExclusiveMaximumValue = null;
        }

        return new AllOfIntersection(
                mergedProperties, mergedRequired, satisfiable,
                unsatisfiableReason, optionalImpossibleProperties,
                rootScalarType, rootEnumValues, rootHasConst, rootConstValue, rootConstJson,
                rootMinimum, rootMaximum,
                rootExclusiveMinimumObj, rootExclusiveMaximumObj,
                rootMinLength, rootMaxLength,
                rootExclusiveMinimumValue, rootExclusiveMaximumValue,
                additionalProperties, rootAllowsNull);
    }

    /**
     * Merges a nested AllOfIntersection into a running result.
     * For properties that already exist, recursively intersects them.
     */
    private static void mergeIntersectionIntoResult(
            Map<String, Schema> mergedProperties, Set<String> mergedRequired,
            Set<String> optionalImpossibleProperties,
            AllOfIntersection nested,
            OpenAPI openAPI, Map<String, Schema> schemas) {
        for (Map.Entry<String, Schema> nestedProp : nested.getProperties().entrySet()) {
            String propName = nestedProp.getKey();
            Schema nestedSchema = nestedProp.getValue();
            if (mergedProperties.containsKey(propName)) {
                mergedProperties.put(propName,
                        intersectPropertySchemas(
                                mergedProperties.get(propName),
                                nestedSchema, openAPI, schemas, new HashSet<>()));
            } else {
                mergedProperties.put(propName, nestedSchema);
            }
        }
        mergedRequired.addAll(nested.getRequired());
        optionalImpossibleProperties.addAll(nested.getOptionalImpossibleProperties());
    }

    private static Object intersectAdditionalProperties(
            Object existing, Object incoming, OpenAPI openAPI, Map<String, Schema> schemas) {
        if (Boolean.FALSE.equals(existing) || Boolean.FALSE.equals(incoming)) {
            return Boolean.FALSE;
        }
        if (existing instanceof Schema && incoming instanceof Schema) {
            return intersectPropertySchemas((Schema) existing, (Schema) incoming,
                    openAPI, schemas, new HashSet<>());
        }
        if (existing instanceof Schema) {
            return existing;
        }
        if (incoming instanceof Schema) {
            return incoming;
        }
        // true and an omitted constraint both leave additional values open.
        return null;
    }

    /**
     * Resolves an allOf branch schema, following $ref targets recursively.
     * If the branch has a $ref, resolves it to a non-allOf schema.
     * If the resolved target is itself allOf, returns it as-is for
     * recursive handling by the caller.
     *
     * @param branch  the allOf contributor (possibly a $ref)
     * @param openAPI the parsed OpenAPI document
     * @param schemas the component schemas index
     * @param visited set of already-visited schema names (cycle guard)
     * @return the resolved schema, or null if unresolvable
     */
    private static Schema resolveAllOfBranch(
            Schema branch, OpenAPI openAPI,
            Map<String, Schema> schemas, Set<String> visited) {
        if (branch == null) return null;
        if (branch.get$ref() == null) return branch;

        String refName = ModelUtils.getSimpleRef(branch.get$ref());
        if (refName == null) return branch;
        if (visited.contains(refName)) return branch; // cycle guard

        Schema refTarget = schemas != null ? schemas.get(refName) : null;
        if (refTarget == null && openAPI != null) {
            refTarget = ModelUtils.getReferencedSchema(openAPI, branch);
        }
        if (refTarget == null) return branch;

        visited.add(refName);
        try {
            // If the resolved target also has allOf, recurse
            if (refTarget.getAllOf() != null && !refTarget.getAllOf().isEmpty()) {
                return refTarget; // Return so caller can recurse
            }
            // If the resolved target has properties, return it directly
            if (refTarget.getProperties() != null && !refTarget.getProperties().isEmpty()) {
                return refTarget;
            }
            return refTarget;
        } finally {
            visited.remove(refName);
        }
    }

    /**
     * Intersects two property schemas, combining their constraints.
     * Returns a synthetic Schema that represents the intersection:
     * <ul>
     *   <li>Types are intersected (must have a common type)</li>
     *   <li>Enums are intersected (common values only)</li>
     *   <li>Numeric bounds are tightened</li>
     *   <li>String bounds are tightened</li>
     *   <li>Patterns are retained from both</li>
     *   <li>Required properties are unioned</li>
     *   <li>Properties are recursively intersected</li>
     * </ul>
     * <p>
     * When the intersection is empty (e.g., string ∩ integer), the resulting
     * Schema is tagged with vendor extension {@code x-cpp-unsatisfiable: true}
     * and the property should either fail generation (if required) or generate
     * decode-time rejection (if optional).
     */
    private static Schema resolveReferenceWithSiblings(
            Schema source, OpenAPI openAPI, Map<String, Schema> schemas, Set<String> visited,
            IdentityHashMap<Schema, Set<Schema>> activePairs) {
        if (source == null || source.get$ref() == null) {
            return source;
        }
        Schema target = ModelUtils.getReferencedSchema(openAPI, source);
        if (target == null || target == source || !hasReferenceSiblings(source)) {
            return target == null ? source : target;
        }

        String reference = source.get$ref();
        source.set$ref(null);
        try {
            return intersectPropertySchemas(target, source, openAPI, schemas, visited, activePairs);
        } finally {
            source.set$ref(reference);
        }
    }

    private static boolean hasReferenceSiblings(Schema schema) {
        return schema.getType() != null || schema.getTypes() != null
                || schema.getEnum() != null || hasConstConstraint(schema)
                || schema.getMinimum() != null || schema.getMaximum() != null
                || schema.getExclusiveMinimumValue() != null
                || schema.getExclusiveMaximumValue() != null
                || Boolean.TRUE.equals(schema.getExclusiveMinimum())
                || Boolean.TRUE.equals(schema.getExclusiveMaximum())
                || schema.getMultipleOf() != null || schema.getPattern() != null
                || schema.getMinLength() != null || schema.getMaxLength() != null
                || schema.getMinItems() != null || schema.getMaxItems() != null
                || schema.getMinProperties() != null || schema.getMaxProperties() != null
                || schema.getProperties() != null || schema.getRequired() != null
                || schema.getAdditionalProperties() != null || schema.getNullable() != null;
    }

    private static Set<String> declaredTypes(Schema schema) {
        Set<String> types = new LinkedHashSet<>();
        if (schema.getType() != null) {
            types.add(schema.getType());
        }
        if (schema.getTypes() != null) {
            for (Object type : schema.getTypes()) {
                if (type != null) {
                    types.add(String.valueOf(type));
                }
            }
        }
        return types;
    }

    private static boolean allowsNull(Schema schema, Set<String> types) {
        if (!Boolean.TRUE.equals(schema.getNullable())
                && !Oas31RawSpecRecovery.pristineTypeHasNull(schema)
                && !types.isEmpty() && !types.contains("null")) {
            return false;
        }
        if (schema.getConst() != null) {
            return false;
        }
        if (Oas31RawSpecRecovery.hasExplicitConst(schema)) {
            return "null".equals(Oas31RawSpecRecovery.constJsonOf(schema));
        }
        List<Object> enumValues = schema.getEnum();
        return enumValues == null || enumValues.contains(null);
    }

    private static Set<String> intersectDeclaredTypes(
            Set<String> existingTypes, Set<String> incomingTypes) {
        if (existingTypes.isEmpty()) {
            return new LinkedHashSet<>(incomingTypes);
        }
        if (incomingTypes.isEmpty()) {
            return new LinkedHashSet<>(existingTypes);
        }

        Set<String> intersection = new LinkedHashSet<>();
        for (String existingType : existingTypes) {
            for (String incomingType : incomingTypes) {
                if (existingType.equals(incomingType)) {
                    intersection.add(existingType);
                } else if (("integer".equals(existingType) && "number".equals(incomingType))
                        || ("number".equals(existingType) && "integer".equals(incomingType))) {
                    intersection.add("integer");
                }
            }
        }
        return intersection;
    }

    private static Schema intersectPropertySchemas(
            Schema existing, Schema incoming,
            OpenAPI openAPI, Map<String, Schema> schemas, Set<String> visited) {
        return intersectPropertySchemas(existing, incoming, openAPI, schemas, visited,
                new IdentityHashMap<>());
    }

    private static Schema intersectPropertySchemas(
            Schema existing, Schema incoming,
            OpenAPI openAPI, Map<String, Schema> schemas, Set<String> visited,
            IdentityHashMap<Schema, Set<Schema>> activePairs) {
        if (existing == null) return incoming;
        if (incoming == null) return existing;

        // Track the active pair by object identity, not a collision-prone hash.
        Set<Schema> activeIncoming = activePairs.computeIfAbsent(existing,
                ignored -> Collections.newSetFromMap(new IdentityHashMap<Schema, Boolean>()));
        if (!activeIncoming.add(incoming)) {
            return existing;
        }
        try {
            return intersectPropertySchemaConstraints(
                    existing, incoming, openAPI, schemas, visited, activePairs);
        } finally {
            activeIncoming.remove(incoming);
            if (activeIncoming.isEmpty()) {
                activePairs.remove(existing);
            }
        }
    }

    private static Schema intersectPropertySchemaConstraints(
            Schema existing, Schema incoming,
            OpenAPI openAPI, Map<String, Schema> schemas, Set<String> visited,
            IdentityHashMap<Schema, Set<Schema>> activePairs) {
        // Resolve $ref targets without discarding constraints attached to a
        // 2020-12 reference node.
        existing = resolveReferenceWithSiblings(
                existing, openAPI, schemas, visited, activePairs);
        incoming = resolveReferenceWithSiblings(
                incoming, openAPI, schemas, visited, activePairs);

        // Both non-null: compute intersection.
        Set<String> existingTypes = declaredTypes(existing);
        Set<String> incomingTypes = declaredTypes(incoming);
        Set<String> intersectedTypes = intersectDeclaredTypes(existingTypes, incomingTypes);
        boolean typeCompatible = existingTypes.isEmpty() || incomingTypes.isEmpty()
                || !intersectedTypes.isEmpty();

        // Build the intersected schema.
        Schema intersected = new Schema();
        if (!intersectedTypes.isEmpty()) {
            if (intersectedTypes.size() == 1 && !intersectedTypes.contains("null")) {
                intersected.setType(intersectedTypes.iterator().next());
            } else {
                intersected.setTypes(intersectedTypes);
            }
        }
        boolean existingAllowsNull = allowsNull(existing, existingTypes);
        boolean incomingAllowsNull = allowsNull(incoming, incomingTypes);
        if (existingAllowsNull && incomingAllowsNull) {
            intersected.setNullable(true);
        }

        List<Object> existingEnum = existing.getEnum();
        List<Object> incomingEnum = incoming.getEnum();
        List<Object> intersectedEnum = null;
        if (existingEnum != null && incomingEnum != null) {
            intersectedEnum = intersectJsonValues(existingEnum, incomingEnum);
            if (intersectedEnum.isEmpty()) {
                typeCompatible = false;
            }
        }

        // Intersect enum values
        if (intersectedEnum != null && !intersectedEnum.isEmpty()) {
            intersected.setEnum(intersectedEnum);
        } else if (existingEnum != null && incomingEnum == null) {
            intersected.setEnum(new ArrayList<>(existingEnum));
        } else if (incomingEnum != null && existingEnum == null) {
            intersected.setEnum(new ArrayList<>(incomingEnum));
        }

        // Intersect const values, including an explicit recovered JSON null.
        boolean existingHasConst = hasConstConstraint(existing);
        boolean incomingHasConst = hasConstConstraint(incoming);
        if (existingHasConst && incomingHasConst) {
            if (constValuesEqual(existing.getConst(), Oas31RawSpecRecovery.constJsonOf(existing),
                    incoming.getConst(), Oas31RawSpecRecovery.constJsonOf(incoming))) {
                copyConstConstraint(existing, intersected);
            } else {
                typeCompatible = false; // conflicting const values
            }
        } else if (existingHasConst) {
            copyConstConstraint(existing, intersected);
        } else if (incomingHasConst) {
            copyConstConstraint(incoming, intersected);
        }

        if (hasConstConstraint(intersected) && !constSatisfiesConstraints(
                intersected.getConst(), Oas31RawSpecRecovery.constJsonOf(intersected),
                intersectedTypes, Boolean.TRUE.equals(intersected.getNullable()),
                intersected.getEnum())) {
            typeCompatible = false;
        }

        // Numeric bounds are compared as value/exclusivity pairs. A strict
        // bound matters only when it is the tightest bound at that endpoint.
        NumericBound lowerBound = new NumericBound();
        NumericBound upperBound = new NumericBound();
        if (existing.getMinimum() != null) {
            lowerBound.mergeLower(existing.getMinimum(),
                    Boolean.TRUE.equals(existing.getExclusiveMinimum()));
        }
        if (existing.getExclusiveMinimumValue() != null) {
            lowerBound.mergeLower(existing.getExclusiveMinimumValue(), true);
        }
        if (incoming.getMinimum() != null) {
            lowerBound.mergeLower(incoming.getMinimum(),
                    Boolean.TRUE.equals(incoming.getExclusiveMinimum()));
        }
        if (incoming.getExclusiveMinimumValue() != null) {
            lowerBound.mergeLower(incoming.getExclusiveMinimumValue(), true);
        }
        if (lowerBound.value != null) {
            intersected.setMinimum(lowerBound.value);
            if (lowerBound.exclusive) {
                intersected.setExclusiveMinimum(true);
                intersected.setExclusiveMinimumValue(lowerBound.value);
            }
        }
        if (existing.getMaximum() != null) {
            upperBound.mergeUpper(existing.getMaximum(),
                    Boolean.TRUE.equals(existing.getExclusiveMaximum()));
        }
        if (existing.getExclusiveMaximumValue() != null) {
            upperBound.mergeUpper(existing.getExclusiveMaximumValue(), true);
        }
        if (incoming.getMaximum() != null) {
            upperBound.mergeUpper(incoming.getMaximum(),
                    Boolean.TRUE.equals(incoming.getExclusiveMaximum()));
        }
        if (incoming.getExclusiveMaximumValue() != null) {
            upperBound.mergeUpper(incoming.getExclusiveMaximumValue(), true);
        }
        if (upperBound.value != null) {
            intersected.setMaximum(upperBound.value);
            if (upperBound.exclusive) {
                intersected.setExclusiveMaximum(true);
                intersected.setExclusiveMaximumValue(upperBound.value);
            }
        }
        if (existing.getMultipleOf() != null || incoming.getMultipleOf() != null) {
            // The synthetic schema is for C++ storage modeling; retain one
            // representative constraint. The evaluator still validates every
            // original allOf contributor.
            if (existing.getMultipleOf() != null) {
                intersected.setMultipleOf(existing.getMultipleOf());
            } else {
                intersected.setMultipleOf(incoming.getMultipleOf());
            }
        }

        // String bounds: take the tighter
        intersected.setMinLength(tighterMinLen(
                existing.getMinLength(), incoming.getMinLength()));
        intersected.setMaxLength(tighterMaxLen(
                existing.getMaxLength(), incoming.getMaxLength()));

        // Retain one pattern on the synthetic storage schema. Exact membership
        // evaluates every original allOf contributor, so no assertion is lost.
        if (existing.getPattern() != null || incoming.getPattern() != null) {
            if (existing.getPattern() != null) {
                intersected.setPattern(existing.getPattern());
            } else {
                intersected.setPattern(incoming.getPattern());
            }
        }

        // Array bounds: take the tighter
        intersected.setMinItems(tighterMinLen(
                existing.getMinItems(), incoming.getMinItems()));
        intersected.setMaxItems(tighterMaxLen(
                existing.getMaxItems(), incoming.getMaxItems()));
        if (Boolean.TRUE.equals(existing.getUniqueItems())
                || Boolean.TRUE.equals(incoming.getUniqueItems())) {
            intersected.setUniqueItems(true);
        }

        // Object bounds: take the tighter
        intersected.setMinProperties(tighterMinLen(
                existing.getMinProperties(), incoming.getMinProperties()));
        intersected.setMaxProperties(tighterMaxLen(
                existing.getMaxProperties(), incoming.getMaxProperties()));

        if (existing.getRequired() != null || incoming.getRequired() != null) {
            Set<String> required = new LinkedHashSet<>();
            if (existing.getRequired() != null) {
                required.addAll(existing.getRequired());
            }
            if (incoming.getRequired() != null) {
                required.addAll(incoming.getRequired());
            }
            if (!required.isEmpty()) {
                intersected.setRequired(new ArrayList<>(required));
            }
        }

        Object additionalProperties = intersectAdditionalProperties(
                existing.getAdditionalProperties(), incoming.getAdditionalProperties(),
                openAPI, schemas);
        if (additionalProperties != null) {
            intersected.setAdditionalProperties(additionalProperties);
        }

        // Recursive property intersection for nested object schemas
        // (properties on properties)
        Map<String, Schema> existingProperties = existing.getProperties();
        Map<String, Schema> incomingProperties = incoming.getProperties();
        if ((existingProperties != null && !existingProperties.isEmpty())
                || (incomingProperties != null && !incomingProperties.isEmpty())) {
            if (existingProperties != null && incomingProperties != null) {
                Map<String, Schema> merged = new LinkedHashMap<>(existingProperties);
                for (Map.Entry<String, Schema> entry : incomingProperties.entrySet()) {
                    String key = entry.getKey();
                    Schema val = entry.getValue();
                    if (merged.containsKey(key)) {
                        merged.put(key, intersectPropertySchemas(
                                merged.get(key), val, openAPI, schemas, visited, activePairs));
                    } else {
                        merged.put(key, val);
                    }
                }
                intersected.setProperties(merged);
            } else if (existingProperties != null) {
                intersected.setProperties(new LinkedHashMap<>(existingProperties));
            } else {
                intersected.setProperties(new LinkedHashMap<>(incomingProperties));
            }
        }

        // Mark unsatisfiable when types are incompatible
        if (!typeCompatible) {
            Map<String, Object> extensions = intersected.getExtensions();
            if (extensions == null) {
                extensions = new LinkedHashMap<>();
                intersected.setExtensions(extensions);
            }
            extensions.put("x-cpp-unsatisfiable", true);
        }

        return intersected;
    }

    private static List<Object> intersectJsonValues(
            List<?> left, List<?> right) {
        List<Object> intersection = new ArrayList<>();
        for (Object candidate : left) {
            for (Object value : right) {
                if (jsonValuesEqual(candidate, value)) {
                    intersection.add(candidate);
                    break;
                }
            }
        }
        return intersection;
    }

    private static boolean jsonValuesEqual(Object left, Object right) {
        if (left == right) {
            return true;
        }
        if (left == null || right == null) {
            return false;
        }
        if (left instanceof Number && right instanceof Number) {
            try {
                return new BigDecimal(left.toString())
                        .compareTo(new BigDecimal(right.toString())) == 0;
            } catch (NumberFormatException ignored) {
                return left.equals(right);
            }
        }
        if (left instanceof List && right instanceof List) {
            List<?> leftList = (List<?>) left;
            List<?> rightList = (List<?>) right;
            if (leftList.size() != rightList.size()) {
                return false;
            }
            for (int index = 0; index < leftList.size(); index++) {
                if (!jsonValuesEqual(leftList.get(index), rightList.get(index))) {
                    return false;
                }
            }
            return true;
        }
        if (left instanceof Map && right instanceof Map) {
            Map<?, ?> leftMap = (Map<?, ?>) left;
            Map<?, ?> rightMap = (Map<?, ?>) right;
            if (!leftMap.keySet().equals(rightMap.keySet())) {
                return false;
            }
            for (Object key : leftMap.keySet()) {
                if (!jsonValuesEqual(leftMap.get(key), rightMap.get(key))) {
                    return false;
                }
            }
            return true;
        }
        return left.equals(right);
    }

    private static boolean constValuesEqual(Object left, String leftJson,
                                            Object right, String rightJson) {
        if (leftJson != null && leftJson.equals(rightJson)) {
            return true;
        }
        if ("null".equals(leftJson) || "null".equals(rightJson)) {
            return false;
        }
        return jsonValuesEqual(left, right);
    }

    private static boolean constSatisfiesConstraints(
            Object constValue, String constJson, Set<String> types,
            boolean nullable, List<Object> enumValues) {
        String constType = jsonSchemaTypeOfConst(constValue, constJson);
        if (constType != null && !types.isEmpty()) {
            boolean typeAllowed;
            if ("null".equals(constType)) {
                typeAllowed = nullable || types.contains("null");
            } else if ("integer".equals(constType)) {
                typeAllowed = types.contains("integer") || types.contains("number");
            } else {
                typeAllowed = types.contains(constType);
            }
            if (!typeAllowed) {
                return false;
            }
        }
        if (enumValues == null) {
            return true;
        }
        for (Object enumValue : enumValues) {
            if (jsonValuesEqual(constValue, enumValue)) {
                return true;
            }
        }
        return false;
    }

    private static String jsonSchemaTypeOfConst(Object value, String json) {
        if ("null".equals(json)) {
            return "null";
        }
        if (value instanceof Boolean) {
            return "boolean";
        }
        if (value instanceof Number) {
            try {
                return new BigDecimal(value.toString()).stripTrailingZeros().scale() <= 0
                        ? "integer" : "number";
            } catch (NumberFormatException ignored) {
                return "number";
            }
        }
        if (value instanceof String) {
            return "string";
        }
        if (value instanceof List) {
            return "array";
        }
        if (value instanceof Map) {
            return "object";
        }
        return null;
    }

    private static boolean hasConstConstraint(Schema schema) {
        return schema != null && (schema.getConst() != null
                || Oas31RawSpecRecovery.hasExplicitConst(schema));
    }

    private static void copyConstConstraint(Schema source, Schema target) {
        if (source.getConst() != null) {
            target.setConst(source.getConst());
        }
        if (Oas31RawSpecRecovery.hasExplicitConst(source)) {
            Oas31RawSpecRecovery.restoreExplicitConst(
                    target, Oas31RawSpecRecovery.constJsonOf(source));
        }
    }

    /**
     * Returns the tighter (larger) of two min bounds, or whichever is non-null.
     */
    private static Integer tighterMinLen(Integer first, Integer second) {
        if (first == null) return second;
        if (second == null) return first;
        return Math.max(first, second);
    }

    /**
     * Returns the tighter (smaller) of two max bounds, or whichever is non-null.
     */
    private static Integer tighterMaxLen(Integer first, Integer second) {
        if (first == null) return second;
        if (second == null) return first;
        return Math.min(first, second);
    }

    /**
     * Builds a synthetic object Schema from an AllOfIntersection result.
     * The synthetic schema is used as input to super.fromModel, replacing
     * the original allOf structure with pre-computed merged properties
     * and required sets.
     *
     * @param schemaName   the model name
     * @param intersection the pre-computed allOf intersection
     * @return a synthetic object Schema with merged properties and required
     */
    static Schema buildSyntheticAllOfSchema(
            String schemaName, AllOfIntersection intersection) {
        Schema synthetic = new Schema();

        String rootType = intersection.getRootScalarType();
        if (rootType == null && intersection.hasRootConst()) {
            rootType = jsonSchemaTypeOfConst(
                    intersection.getRootConstValue(), intersection.getRootConstJson());
        }
        synthetic.setType(rootType != null ? rootType : "object");
        if (!"null".equals(rootType) && intersection.allowsRootNull()) {
            synthetic.setNullable(true);
        }

        // Apply intersected root-level enum values
        if (intersection.getRootEnumValues() != null
                && !intersection.getRootEnumValues().isEmpty()) {
            synthetic.setEnum(new ArrayList<>(intersection.getRootEnumValues()));
        }

        // Apply intersected root-level const value, including an explicit JSON null.
        if (intersection.hasRootConst()) {
            synthetic.setConst(intersection.getRootConstValue());
            Oas31RawSpecRecovery.restoreExplicitConst(
                    synthetic, intersection.getRootConstJson());
        }

        // Apply intersected numeric bounds
        if (intersection.getRootMinimum() != null) {
            synthetic.setMinimum(intersection.getRootMinimum());
        }
        if (intersection.getRootMaximum() != null) {
            synthetic.setMaximum(intersection.getRootMaximum());
        }
        if (intersection.getRootExclusiveMinimum() != null) {
            synthetic.setExclusiveMinimum(intersection.getRootExclusiveMinimum());
        }
        if (intersection.getRootExclusiveMaximum() != null) {
            synthetic.setExclusiveMaximum(intersection.getRootExclusiveMaximum());
        }
        if (intersection.getRootExclusiveMinimumValue() != null) {
            synthetic.setExclusiveMinimumValue(intersection.getRootExclusiveMinimumValue());
        }
        if (intersection.getRootExclusiveMaximumValue() != null) {
            synthetic.setExclusiveMaximumValue(intersection.getRootExclusiveMaximumValue());
        }

        // Apply intersected string length bounds
        if (intersection.getRootMinLength() != null) {
            synthetic.setMinLength(intersection.getRootMinLength());
        }
        if (intersection.getRootMaxLength() != null) {
            synthetic.setMaxLength(intersection.getRootMaxLength());
        }

        // Copy merged properties (skipping optional-impossible properties)
        if (!intersection.getProperties().isEmpty()) {
            Map<String, Schema> syntheticProps = new LinkedHashMap<>();
            for (Map.Entry<String, Schema> propEntry
                    : intersection.getProperties().entrySet()) {
                String propName = propEntry.getKey();
                if (intersection.getOptionalImpossibleProperties().contains(propName)) {
                    // For optional-impossible properties (e.g., string ∩ int32),
                    // use the first contributor's schema so the property has a
                    // storage member (avoids empty-shell detection). Mark with
                    // x-cpp-optional-impossible for template-level awareness.
                    Schema propSchema = propEntry.getValue();
                    // The intersected schema may have x-cpp-unsatisfiable set.
                    // Ensure it has at least one contributor type so fromModel
                    // produces a CodegenProperty with a real dataType. Fall back
                    // to the existing intersected schema as-is when it already
                    // has a type or if no better alternative is available.
                    if (propSchema.getType() == null) {
                        // Assign a fallback type so the property gets a member.
                        // Prefer the first contributor's type, otherwise use
                        // boost::json::value as the most generic C++ type.
                        propSchema.setType("string");
                    }
                    Map<String, Object> ext = propSchema.getExtensions();
                    if (ext == null) {
                        ext = new LinkedHashMap<>();
                        propSchema.setExtensions(ext);
                    }
                    ext.put("x-cpp-optional-impossible", true);
                    syntheticProps.put(propName, propSchema);
                } else {
                    syntheticProps.put(propName, propEntry.getValue());
                }
            }
            synthetic.setProperties(syntheticProps);
        }

        if (intersection.getAdditionalProperties() != null) {
            synthetic.setAdditionalProperties(intersection.getAdditionalProperties());
        }

        // Set required as the union of required from all contributors
        if (!intersection.getRequired().isEmpty()) {
            synthetic.setRequired(new ArrayList<>(intersection.getRequired()));
        }

        return synthetic;
    }

    /**
     * Builds a list of {key, value} maps from the full set of discriminator
     * mapped models (explicit URI mappings + implicit component-name mappings)
     * for template-iteration.  Each entry maps a C++-escaped discriminator value
     * to a composition branch index so the template can reorder candidate
     * validation for diagnostics.
     * <p>
     * Unresolvable mappings (where the model name does not match any branch
     * resolved schema name) fail generation with a clear diagnostic per §8.
     *
     * @param mappedModels the full set of discriminator mapped models
     * @param branches     the composition branch descriptors
     * @return list of {key, value} maps; non-empty when at least one mapping
     *         resolves to a valid branch
     * @throws RuntimeException when a mapping does not resolve to any branch
     */
    public static List<Map<String, Object>> buildDiscriminatorBranchIndex(
            Set<CodegenDiscriminator.MappedModel> mappedModels,
            List<CompositionBranchDescriptor> branches) {
        List<Map<String, Object>> indexList = new ArrayList<>();
        if (mappedModels == null || mappedModels.isEmpty()) return indexList;
        for (CodegenDiscriminator.MappedModel mm : mappedModels) {
            if (mm == null) continue;
            int branchIndex = -1;
            for (int bi = 0; bi < branches.size(); bi++) {
                String resolvedName = branches.get(bi).getResolvedSchemaName();
                if (resolvedName == null) continue;
                // Match on raw schemaName first (handles lowercase/raw names),
                // then on sanitized modelName (handles normalised names).
                if (resolvedName.equals(mm.getSchemaName())
                        || resolvedName.equals(mm.getModelName())) {
                    branchIndex = bi;
                    break;
                }
            }
            if (branchIndex >= 0) {
                Map<String, Object> entry = new LinkedHashMap<>();
                entry.put("key", CppBoostBeastClientCodegen.escapeCppStringContent(mm.getMappingName()));
                entry.put("value", branchIndex);
                indexList.add(entry);
            } else {
                // §8: unresolvable → hard diagnostic
                throw new RuntimeException(
                    "Discriminator mapping value '"
                    + CppBoostBeastClientCodegen.escapeCppStringContent(mm.getMappingName())
                    + "' (schema: " + mm.getSchemaName()
                    + ", model: " + mm.getModelName()
                    + ") does not match any composition branch for schema '"
                    + (mm.getModelName() != null ? mm.getModelName() : "(unknown)")
                    + "'. Valid branches: "
                    + branches.stream()
                        .map(CompositionBranchDescriptor::getResolvedSchemaName)
                        .filter(n -> n != null)
                        .collect(Collectors.joining(", ")));
            }
        }
        return indexList;
    }

    /**
     * Fallback variant: builds a list of {key, value} maps from explicit
     * discriminator mapping entries only (used when the codegen model's
     * full MappedModel set is unavailable).
     *
     * @param discMapping the discriminator.value → target mapping
     * @param branches    the composition branch descriptors
     * @return list of {key, value} maps
     */
    public static List<Map<String, Object>> buildDiscriminatorBranchIndex(
            Map<String, String> discMapping,
            List<CompositionBranchDescriptor> branches) {
        List<Map<String, Object>> indexList = new ArrayList<>();
        if (discMapping == null || discMapping.isEmpty()) return indexList;
        for (Map.Entry<String, String> entry : discMapping.entrySet()) {
            String targetName = extractSimpleRef(entry.getValue());
            if (targetName == null) continue;
            int branchIndex = -1;
            for (int bi = 0; bi < branches.size(); bi++) {
                if (targetName.equals(branches.get(bi).getResolvedSchemaName())) {
                    branchIndex = bi;
                    break;
                }
            }
            if (branchIndex >= 0) {
                Map<String, Object> entryMap = new LinkedHashMap<>();
                entryMap.put("key", CppBoostBeastClientCodegen.escapeCppStringContent(entry.getKey()));
                entryMap.put("value", branchIndex);
                indexList.add(entryMap);
            } else {
                throw new RuntimeException(
                    "Discriminator mapping target '" + entry.getValue()
                    + "' (resolved: " + targetName
                    + ") does not match any composition branch. Valid branches: "
                    + branches.stream()
                        .map(CompositionBranchDescriptor::getResolvedSchemaName)
                        .filter(n -> n != null)
                        .collect(Collectors.joining(", ")));
            }
        }
        return indexList;
    }

    /**
     * Extracts a simple schema name from a discriminator mapping value.
     * Handles both URI references (e.g. "#/components/schemas/Mammal")
     * and plain component names (e.g. "Mammal").
     */
    private static String extractSimpleRef(String mappingValue) {
        if (mappingValue == null || mappingValue.isEmpty()) return null;
        String ref = mappingValue.trim();
        if (ref.startsWith("#/")) {
            int lastSlash = ref.lastIndexOf('/');
            return lastSlash >= 0 ? ref.substring(lastSlash + 1) : ref;
        }
        return ref;
    }

    /**
     * Ordered lowering rules for composed types (OAS-first):
     * 1. anyOf/oneOf: [T, null] → std::optional&lt;T&gt;
     * 2. anyOf only: all strings/string-enums → std::string
     * 3. Remove null branches
     * 4. Single non-null branch → that branch's type
     * 5. Deduplicate identical branch types
     * 6. oneOf open-string + string-enum (type-erased) → boost::json::value
     *    (do not pretend exclusivity after both erase to std::string)
     * 7. oneOf multi-branch → single identical C++ type (alias collapse) → that type
     * 8. Emit std::variant&lt;Branches...&gt; or boost::json::value
     * <p>
     * When a non-null {@code descriptor} is provided, its branch metadata
     * (nullCapability, supportedAssertions) replaces C++ type-string heuristics
     * for Rules 1, 3, and 6.
     * Warnings are routed through {@code warningSink} so this stateless helper
     * does not retain a process-wide logger.
     */
    static String lowerComposedTypes(List<CppBoostBeastClientCodegen.ComposedBranch> branches,
                                     String composedKeyword,
                                     CompositionDescriptor descriptor,
                                     Consumer<String> warningSink) {
        if (branches == null || branches.isEmpty()) {
            return "boost::json::value";
        }
        List<String> branchTypes = branches.stream()
                .map(b -> b.cppType)
                .collect(Collectors.toList());

        // Rule 1: anyOf/oneOf: [T, null] → std::optional<T>
        // Use descriptor nullCapability when available for semantic accuracy.
        // Uses originalBranchIndex to align with descriptor after self-ref filtering.
        // Tightened: non-null branch must have NullCapability.NEVER (not CONDITIONAL).
        if (descriptor != null) {
            int alwaysNullCount = 0;
            int nonNullComposedIndex = -1;
            List<CompositionBranchDescriptor> descBranches = descriptor.getBranches();
            for (int ci = 0; ci < branches.size(); ci++) {
                int descIdx = branches.get(ci).originalBranchIndex;
                if (descIdx < 0 || descIdx >= descBranches.size()) continue;
                CompositionBranchDescriptor.NullCapability nc =
                        descBranches.get(descIdx).getNullCapability();
                if (nc == CompositionBranchDescriptor.NullCapability.ALWAYS) {
                    alwaysNullCount++;
                } else if (nc == CompositionBranchDescriptor.NullCapability.NEVER
                        && nonNullComposedIndex < 0) {
                    nonNullComposedIndex = ci;
                }
            }
            if (alwaysNullCount == 1 && branches.size() == 2
                    && nonNullComposedIndex >= 0
                    && nonNullComposedIndex < branchTypes.size()) {
                String nonNullBranch = branchTypes.get(nonNullComposedIndex);
                if (nonNullBranch != null) {
                    return "std::optional<" + nonNullBranch + ">";
                }
            }
        } else {
            // Fallback: C++ type-string heuristic (no descriptor available)
            int nullCount = (int) branchTypes.stream().filter("std::nullptr_t"::equals).count();
            if (nullCount == 1 && branchTypes.size() == 2) {
                String nonNullBranch = branchTypes.stream()
                        .filter(bt -> !"std::nullptr_t".equals(bt))
                        .findFirst().orElse(null);
                if (nonNullBranch != null) {
                    return "std::optional<" + nonNullBranch + ">";
                }
            }
        }

        // Rule 2: anyOf-only collapse of unconstrained string branches.
        // Enum constraints require distinct branch validators, and oneOf cannot
        // collapse without losing exclusive-match semantics.
        if ("anyOf".equals(composedKeyword) && branchTypes.stream().allMatch("std::string"::equals)) {
            // Check if any branch has enum assertions using descriptor metadata
            // or fallback ComposedBranch isEnum flag.
            boolean hasEnumString = false;
            if (descriptor != null) {
                List<CompositionBranchDescriptor> descBranches = descriptor.getBranches();
                for (CppBoostBeastClientCodegen.ComposedBranch cb : branches) {
                    int descIdx = cb.originalBranchIndex;
                    if (descIdx >= 0 && descIdx < descBranches.size()
                            && descBranches.get(descIdx).getSupportedAssertions().contains("enum")) {
                        hasEnumString = true;
                        break;
                    }
                }
            } else {
                hasEnumString = branches.stream().anyMatch(b -> b.isEnum);
            }
            if (!hasEnumString) {
                return "std::string";
            }
            // Has enum string branches — fall through to CompositionBranchValue
            // preservation (Rule 5) which keeps validators active.
        }

        // Rule 3: Remove null branches for further processing, preserving all
        // branches when every branch is null so oneOf cardinality remains exact.
        List<CppBoostBeastClientCodegen.ComposedBranch> nonNullMeta;
        if (descriptor != null) {
            List<CompositionBranchDescriptor> descBranches = descriptor.getBranches();
            nonNullMeta = new ArrayList<>();
            boolean hasNonNull = false;
            for (CppBoostBeastClientCodegen.ComposedBranch cb : branches) {
                int descIdx = cb.originalBranchIndex;
                if (descIdx >= 0 && descIdx < descBranches.size()) {
                    CompositionBranchDescriptor.NullCapability nc =
                            descBranches.get(descIdx).getNullCapability();
                    if (nc != CompositionBranchDescriptor.NullCapability.ALWAYS) {
                        nonNullMeta.add(cb);
                        hasNonNull = true;
                    }
                }
            }
            // All branches were null — keep them for identity preservation
            if (!hasNonNull && !branches.isEmpty()) {
                nonNullMeta = new ArrayList<>(branches);
            }
        } else {
            List<CppBoostBeastClientCodegen.ComposedBranch> nonNullOnly = branches.stream()
                    .filter(b -> !"std::nullptr_t".equals(b.cppType))
                    .collect(Collectors.toList());
            if (!nonNullOnly.isEmpty()) {
                nonNullMeta = nonNullOnly;
            } else {
                // All branches are null — keep them
                nonNullMeta = new ArrayList<>(branches);
            }
        }
        List<String> nonNullBranches = nonNullMeta.stream()
                .map(b -> b.cppType)
                .collect(Collectors.toList());

        // Rule 3b: Preserve nested variants as outer alternatives. Branch
        // conversion parses each descriptor branch into its declared C++ type;
        // flattening that type would make the converted value unassignable and
        // would erase the nested composition's branch boundary.

        // Rule 4: All-null or empty → boost::json::value
        if (nonNullBranches.isEmpty()) {
            return "boost::json::value";
        }

        // Rule 5: Detect duplicate branch types that would lose schema
        // identity after C++ dedup. When multiple branches lower to the
        // same C++ type (e.g., two double branches with different numeric
        // constraints, or a string + string-enum both becoming std::string),
        // wrap each in CompositionBranchValue<originalBranchIndex, Type>
        // to preserve distinct branch identity.
        boolean hasDuplicateTypes = false;
        outer:
        for (int i = 0; i < nonNullBranches.size(); i++) {
            for (int j = i + 1; j < nonNullBranches.size(); j++) {
                if (nonNullBranches.get(i).equals(nonNullBranches.get(j))) {
                    hasDuplicateTypes = true;
                    break outer;
                }
            }
        }

        if (hasDuplicateTypes) {
            // Shortcut: wrap all branches in CompositionBranchValue to
            // preserve identity. Nested variants remain intact so the wrapper
            // type exactly matches the descriptor branch conversion type.
            // Also skip Rule 6 (string exclusivity) since tagged branches
            // already preserve distinct membership.
            List<String> tagged = new ArrayList<>();
            for (int i = 0; i < nonNullBranches.size(); i++) {
                String rawType = nonNullBranches.get(i);
                int origIdx = nonNullMeta.get(i).originalBranchIndex;
                // For inline schemas (origIdx < 0), use flat position as tag.
                int brIdx = origIdx >= 0 ? origIdx : i;
                tagged.add("CompositionBranchValue<" + brIdx
                        + ", " + rawType + ">");
            }
            // When hasDuplicateTypes, null branches must be wrapped in
            // CompositionBranchValue too — never bare std::nullptr_t.
            // Find null branches that were filtered by Rule 3 and wrap
            // them, skipping any that Rule 3 already preserved in tagged.
            boolean hasNull = branchTypes.stream().anyMatch("std::nullptr_t"::equals);
            if (hasNull) {
                for (int ni = 0; ni < branches.size(); ni++) {
                    if ("std::nullptr_t".equals(branches.get(ni).cppType)) {
                        int origIdx = branches.get(ni).originalBranchIndex;
                        int brIdx = origIdx >= 0 ? origIdx : ni;
                        String cbvNull = "CompositionBranchValue<" + brIdx
                                + ", std::nullptr_t>";
                        if (!tagged.contains(cbvNull)) {
                            tagged.add(cbvNull);
                        }
                    }
                }
            }
            return "std::variant<" + String.join(", ", tagged) + ">";
        }

        // Rule 6: Deduplicate identical branch types (safe when no duplicates).
        List<String> deduped = nonNullBranches.stream()
                .distinct()
                .collect(Collectors.toList());

        // Rule 7: oneOf string branches that lose exclusivity after type lowering.
        // Branches [open-string, string-enum] or [string-enum-A, string-enum-B] all
        // collapse to std::string after type lowering, so every string value matches
        // every original string-like branch. Under JSON Schema oneOf, this means
        // values matching multiple original branches cannot be detected (count is
        // artificially 1 instead of 2+), causing false acceptance of invalid oneOf
        // inputs. Type-erase to boost::json::value when multiple string-like branches
        // collapse and at least one has enum constraints (the constraint is the only
        // thing that distinguishes otherwise-identical branches). anyOf keeps the
        // string collapse (rule 2) since first-match is correct behavior.
        //
        // When a descriptor is available, use its supportedAssertions for enum
        // detection instead of the ComposedBranch isEnum flag. Descriptor
        // assertions are semantically richer (captured from raw schema scanning)
        // and carried from preprocessOpenAPI through all lowering passes.
        if ("oneOf".equals(composedKeyword) && nonNullMeta.size() > 1) {
            long preDedupStringCount = nonNullMeta.stream()
                    .filter(b -> b.isStringLike)
                    .count();
            long postDedupStringCount = deduped.stream()
                    .filter("std::string"::equals)
                    .count();
            List<CompositionBranchDescriptor> descBranches = descriptor != null
                    ? descriptor.getBranches() : null;
            boolean hasStringEnum = nonNullMeta.stream()
                    .anyMatch(b -> {
                        if (!b.isStringLike) return false;
                        // Descriptor path: consult supportedAssertions
                        if (descBranches != null && b.originalBranchIndex >= 0
                                && b.originalBranchIndex < descBranches.size()) {
                            return descBranches.get(b.originalBranchIndex)
                                    .getSupportedAssertions().contains("enum");
                        }
                        // Fallback: use ComposedBranch.isEnum (CodegenProperty)
                        return b.isEnum;
                    });
            if (preDedupStringCount > postDedupStringCount && hasStringEnum) {
                warningSink.accept(
                        "oneOf string branches erase to std::string; "
                                + "emitting boost::json::value to avoid false exclusive-union fidelity");
                return "boost::json::value";
            }
        }

        // Rule 7: A single non-null type can still be nullable after the
        // duplicate null branches were removed. anyOf can use optional storage;
        // oneOf retains an explicit null alternative so validation can reject
        // duplicate-null matches before accepting a value.
        if (deduped.size() == 1) {
            boolean hasNull = branchTypes.stream().anyMatch("std::nullptr_t"::equals);
            if (hasNull) {
                if ("anyOf".equals(composedKeyword)) {
                    return "std::optional<" + deduped.get(0) + ">";
                }
                return "std::variant<" + deduped.get(0) + ", std::nullptr_t>";
            }
            return deduped.get(0);
        }

        // Rule 8: Emit std::variant<Branches...>
        List<String> variantBranches = new ArrayList<>(deduped);
        // Re-append null for any null-containing composition not consumed
        // by Rule 1 ([T, null] -> optional<T>). Rule 1 always returns early,
        // so every null surviving to this point must be restored.
        boolean hasNull = branchTypes.stream().anyMatch("std::nullptr_t"::equals);
        boolean nullsAlreadyPreserved = variantBranches.stream().anyMatch(
                v -> v.contains("std::nullptr_t"));
        if (hasNull && !nullsAlreadyPreserved) {
            variantBranches.add("std::nullptr_t");
        }
        return "std::variant<" + String.join(", ", variantBranches) + ">";
    }
}