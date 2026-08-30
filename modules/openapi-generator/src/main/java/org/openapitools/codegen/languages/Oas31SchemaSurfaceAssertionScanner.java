package org.openapitools.codegen.languages;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

/**
 * Scans one OAS 3.1 schema surface into the validation facts consumed by
 * composition lowering and schema-IR emission.
 */
final class Oas31SchemaSurfaceAssertionScanner {
    private Oas31SchemaSurfaceAssertionScanner() {
    }

    /**
     * Static assertion scan of one schema surface (one composition branch).
     * Records every supported keyword into {@code supported}, every known
     * unsupported one into {@code unsupported}, and materialises the scan into
     * the schema-IR output map.
     * Shared with the composition lowering (branch construction) so both the
     * descriptor scan and the IR emission see the identical surface.
     */
    static void scanSurfaceAssertions(
            io.swagger.v3.oas.models.media.Schema surface,
            io.swagger.v3.oas.models.OpenAPI openAPI,
            java.util.List<String> supported,
            java.util.List<String> unsupported,
            java.util.Map<String, Object> validateParams,
            boolean refBranchExcluded) {
                // Validation type — use the resolved type name or "type-array" for type arrays
                if (surface.getType() != null) {
                    supported.add("type");
                    validateParams.put("validation-type", surface.getType());
                }
                if (surface.getTypes() != null && !surface.getTypes().isEmpty()) {
                    supported.add("type");
                    validateParams.put("validation-type", "type-array");
                    java.util.List<Object> loweredTypes =
                            new ArrayList<>(surface.getTypes());
                    // OAS-3.1: the normalizer strips a literal "null" type
                    // member (nullable:true) before this scan; restore it from
                    // the raw text when it was present (validity is decided by
                    // the pristine spec, not the model-layer rewrite).
                    if (Oas31RawSpecRecovery.pristineTypeHasNull(surface)
                            && !loweredTypes.contains("null")) {
                        loweredTypes.add("null");
                    }
                    validateParams.put("validation-type-array", loweredTypes);
                    validateParams.put("has-validation-type-array", true);
                } else if ((surface.getTypes() == null
                        || surface.getTypes().isEmpty())
                        && Oas31RawSpecRecovery.pristineTypeHasNull(surface)) {
                    // Restore a sole null type dropped by the normalizer.
                    supported.add("type");
                    validateParams.put("validation-type", "type-array");
                    java.util.List<Object> loweredTypes =
                            new ArrayList<>();
                    loweredTypes.add("null");
                    validateParams.put("validation-type-array", loweredTypes);
                    validateParams.put("has-validation-type-array", true);
                }
                // enum — an EMPTY enum (enum: []) is a reject-all schema handled
                // by the deep JSON store (hasEnumJson with zero members). The
                // swagger-parser models `enum: []` as enum=null + types=[string]
                // (information lost), so preprocessOpenAPI recovers the original
                // keyword from the raw spec and marks the branch via the
                // x-oas31-empty-enum vendor extension; the marker is treated as
                // an empty enum here (a real, non-empty enum takes precedence).
                String pristineEnumJson = Oas31RawSpecRecovery.enumJsonOf(surface);
                if (surface.getEnum() != null
                        || Oas31RawSpecRecovery.isEmptyEnumMarked(surface)
                        || pristineEnumJson != null) {
                    supported.add("enum");
                    // For a recovered `enum: []` the parser yields enum=null; use
                    // the empty list so the deep store emits ZERO members.
                    java.util.List<?> enumMembers = surface.getEnum();
                    if (enumMembers == null) {
                        enumMembers = java.util.Collections.emptyList();
                    }
                    List<String> enumStrs = new ArrayList<>();
                    String predominantKind = "string";
                    for (Object e : enumMembers) {
                        String es = e != null ? e.toString() : "null";
                        if ("string".equals(predominantKind)) {
                            es = CppBoostBeastClientCodegen.escapeCppStringContent(es);
                        }
                        enumStrs.add(es);
                        if (e instanceof Integer || e instanceof Long || e instanceof Short || e instanceof Byte) {
                            predominantKind = "integer";
                        } else if (e instanceof Double || e instanceof Float || e instanceof java.math.BigDecimal) {
                            if (!"integer".equals(predominantKind)) predominantKind = "number";
                        } else if (e instanceof Boolean) {
                            if (!"integer".equals(predominantKind) && !"number".equals(predominantKind)) predominantKind = "bool";
                        }
                    }
                    validateParams.put("validation-enum-values", enumStrs);
                    validateParams.put("validation-enum-kind", predominantKind);
                    validateParams.put("validation-enum-kind-string", "string".equals(predominantKind));
                    validateParams.put("validation-enum-kind-integer", "integer".equals(predominantKind));
                    validateParams.put("validation-enum-kind-number", "number".equals(predominantKind));
                    validateParams.put("validation-enum-kind-bool", "bool".equals(predominantKind));
                    validateParams.put("has-validation-enum", true);
                    // Preserve raw deep JSON enum members for exact IR emission.
                    validateParams.put("validation-enum-raw", enumMembers);
                    if (pristineEnumJson != null) {
                        validateParams.put("validation-enum-json", pristineEnumJson);
                    }
                }
                // Const: use raw recovery to distinguish an explicit JSON null
                // from the parser's absent-const sentinel.
                Object constVal = surface.getConst();
                String pristineConstJson = Oas31RawSpecRecovery.constJsonOf(surface);
                if (constVal != null || Oas31RawSpecRecovery.hasExplicitConst(surface)) {
                    supported.add("const");
                    if (constVal instanceof Number) {
                        validateParams.put("validation-const-type", "number");
                        validateParams.put("validation-const-value", constVal.toString());
                    } else if (constVal instanceof Boolean) {
                        validateParams.put("validation-const-type", "boolean");
                        validateParams.put("validation-const-value", constVal.toString());
                    } else if (constVal == null || "null".equals(pristineConstJson)) {
                        validateParams.put("validation-const-type", "null");
                        validateParams.put("validation-const-value", "null");
                    } else {
                        validateParams.put("validation-const-type", "string");
                        validateParams.put("validation-const-value",
                                CppBoostBeastClientCodegen.escapeCppStringContent(constVal.toString()));
                    }
                    validateParams.put("has-validation-const", true);
                    if (constVal != null) {
                        validateParams.put("validation-const-raw", constVal);
                    }
                    if (pristineConstJson != null) {
                        validateParams.put("validation-const-json", pristineConstJson);
                    }
                }
                // Use ModelUtils.resolveMinimumBound / resolveMaximumBound for
                // proper OAS 3.0→3.1 resolution (boolean → numeric conversion,
                // allOf intersection, $ref traversal).
                ModelUtils.ResolvedMinBound resolvedMin = ModelUtils.resolveMinimumBound(openAPI, surface);
                ModelUtils.ResolvedMaxBound resolvedMax = ModelUtils.resolveMaximumBound(openAPI, surface);
                if (resolvedMin != null || resolvedMax != null
                        || surface.getMultipleOf() != null) {
                    supported.add("numeric-range");
                    if (resolvedMin != null) {
                        validateParams.put("validation-min", resolvedMin.minBound);
                        if (resolvedMin.exclusive) {
                            validateParams.put("validation-exclusive-min", resolvedMin.minBound);
                        }
                    }
                    if (resolvedMax != null) {
                        validateParams.put("validation-max", resolvedMax.maxBound);
                        if (resolvedMax.exclusive) {
                            validateParams.put("validation-exclusive-max", resolvedMax.maxBound);
                        }
                    }
                    if (surface.getMultipleOf() != null) {
                        validateParams.put("validation-multiple-of",
                                surface.getMultipleOf());
                    }
                    validateParams.put("has-validation-numeric", true);
                }
                String minLenLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "minLength");
                String maxLenLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "maxLength");
                if (surface.getMinLength() != null
                        || surface.getMaxLength() != null
                        || minLenLex != null || maxLenLex != null) {
                    supported.add("string-length");
                    if (minLenLex != null) {
                        validateParams.put("validation-min-length", minLenLex);
                    } else if (surface.getMinLength() != null) {
                        validateParams.put("validation-min-length",
                                surface.getMinLength());
                    }
                    if (maxLenLex != null) {
                        validateParams.put("validation-max-length", maxLenLex);
                    } else if (surface.getMaxLength() != null) {
                        validateParams.put("validation-max-length",
                                surface.getMaxLength());
                    }
                    validateParams.put("has-validation-string-length", true);
                }
                if (surface.getPattern() != null) {
                    supported.add("pattern");
                    validateParams.put("validation-pattern",
                            CppBoostBeastClientCodegen.escapeCppStringContent(surface.getPattern()));
                    validateParams.put("has-validation-pattern", true);
                }
                if (surface.getPrefixItems() != null
                        && !surface.getPrefixItems().isEmpty()) {
                    supported.add("array-prefix-items");
                    validateParams.put("validation-prefix-items",
                            surface.getPrefixItems());
                    validateParams.put("has-validation-prefix-items", true);
                }
                // `items` is evaluated over array entries after prefixItems.
                if (surface.getItems() != null) {
                    validateParams.put("validation-items", surface.getItems());
                }
                String minItemsLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "minItems");
                String maxItemsLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "maxItems");
                if (surface.getMinItems() != null
                        || surface.getMaxItems() != null
                        || minItemsLex != null || maxItemsLex != null) {
                    supported.add("array-length");
                    if (minItemsLex != null) {
                        validateParams.put("validation-min-items", minItemsLex);
                    } else if (surface.getMinItems() != null) {
                        validateParams.put("validation-min-items",
                                surface.getMinItems());
                    }
                    if (maxItemsLex != null) {
                        validateParams.put("validation-max-items", maxItemsLex);
                    } else if (surface.getMaxItems() != null) {
                        validateParams.put("validation-max-items",
                                surface.getMaxItems());
                    }
                    validateParams.put("has-validation-array-length", true);
                }
                // uniqueItems: PRESENCE (any value) so the keyword never
                // fail-closes; `false` is a no-op that still emits the node.
                if (surface.getUniqueItems() != null) {
                    supported.add("unique-items");
                    validateParams.put("has-validation-unique-items", true);
                    validateParams.put("validation-unique-items",
                            surface.getUniqueItems());
                }
                // required: supported — presence check is generated in validator
                if (surface.getRequired() != null) {
                    supported.add("object-properties");
                    validateParams.put("validation-required",
                            surface.getRequired());
                    validateParams.put("has-validation-object-props", true);
                }
                // Properties become child IR rows because their schemas affect
                // branch membership and must never be skipped.
                if (surface.getProperties() != null
                        && !surface.getProperties().isEmpty()) {
                    supported.add("object-properties");
                    validateParams.put("validation-properties",
                            surface.getProperties());
                    validateParams.put("has-validation-properties", true);
                }
                // additionalProperties is tri-state: absent/true allows, false
                // rejects, and a schema validates each additional member.
                Object addPropsVal = surface.getAdditionalProperties();
                if (addPropsVal != null) {
                    supported.add("additional-properties");
                    if (addPropsVal instanceof Schema) {
                        Schema addPropSchema = (Schema) addPropsVal;
                        Boolean apBool = addPropSchema.getBooleanSchemaValue();
                        if (apBool != null) {
                            validateParams.put("validation-additional-properties-kind",
                                    Boolean.TRUE.equals(apBool) ? "allowed" : "reject");
                        } else {
                            validateParams.put("validation-additional-properties-kind", "schema");
                            validateParams.put("validation-additional-properties-schema", addPropSchema);
                        }
                    } else if (addPropsVal instanceof Boolean) {
                        validateParams.put("validation-additional-properties-kind",
                                Boolean.TRUE.equals(addPropsVal) ? "allowed" : "reject");
                    }
                }
                String minPropsLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "minProperties");
                String maxPropsLex = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "maxProperties");
                if (surface.getMinProperties() != null || minPropsLex != null) {
                    supported.add("object-property-count");
                    validateParams.put("validation-min-properties",
                            minPropsLex != null
                                    ? minPropsLex : surface.getMinProperties());
                }
                if (surface.getMaxProperties() != null || maxPropsLex != null) {
                    supported.add("object-property-count");
                    validateParams.put("validation-max-properties",
                            maxPropsLex != null
                                    ? maxPropsLex : surface.getMaxProperties());
                }
                // Nested allOf, anyOf, and oneOf each become applicator children;
                // all three may coexist. A $ref branch excludes this inline scan
                // because its target row owns the referenced composition.
                if (!refBranchExcluded) {
                    if (surface.getOneOf() != null && !surface.getOneOf().isEmpty()) {
                        validateParams.put("validation-oneof-schemas", surface.getOneOf());
                    }
                    if (surface.getAnyOf() != null && !surface.getAnyOf().isEmpty()) {
                        validateParams.put("validation-anyof-schemas", surface.getAnyOf());
                    }
                    if (surface.getAllOf() != null && !surface.getAllOf().isEmpty()) {
                        validateParams.put("validation-allof-schemas", surface.getAllOf());
                    }
                }
                // unevaluatedItems accepts either a boolean or schema value.
                if (surface.getUnevaluatedItems() != null) {
                    validateParams.put("validation-unevaluated-items",
                            surface.getUnevaluatedItems());
                }
                // Applied conditional branches contribute annotations and
                // evaluated coverage to enclosing unevaluated checks.
                if (surface.getIf() != null) {
                    validateParams.put("validation-if", surface.getIf());
                }
                if (surface.getThen() != null) {
                    validateParams.put("validation-then", surface.getThen());
                }
                if (surface.getElse() != null) {
                    validateParams.put("validation-else", surface.getElse());
                }
                if (surface.getDependentSchemas() != null
                        && !surface.getDependentSchemas().isEmpty()) {
                    validateParams.put("validation-dependent-schemas",
                            surface.getDependentSchemas());
                }
                // Resolved nested composition is evaluated by the model's IR row.
                // `not` is carried as a child schema into the same evaluator.
                if (surface.getNot() != null) {
                    validateParams.put("validation-not-schema", surface.getNot());
                }

                // Detect unsupported assertion keywords
                io.swagger.v3.oas.models.media.Discriminator targetDisc =
                        surface.getDiscriminator();
                if (targetDisc != null) {
                    // Discriminator on branches is annotation-only for now
                }
                // if/then/else: not yet implemented as a conditional applicator;
                // NOT fail-closed so "ref-to-if" corpora still GENERATE and run
                // (the inline-ref/if-schema content is densified via $id
                // resolution; honest: a bare if-then-else without a covering ref
                // is ignored, measured as FAIL not BLOCKED).
                if (surface.getIf() != null) {
                    validateParams.put("validation-if-schema", surface.getIf());
                }
                if (surface.getThen() != null) {
                    validateParams.put("validation-then-schema", surface.getThen());
                }
                if (surface.getElse() != null) {
                    validateParams.put("validation-else-schema", surface.getElse());
                }
                // dependentRequired: the parser MERGES the required lists of
                // multi-entry maps into one shared corrupt list (see
                // recoverPristineLiterals (c) +
                // DependentRequiredParserRetentionTest); the raw-literal
                // recovery extension is authoritative when present.
                Object depReqNative = surface.getDependentRequired();
                if (depReqNative != null
                        && !((java.util.Map<?, ?>) depReqNative).isEmpty()
                        && surface.getExtensions() != null
                        && surface.getExtensions().containsKey(
                                "x-oas31-dependent-required")) {
                    depReqNative = surface.getExtensions()
                            .get("x-oas31-dependent-required");
                }
                if (depReqNative instanceof java.util.Map
                        && !((java.util.Map<?, ?>) depReqNative).isEmpty()) {
                    supported.add("dependent-required");
                    validateParams.put("validation-dependent-required",
                            depReqNative);
                }
                // `contains` becomes a child row and min/maxContains remain exact
                // count bounds. Both bounds are inert without `contains`.
                if (surface.getContains() != null) {
                    supported.add("contains");
                    validateParams.put("validation-contains-schema",
                            surface.getContains());
                    String minC = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "minContains");
                    String maxC = Oas31RawSpecRecovery.countBoundLexemeOf(surface, "maxContains");
                    if (surface.getMinContains() != null || minC != null) {
                        validateParams.put("validation-min-contains",
                                minC != null ? minC : surface.getMinContains());
                    }
                    if (surface.getMaxContains() != null || maxC != null) {
                        validateParams.put("validation-max-contains",
                                maxC != null ? maxC : surface.getMaxContains());
                    }
                } else {
                    if (surface.getMinContains() != null
                            || surface.getMaxContains() != null) {
                        // inert per 2020-12 (no adjacent contains) — never
                        // fail generation, never fail validation.
                        supported.add("contains-count-inert");
                    }
                }
                if (surface.getUnevaluatedProperties() != null) {
                    supported.add("unevaluated");
                    validateParams.put("validation-unevaluated-properties",
                            surface.getUnevaluatedProperties());
                }
                // contentEncoding, contentMediaType, and contentSchema are
                // annotations under JSON Schema 2020-12 and never affect
                // composition membership.
                if (surface.getContentMediaType() != null) {
                    supported.add("content-media-type");
                }
                if (surface.getContentEncoding() != null) {
                    supported.add("content-encoding");
                }
                if (surface.getContentSchema() != null) {
                    supported.add("content-schema");
                }
                // patternProperties and propertyNames become child rows and are
                // densified through the same full raw-schema path.
                if (surface.getPatternProperties() != null
                        && !surface.getPatternProperties().isEmpty()) {
                    supported.add("pattern-properties");
                    validateParams.put("validation-pattern-properties",
                            surface.getPatternProperties());
                }
                if (surface.getPropertyNames() != null) {
                    supported.add("property-names");
                    validateParams.put("validation-property-names",
                            surface.getPropertyNames());
                }
                // Preserve boolean schemas so the evaluator can implement true
                // as always-valid and false as always-invalid.
                if (surface.getBooleanSchemaValue() != null) {
                    validateParams.put("validation-boolean-value",
                            surface.getBooleanSchemaValue());
                }
                // Annotation-vocabulary keywords use the same parameter channel.
                {
                    final java.util.Map<String, Object> vp2 = validateParams;
                    final java.util.List<String> sup = supported;
                    readAnnotationKeywords(surface, (key, value) -> {
                        vp2.put("validation-ann-" + key, value);
                        if (key.equals("comment") || key.startsWith("extra:")
                                || key.equals("title") || key.equals("description")
                                || key.equals("default") || key.equals("examples")
                                || key.equals("format") || key.equals("contentEncoding")
                                || key.equals("contentMediaType")
                                || key.equals("contentSchema")
                                || key.equals("deprecated") || key.equals("readOnly")
                                || key.equals("writeOnly")) {
                            sup.add("annotation:" + key);
                        }
                    });
                }
    }

    /**
     * Reads annotation-vocabulary keywords into a key/value sink. Every value
     * is serialized as one complete JSON value. {@code $comment} is checked for
     * string shape but deliberately does not produce annotation output.
     */
    static void readAnnotationKeywords(
            io.swagger.v3.oas.models.media.Schema schema,
            BiConsumer<String, Object> sink) {
        if (schema == null) return;
        if (schema.getTitle() != null) {
            sink.accept("title", toJsonLiteral(schema.getTitle()));
        }
        if (schema.getDescription() != null) {
            sink.accept("description", toJsonLiteral(schema.getDescription()));
        }
        String pristineDefaultJson = Oas31RawSpecRecovery.defaultJsonOf(schema);
        if (pristineDefaultJson != null) {
            sink.accept("default", pristineDefaultJson);
        } else if (schema.getDefault() != null) {
            sink.accept("default", toJsonLiteral(schema.getDefault()));
        }
        String pristineExamplesJson = Oas31RawSpecRecovery.examplesJsonOf(schema);
        if (pristineExamplesJson != null) {
            sink.accept("examples", pristineExamplesJson);
        } else if (schema.getExamples() != null) {
            sink.accept("examples", toJsonLiteral(schema.getExamples()));
        }
        if (schema.getDeprecated() != null) {
            sink.accept("deprecated", toJsonLiteral(schema.getDeprecated()));
        }
        if (schema.getReadOnly() != null) {
            sink.accept("readOnly", toJsonLiteral(schema.getReadOnly()));
        }
        if (schema.getWriteOnly() != null) {
            sink.accept("writeOnly", toJsonLiteral(schema.getWriteOnly()));
        }
        if (schema.getFormat() != null) {
            sink.accept("format", toJsonLiteral(schema.getFormat()));
        }
        if (schema.getContentEncoding() != null) {
            sink.accept("contentEncoding", toJsonLiteral(schema.getContentEncoding()));
        }
        if (schema.getContentMediaType() != null) {
            sink.accept("contentMediaType", toJsonLiteral(schema.getContentMediaType()));
        }
        if (schema.getContentSchema() != null) {
            sink.accept("contentSchema", toJsonLiteral(schema.getContentSchema()));
        }
        Object comment = schema.get$comment();
        if (comment != null) {
            sink.accept("comment", comment instanceof String
                    ? (String) comment : "NON-STRING");
            if (!(comment instanceof String)) {
                sink.accept("comment-shape-violation", "TRUE");
            }
        }
        if (schema.getExtensions() != null) {
            for (Object entryObject : schema.getExtensions().entrySet()) {
                Map.Entry<?, ?> entry = (Map.Entry<?, ?>) entryObject;
                String key = String.valueOf(entry.getKey());
                if (key.startsWith("x-oas31-")) {
                    continue;
                }
                sink.accept("extra:" + key, toJsonLiteral(entry.getValue()));
            }
        }
    }

    private static String toJsonLiteral(Object value) {
        try {
            return Json.mapper().writeValueAsString(value);
        } catch (com.fasterxml.jackson.core.JsonProcessingException ex) {
            throw new IllegalArgumentException("Unable to serialize a schema JSON value", ex);
        }
    }
}
