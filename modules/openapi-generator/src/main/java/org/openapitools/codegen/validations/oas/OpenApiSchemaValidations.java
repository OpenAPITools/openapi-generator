package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.*;
import java.util.stream.Collectors;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link Schema}
 */
class OpenApiSchemaValidations extends GenericValidator<SchemaWrapper> {
    OpenApiSchemaValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableOneOfWithPropertiesRecommendation()) {
                rules.add(ValidationRule.warn(
                        "Schema defines properties alongside oneOf.",
                        "Schemas defining properties and oneOf are not clearly defined in the OpenAPI Specification. While our tooling supports this, it may cause issues with other tools.",
                        OpenApiSchemaValidations::checkOneOfWithProperties
                ));
            }
            if (ruleConfiguration.isEnableSchemaTypeRecommendation()) {
                rules.add(ValidationRule.warn(
                        "Schema uses the 'null' type but OAS document is version 3.0.",
                        "The 'null' type is not supported in OpenAPI 3.0.x. It is supported in OpenAPI 3.1 and above. While our tooling supports this, it may cause issues with other tools.",
                        OpenApiSchemaValidations::checkNullType
                ));
            }
            if (ruleConfiguration.isEnableNullableAttributeRecommendation()) {
                rules.add(ValidationRule.warn(
                        "Schema uses the 'nullable' attribute.",
                        "The 'nullable' attribute is deprecated in OpenAPI 3.1, and may no longer be supported in future releases. Consider migrating to the 'null' type.",
                        OpenApiSchemaValidations::checkNullableAttribute
                ));
            }
            if (ruleConfiguration.isEnableInvalidTypeRecommendation()) {
                rules.add(ValidationRule.warn(
                        "Schema uses an invalid value for the 'type' attribute.",
                        "The 'type' attribute must be one of 'null', 'boolean', 'object', 'array', 'number', 'string', or 'integer'.",
                        OpenApiSchemaValidations::checkInvalidType
                ));
            }
        }
        if (ruleConfiguration.isEnableAllOfConflictingDefaultsRecommendation()) {
            rules.add(ValidationRule.warn(
                    "Schema has conflicting default values across allOf branches.",
                    "Conflicting default values detected in allOf composed schema. OpenAPI does not define precedence for defaults in allOf. Consider consolidating to a single source of truth.",
                    OpenApiSchemaValidations::checkAllOfConflictingDefaults
            ));
        }
        if (ruleConfiguration.isEnableAllOfShadowedDefaultsRecommendation()) {
            rules.add(ValidationRule.warn(
                    "Schema has a root-level default that shadows allOf branch defaults.",
                    "A root-level 'default' is defined alongside 'allOf'. Defaults in allOf branches are shadowed and will be ignored by most generators. Remove redundant nested defaults or move the default to a single location.",
                    OpenApiSchemaValidations::checkAllOfShadowedDefaults
            ));
        }
        if (ruleConfiguration.isEnableAllOfRedundantDefaultsRecommendation()) {
            rules.add(ValidationRule.warn(
                    "Schema has redundant identical default values across allOf branches.",
                    "Identical default values are repeated in multiple allOf branches. This adds noise without adding clarity. Consider defining the default only once.",
                    OpenApiSchemaValidations::checkAllOfRedundantDefaults
            ));
        }
    }

    /**
     * JSON Schema defines oneOf as a validation property which can be applied to any schema.
     * <p>
     * OpenAPI Specification is a variant of JSON Schema for which oneOf is defined as:
     * "Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema."
     * <p>
     * Where the only examples of oneOf in OpenAPI Specification are used to define either/or type structures rather than validations.
     * Because of this ambiguity in the spec about what is non-standard about oneOf support, we'll warn as a recommendation that
     * properties on the schema defining oneOf relationships may not be intentional in the OpenAPI Specification.
     *
     * @param schemaWrapper An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkOneOfWithProperties(SchemaWrapper schemaWrapper) {
        Schema schema = schemaWrapper.getSchema();
        ValidationRule.Result result = ValidationRule.Pass.empty();

        if (ModelUtils.isComposedSchema(schema)) {
            // check for loosely defined oneOf extension requirements.
            // This is a recommendation because the 3.0.x spec is not clear enough on usage of oneOf.
            // see https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.2.1.3 and the OAS section on 'Composition and Inheritance'.
            if (ModelUtils.hasOneOf(schema)) {
                if (ModelUtils.hasProperties(schema) && schema.getProperties().get("discriminator") == null) {
                    // not necessarily "invalid" here, but we trigger the recommendation which requires the method to return false.
                    result = ValidationRule.Fail.empty();
                }
            }
        }
        return result;
    }

    /**
     * JSON Schema specifies type: 'null'.
     * <p>
     * 'null' type is supported in OpenAPI Specification 3.1 and above. It is not supported in OpenAPI 3.0.x.
     * Note: the validator invokes checkNullType() for every top-level schema in the OAS document. The method
     * is not called for nested schemas that are defined inline.
     *
     * @param schemaWrapper An input schema, regardless of the type of schema.
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkNullType(SchemaWrapper schemaWrapper) {
        Schema schema = schemaWrapper.getSchema();
        ValidationRule.Result result = ValidationRule.Pass.empty();
        if (schemaWrapper.getOpenAPI() != null) {
            SemVer version = new SemVer(schemaWrapper.getOpenAPI().getOpenapi());
            if (version.atLeast("3.0") && version.compareTo(new SemVer("3.1")) < 0) {
                // OAS spec is 3.0.x
                if (ModelUtils.isNullType(schema)) {
                    result = new ValidationRule.Fail();
                    result.setDetails(String.format(Locale.ROOT,
                            "Schema '%s' uses a 'null' type, which is specified in OAS 3.1 and above, but OAS document is version %s",
                            nameOf(schema), schemaWrapper.getOpenAPI().getOpenapi()));
                    return result;
                }
            }
        }
        return result;
    }

    /**
     * JSON Schema uses the 'nullable' attribute.
     * <p>
     * The 'nullable' attribute is supported in OpenAPI Specification 3.0.x, but it is deprecated in OpenAPI 3.1 and above.
     *
     * @param schema An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkNullableAttribute(SchemaWrapper schemaWrapper) {
        Schema schema = schemaWrapper.getSchema();
        ValidationRule.Result result = ValidationRule.Pass.empty();
        if (schemaWrapper.getOpenAPI() != null) {
            SemVer version = new SemVer(schemaWrapper.getOpenAPI().getOpenapi());
            if (version.atLeast("3.1")) {
                if (ModelUtils.isNullable(schema)) {
                    result = new ValidationRule.Fail();
                    result.setDetails(String.format(Locale.ROOT,
                            "OAS document is version '%s'. Schema '%s' uses 'nullable' attribute, which has been deprecated in OAS 3.1.",
                            schemaWrapper.getOpenAPI().getOpenapi(), nameOf(schema)));
                    return result;
                }
            }
        }
        return result;
    }

    private static String nameOf(Schema schema) {
        return schema.getName() != null ? schema.getName() : schema.getTitle();
    }

    // The set of valid OAS values for the 'type' attribute.
    private static Set<String> validTypes = new HashSet<String>(
            Arrays.asList("null", "boolean", "object", "array", "number", "string", "integer"));

    /**
     * Validate the OAS document uses supported values for the 'type' attribute.
     * <p>
     * The type must be one of the following values: null, boolean, object, array, number, string, integer.
     *
     * @param schema An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkInvalidType(SchemaWrapper schemaWrapper) {
        Schema schema = schemaWrapper.getSchema();
        ValidationRule.Result result = ValidationRule.Pass.empty();
        if (schema.getType() != null && !validTypes.contains(schema.getType())) {
            result = new ValidationRule.Fail();
            result.setDetails(String.format(Locale.ROOT,
                    "Schema '%s' uses the '%s' type, which is not a valid type.",
                    nameOf(schema), schema.getType()));
            return result;
        }
        return result;
    }

    /**
     * Checks whether the schema has conflicting (2+ distinct) {@code default} values across its
     * {@code allOf} tree.  OpenAPI does not define precedence for defaults in {@code allOf}, so
     * any conflict is a spec smell.
     *
     * @param schemaWrapper the schema to validate
     * @return {@link ValidationRule.Pass} if no conflict exists, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkAllOfConflictingDefaults(SchemaWrapper schemaWrapper) {
        if (!ModelUtils.isComposedSchema(schemaWrapper.getSchema()) ||
                !ModelUtils.hasAllOf(schemaWrapper.getSchema())) {
            return ValidationRule.Pass.empty();
        }

        List<ModelUtils.DefaultCandidate> candidates = collectCandidates(schemaWrapper);
        long distinctCount = candidates.stream().map(c -> c.value).distinct().count();
        if (distinctCount > 1) {
            List<Object> distinctValues = candidates.stream()
                    .map(c -> c.value)
                    .distinct()
                    .collect(Collectors.toList());
            ValidationRule.Fail fail = new ValidationRule.Fail();
            fail.setDetails(String.format(Locale.ROOT,
                    "Schema '%s' has conflicting default values %s across allOf branches.",
                    nameOf(schemaWrapper.getSchema()), distinctValues));
            return fail;
        }
        return ValidationRule.Pass.empty();
    }

    /**
     * Checks whether the schema has a root-level {@code default:} alongside {@code allOf} items
     * that also carry defaults.  The root default shadows (and effectively overwrites) any nested
     * defaults, which may be unintentional.
     *
     * @param schemaWrapper the schema to validate
     * @return {@link ValidationRule.Pass} if no shadowing exists, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkAllOfShadowedDefaults(SchemaWrapper schemaWrapper) {
        Schema<?> schema = schemaWrapper.getSchema();
        if (!ModelUtils.hasAllOf(schema) || schema.getDefault() == null) {
            return ValidationRule.Pass.empty();
        }

        List<ModelUtils.DefaultCandidate> candidates = collectCandidates(schemaWrapper);
        boolean hasNestedDefaults = candidates.stream().anyMatch(c -> c.depth > 0);
        if (hasNestedDefaults) {
            ValidationRule.Fail fail = new ValidationRule.Fail();
            fail.setDetails(String.format(Locale.ROOT,
                    "Schema '%s' has a root-level 'default: %s' that shadows defaults in allOf branches.",
                    nameOf(schema), schema.getDefault()));
            return fail;
        }
        return ValidationRule.Pass.empty();
    }

    /**
     * Checks whether the schema has the same {@code default} value repeated in multiple
     * {@code allOf} branches.  Redundant identical defaults add maintenance noise without
     * adding clarity.
     *
     * @param schemaWrapper the schema to validate
     * @return {@link ValidationRule.Pass} if no redundancy exists, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkAllOfRedundantDefaults(SchemaWrapper schemaWrapper) {
        if (!ModelUtils.isComposedSchema(schemaWrapper.getSchema()) ||
                !ModelUtils.hasAllOf(schemaWrapper.getSchema())) {
            return ValidationRule.Pass.empty();
        }

        List<ModelUtils.DefaultCandidate> candidates = collectCandidates(schemaWrapper);
        if (candidates.size() < 2) return ValidationRule.Pass.empty();

        // Only flag when there is exactly ONE distinct value but MORE than one candidate
        // (i.e., the same value is repeated — not a conflict, but redundant).
        long distinctCount = candidates.stream().map(c -> c.value).distinct().count();
        if (distinctCount == 1 && candidates.size() > 1) {
            ValidationRule.Fail fail = new ValidationRule.Fail();
            fail.setDetails(String.format(Locale.ROOT,
                    "Schema '%s' has the default value '%s' repeated in %d allOf branches. "
                            + "Consider defining it only once.",
                    nameOf(schemaWrapper.getSchema()), candidates.get(0).value, candidates.size()));
            return fail;
        }
        return ValidationRule.Pass.empty();
    }

    /** Collects default candidates for a schema, tolerating a null OpenAPI document. */
    private static List<ModelUtils.DefaultCandidate> collectCandidates(SchemaWrapper schemaWrapper) {
        io.swagger.v3.oas.models.OpenAPI openAPI = schemaWrapper.getOpenAPI();
        if (openAPI == null) {
            openAPI = new io.swagger.v3.oas.models.OpenAPI();
        }
        return ModelUtils.collectDefaultCandidatesForLinting(openAPI, schemaWrapper.getSchema());
    }
}
