package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;

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

        if (schema instanceof ComposedSchema) {
            final ComposedSchema composed = (ComposedSchema) schema;
            // check for loosely defined oneOf extension requirements.
            // This is a recommendation because the 3.0.x spec is not clear enough on usage of oneOf.
            // see https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.2.1.3 and the OAS section on 'Composition and Inheritance'.
            if (composed.getOneOf() != null && composed.getOneOf().size() > 0) {
                if (composed.getProperties() != null && composed.getProperties().size() >= 1 && composed.getProperties().get("discriminator") == null) {
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
     * @param schema An input schema, regardless of the type of schema.
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
                    String name = schema.getName();
                    if (name == null) {
                        name = schema.getTitle();
                    }
                    result.setDetails(String.format(Locale.ROOT,
                        "Schema '%s' uses a 'null' type, which is specified in OAS 3.1 and above, but OAS document is version %s",
                        name, schemaWrapper.getOpenAPI().getOpenapi()));
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
                    String name = schema.getName();
                    if (name == null) {
                        name = schema.getTitle();
                    }
                    result.setDetails(String.format(Locale.ROOT,
                        "OAS document is version '%s'. Schema '%s' uses 'nullable' attribute, which has been deprecated in OAS 3.1.",
                        schemaWrapper.getOpenAPI().getOpenapi(), name));
                    return result;
                }
            }
        }
        return result;
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
            String name = schema.getName();
            if (name == null) {
                name = schema.getTitle();
            }
            result.setDetails(String.format(Locale.ROOT,
                "Schema '%s' uses the '%s' type, which is not a valid type.",
                name, schema.getType()));
            return result;
        }
        return result;
    }
}
