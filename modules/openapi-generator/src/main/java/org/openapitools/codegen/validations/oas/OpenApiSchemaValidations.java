package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link Schema}
 */
class OpenApiSchemaValidations extends GenericValidator<Schema> {
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
     * @param schema An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkOneOfWithProperties(Schema schema) {
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
}
