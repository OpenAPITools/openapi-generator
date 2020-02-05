package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

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
                        "Schema defines uses the 'null' type.",
                        "The 'null' type is not supported in OpenAPI 3.0.x. It is supported in OpenAPI 3.1 and above. While our tooling supports this, it may cause issues with other tools.",
                        OpenApiSchemaValidations::checkNullType
                ));
                rules.add(ValidationRule.warn(
                        "Schema defines uses a type array.",
                        "The type array is not supported in OpenAPI 3.0.x. It is supported in OpenAPI 3.1 and above. While our tooling supports this, it may cause issues with other tools.",
                        OpenApiSchemaValidations::checkTypeArray
                ));
            }
            if (ruleConfiguration.isEnableNullableAttributeRecommendation()) {
                rules.add(ValidationRule.warn(
                        "Schema uses the 'nullable' attribute.",
                        "The 'nullable' attribute is deprecated in OpenAPI 3.1, and may no longer be supported in future releases. Consider migrating to 'null' type.",
                        OpenApiSchemaValidations::checkNullableAttribute
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
     * 
     * @param schema An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkNullType(Schema schema) {
        ValidationRule.Result result = ValidationRule.Pass.empty();

        if (ModelUtils.isNullSchema(schema)) {
            result = new ValidationRule.Fail();
            result.setDetails(String.format(Locale.ROOT, "Specification is version '%s' %s uses a 'null' type.", "3.0.1", schema.getName()));
            return result;
        }
        if (schema instanceof ComposedSchema) {
            final ComposedSchema composed = (ComposedSchema) schema;
            List<Schema> interfaces = ModelUtils.getInterfaces(composed);
            if (!interfaces.isEmpty()) {
                for (Schema interfaceSchema : interfaces) {
                    if (ModelUtils.isNullSchema(interfaceSchema)) {
                            result = new ValidationRule.Fail();
                            result.setDetails(String.format(Locale.ROOT, "Specification is version '%s' %s uses a 'null' type.", "3.0.1",
                                interfaceSchema.getName()));
                            return result;
                    }
                }
            }
        }
        return result;
    }

    /**
     * JSON Schema uses a type array.
     * <p>
     * A type array is supported in OpenAPI Specification 3.1 and above. It is not supported in OpenAPI 3.0.x.
     * 
     * @param schema An input schema, regardless of the type of schema
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail}
     */
    private static ValidationRule.Result checkTypeArray(Schema schema) {
        ValidationRule.Result result = ValidationRule.Pass.empty();
        // TODO
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
    private static ValidationRule.Result checkNullableAttribute(Schema schema) {
        ValidationRule.Result result = ValidationRule.Pass.empty();
        // TODO
        return result;
    }
}
