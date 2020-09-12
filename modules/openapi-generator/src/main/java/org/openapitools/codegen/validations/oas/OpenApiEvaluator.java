package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.tags.Tag;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.validation.*;

import java.util.*;

/**
 * A validator which evaluates an OpenAPI 3.x specification document
 */
public class OpenApiEvaluator implements Validator<OpenAPI> {
    private RuleConfiguration ruleConfiguration;

    /**
     * Constructs a new instance of {@link OpenApiEvaluator} with applied rules.
     *
     * @param ruleConfiguration The set of rules to be applied to evaluation.
     */
    public OpenApiEvaluator(RuleConfiguration ruleConfiguration) {
        this.ruleConfiguration = ruleConfiguration;
    }

    /**
     * Validates input, resulting in a instance of {@link ValidationResult} which provides details on all validations performed (success, error, warning).
     *
     * @param specification The {@link OpenAPI} object instance to be validated.
     * @return A {@link ValidationResult} which details the success, error, and warning validation results.
     */
    @Override
    public ValidationResult validate(OpenAPI specification) {
        ValidationResult validationResult = new ValidationResult();
        if (specification == null) return validationResult;

        OpenApiParameterValidations parameterValidations = new OpenApiParameterValidations(ruleConfiguration);
        OpenApiSecuritySchemeValidations securitySchemeValidations = new OpenApiSecuritySchemeValidations(ruleConfiguration);
        OpenApiSchemaValidations schemaValidations = new OpenApiSchemaValidations(ruleConfiguration);
        OpenApiOperationValidations operationValidations = new OpenApiOperationValidations(ruleConfiguration);

        if (ruleConfiguration.isEnableUnusedSchemasRecommendation()) {
            ValidationRule unusedSchema = ValidationRule.create(Severity.WARNING, "Unused schema", "A schema was determined to be unused.", s -> ValidationRule.Pass.empty());
            ModelUtils.getUnusedSchemas(specification).forEach(schemaName -> validationResult.addResult(Validated.invalid(unusedSchema, "Unused model: " + schemaName)));
        }

        // Get list of all schemas under /components/schemas, including nested schemas defined inline and composed schema.
        // The validators must be able to validate every schema defined in the OAS document.
        List<Schema> schemas = ModelUtils.getAllSchemas(specification);
        schemas.forEach(schema -> {
            SchemaWrapper wrapper = new SchemaWrapper(specification, schema);
            validationResult.consume(schemaValidations.validate(wrapper));
        });

        List<Parameter> parameters = new ArrayList<>(50);

        Paths paths = specification.getPaths();
        if (paths != null) {
            paths.forEach((key, pathItem) -> {
                // parameters defined "globally"
                List<Parameter> pathParameters = pathItem.getParameters();
                if (pathParameters != null) parameters.addAll(pathItem.getParameters());

                pathItem.readOperationsMap().forEach((httpMethod, op) -> {
                    if (op != null) {
                        // parameters on each operation method
                        if (op.getParameters() != null) {
                            parameters.addAll(op.getParameters());
                        }

                        OperationWrapper wrapper = new OperationWrapper(specification, op, httpMethod);
                        validationResult.consume(operationValidations.validate(wrapper));
                    }
                });
            });
        }

        Components components = specification.getComponents();
        if (components != null) {
            Map<String, SecurityScheme> securitySchemes = components.getSecuritySchemes();
            if (securitySchemes != null && !securitySchemes.isEmpty()) {
                securitySchemes.values().forEach(securityScheme -> {
                    SecuritySchemeWrapper wrapper = new SecuritySchemeWrapper(specification, securityScheme);
                    validationResult.consume(securitySchemeValidations.validate(wrapper));
                });
            }

            if (components.getParameters() != null) {
                parameters.addAll(components.getParameters().values());
            }
        }

        parameters.forEach(parameter -> {
            parameter = ModelUtils.getReferencedParameter(specification, parameter);
            ParameterWrapper wrapper = new ParameterWrapper(specification, parameter);
            validationResult.consume(parameterValidations.validate(wrapper));
        });

        List<Tag> tags = specification.getTags();
        if (tags != null && tags.size() > 1) {
            Set<String> distinct = new HashSet<>();
            Set<String> duplicated = new HashSet<>();
            tags.forEach(tag -> {
                // add returns false if it already exists…
                if (!distinct.add(tag.getName())) {
                    duplicated.add(tag.getName());
                }
            });
            if (duplicated.size() > 0) {
                // From https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#fixed-fields
                // A list of tags used by the specification with additional metadata. The order of the tags can be used
                // to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object
                // must be declared. The tags that are not declared MAY be organized randomly or based on the tools'
                // logic. Each tag name in the list MUST be unique.
                ValidationRule rule = ValidationRule.warn("Duplicate tags", "The specification requires that tag names are unique.", s -> ValidationRule.Fail.empty());
                validationResult.addResult(Validated.invalid(rule, "Duplicated tag(s): " + String.join(",", duplicated)));
            }
        }

        return validationResult;
    }
}
