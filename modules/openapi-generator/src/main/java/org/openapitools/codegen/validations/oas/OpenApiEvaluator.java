package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.validation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

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

        if (ruleConfiguration.isEnableUnusedSchemasRecommendation()) {
            ValidationRule unusedSchema = ValidationRule.create(Severity.WARNING, "Unused schema", "A schema was determined to be unused.", s -> true);
            ModelUtils.getUnusedSchemas(specification).forEach(schemaName -> validationResult.addResult(Validated.invalid(unusedSchema, "Unused model: " + schemaName)));
        }

        Map<String, Schema> schemas = ModelUtils.getSchemas(specification);
        schemas.forEach((key, schema) -> validationResult.consume(schemaValidations.validate(schema)));

        List<Parameter> parameters = new ArrayList<>(50);

        Paths paths = specification.getPaths();
        if (paths != null) {
            paths.forEach((key, pathItem) -> {
                // parameters defined "globally"
                List<Parameter> pathParameters = pathItem.getParameters();
                if (pathParameters != null) parameters.addAll(pathItem.getParameters());

                // parameters on each operation method
                Stream.of(
                        pathItem.getGet(),
                        pathItem.getDelete(),
                        pathItem.getHead(),
                        pathItem.getOptions(),
                        pathItem.getPatch(),
                        pathItem.getPost(),
                        pathItem.getPut(),
                        pathItem.getTrace()).forEach(op -> {
                    if (op != null && op.getParameters() != null) parameters.addAll(op.getParameters());
                });
            });
        }

        Components components = specification.getComponents();
        if (components != null) {
            Map<String, SecurityScheme> securitySchemes = components.getSecuritySchemes();
            if (securitySchemes != null && !securitySchemes.isEmpty()) {
                securitySchemes.values().forEach(securityScheme -> validationResult.consume(securitySchemeValidations.validate(securityScheme)));
            }

            if (components.getParameters() != null) {
                parameters.addAll(components.getParameters().values());
            }
        }

        parameters.forEach(parameter -> {
            validationResult.consume(parameterValidations.validate(parameter));
        });

        return validationResult;
    }
}
