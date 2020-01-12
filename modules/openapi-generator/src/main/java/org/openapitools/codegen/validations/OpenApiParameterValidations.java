package org.openapitools.codegen.validations;

import io.swagger.v3.oas.models.parameters.HeaderParameter;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;

public class OpenApiParameterValidations extends GenericValidator<Parameter> {
    OpenApiParameterValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreSuggestion()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiParameterValidations::isApacheNginxHeaderSuggestion
                ));
            }
        }
    }

    private static boolean isApacheNginxHeaderSuggestion(Parameter parameter) {
        boolean result = false;
        if (parameter instanceof HeaderParameter) {
            HeaderParameter headerParameter = (HeaderParameter) parameter;
            String headerName = headerParameter.getName();
            if (StringUtils.isNotEmpty(headerName) && StringUtils.contains(headerName, '_')) {
                result = true;
            }
        }
        return result;
    }
}
