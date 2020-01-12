package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.parameters.HeaderParameter;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link Parameter}
 */
class OpenApiParameterValidations extends GenericValidator<Parameter> {
    OpenApiParameterValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreRecommendation()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiParameterValidations::isApacheNginxHeaderSuggestion
                ));
            }
        }
    }

    /**
     * Apache and Nginx default to legacy CGI behavior in which header with underscore are ignored. Raise this for awareness to the user.
     *
     * @param parameter Any spec doc parameter. The method will handle {@link HeaderParameter} evaluation.
     *
     * @return <code>true</code> if the header has an underscore (e.g. 'api_key')
     */
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
