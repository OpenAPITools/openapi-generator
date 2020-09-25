package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.parameters.HeaderParameter;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;
import java.util.Locale;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link Parameter}
 */
class OpenApiParameterValidations extends GenericValidator<ParameterWrapper> {
    OpenApiParameterValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreRecommendation()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiParameterValidations::apacheNginxHeaderCheck
                ));
            }
        }
    }

    /**
     * Apache and Nginx default to legacy CGI behavior in which header with underscore are ignored. Raise this for awareness to the user.
     *
     * @param parameter Any spec doc parameter. The method will handle {@link HeaderParameter} evaluation.
     * @return {@link ValidationRule.Pass} if the check succeeds, otherwise {@link ValidationRule.Fail} with details "[key] contains an underscore."
     */
    private static ValidationRule.Result apacheNginxHeaderCheck(ParameterWrapper parameterWrapper) {
        Parameter parameter = parameterWrapper.getParameter();
        if (parameter == null || !parameter.getIn().equals("header")) return ValidationRule.Pass.empty();
        ValidationRule.Result result = ValidationRule.Pass.empty();

        String headerName = parameter.getName();
        if (StringUtils.isNotEmpty(headerName) && StringUtils.contains(headerName, '_')) {
            result = new ValidationRule.Fail();
            result.setDetails(String.format(Locale.ROOT, "%s contains an underscore.", headerName));
        }

        return result;
    }
}
