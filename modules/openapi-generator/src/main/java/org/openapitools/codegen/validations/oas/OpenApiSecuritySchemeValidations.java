package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;
import java.util.Locale;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link SecurityScheme}
 */
class OpenApiSecuritySchemeValidations extends GenericValidator<SecuritySchemeWrapper> {
    OpenApiSecuritySchemeValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreRecommendation()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiSecuritySchemeValidations::apacheNginxHeaderCheck
                ));
            }
        }
    }

    /**
     * Apache and Nginx default to legacy CGI behavior in which header with underscore are ignored. Raise this for awareness to the user.
     *
     * @param securityScheme Security schemes are often used as header parameters (e.g. APIKEY).
     * @return <code>true</code> if the check succeeds (header does not have an underscore, e.g. 'api_key')
     */
    private static ValidationRule.Result apacheNginxHeaderCheck(SecuritySchemeWrapper securitySchemeWrapper) {
        SecurityScheme securityScheme = securitySchemeWrapper.getSecurityScheme();
        if (securityScheme == null || securityScheme.getIn() != SecurityScheme.In.HEADER)
            return ValidationRule.Pass.empty();
        ValidationRule.Result result = ValidationRule.Pass.empty();

        String key = securityScheme.getName();
        if (StringUtils.contains(key, '_')) {
            result = new ValidationRule.Fail();
            result.setDetails(String.format(Locale.ROOT, "%s contains an underscore.", key));
        }

        return result;
    }
}
