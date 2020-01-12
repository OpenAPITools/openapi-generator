package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;

/**
 * A standalone instance for evaluating rules and recommendations related to OAS {@link SecurityScheme}
 */
class OpenApiSecuritySchemeValidations extends GenericValidator<SecurityScheme> {
    OpenApiSecuritySchemeValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreRecommendation()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiSecuritySchemeValidations::isApacheNginxHeaderSuggestion
                ));
            }
        }
    }

    /**
     * Apache and Nginx default to legacy CGI behavior in which header with underscore are ignored. Raise this for awareness to the user.
     *
     * @param securityScheme Security schemes are often used as header parameters (e.g. APIKEY).
     *
     * @return <code>true</code> if the header has an underscore (e.g. 'api_key')
     */
    private static boolean isApacheNginxHeaderSuggestion(SecurityScheme securityScheme) {
        return securityScheme != null &&
                securityScheme.getIn() == SecurityScheme.In.HEADER &&
                StringUtils.contains(securityScheme.getName(), '_');
    }
}
