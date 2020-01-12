package org.openapitools.codegen.validations;

import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.GenericValidator;
import org.openapitools.codegen.validation.ValidationRule;

import java.util.ArrayList;

public class OpenApiSecuritySchemeValidations extends GenericValidator<SecurityScheme> {
    OpenApiSecuritySchemeValidations(RuleConfiguration ruleConfiguration) {
        super(new ArrayList<>());
        if (ruleConfiguration.isEnableRecommendations()) {
            if (ruleConfiguration.isEnableApacheNginxUnderscoreSuggestion()) {
                rules.add(ValidationRule.warn(
                        ValidationConstants.ApacheNginxUnderscoreDescription,
                        ValidationConstants.ApacheNginxUnderscoreFailureMessage,
                        OpenApiSecuritySchemeValidations::isApacheNginxHeaderSuggestion
                ));
            }
        }
    }

    private static boolean isApacheNginxHeaderSuggestion(SecurityScheme securityScheme) {
        return securityScheme != null &&
                SecurityScheme.Type.APIKEY.equals(securityScheme.getType()) &&
                StringUtils.contains(securityScheme.getName(), '_');
    }
}
