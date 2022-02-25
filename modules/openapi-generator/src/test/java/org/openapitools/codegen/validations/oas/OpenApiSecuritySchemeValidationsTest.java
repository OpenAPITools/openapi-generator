package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.stream.Collectors;

public class OpenApiSecuritySchemeValidationsTest {

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable apache nginx via turning off recommendations")
    public void testApacheNginxWithDisabledRecommendations(SecurityScheme.In in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(false);
        OpenApiSecuritySchemeValidations validator = new OpenApiSecuritySchemeValidations(config);

        SecurityScheme securityScheme = new SecurityScheme().in(in).name(key);

        ValidationResult result = validator.validate(new SecuritySchemeWrapper(null, securityScheme));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> ValidationConstants.ApacheNginxUnderscoreFailureMessage.equals(invalid.getMessage()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected recommendations to be disabled completely.");
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable apache nginx via turning off rule")
    public void testApacheNginxWithDisabledRule(SecurityScheme.In in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableApacheNginxUnderscoreRecommendation(false);
        OpenApiSecuritySchemeValidations validator = new OpenApiSecuritySchemeValidations(config);

        SecurityScheme securityScheme = new SecurityScheme().in(in).name(key);

        ValidationResult result = validator.validate(new SecuritySchemeWrapper(null, securityScheme));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> ValidationConstants.ApacheNginxUnderscoreFailureMessage.equals(invalid.getMessage()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected rule to be disabled.");
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "default apache nginx recommendation")
    public void testDefaultRecommendationApacheNginx(SecurityScheme.In in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiSecuritySchemeValidations validator = new OpenApiSecuritySchemeValidations(config);

        SecurityScheme securityScheme = new SecurityScheme().in(in).name(key);

        ValidationResult result = validator.validate(new SecuritySchemeWrapper(null, securityScheme));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> ValidationConstants.ApacheNginxUnderscoreFailureMessage.equals(invalid.getMessage()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        if (matches) {
            Assert.assertEquals(warnings.size(), 1, "Expected " + key + " to match recommendation.");
        } else {
            Assert.assertEquals(warnings.size(), 0, "Expected " + key + " not to match recommendation.");
        }
    }

    @DataProvider(name = "apacheNginxRecommendationExpectations")
    public Object[][] apacheNginxRecommendationExpectations() {
        return new Object[][]{
                {SecurityScheme.In.HEADER, "api_key", true},
                {SecurityScheme.In.HEADER, "apikey", false},
                {SecurityScheme.In.COOKIE, "api_key", false},
                {SecurityScheme.In.COOKIE, "apikey", false},
                {SecurityScheme.In.QUERY, "api_key", false},
                {SecurityScheme.In.COOKIE, "apikey", false}
        };
    }
}