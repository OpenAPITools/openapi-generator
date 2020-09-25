package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.parameters.Parameter;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.stream.Collectors;

public class OpenApiParameterValidationsTest {

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable apache nginx via turning off recommendations")
    public void testApacheNginxWithDisabledRecommendations(String in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(false);
        OpenApiParameterValidations validator = new OpenApiParameterValidations(config);

        Parameter parameter = new Parameter();
        parameter.setIn(in);
        parameter.setName(key);

        ValidationResult result = validator.validate(new ParameterWrapper(null, parameter));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> ValidationConstants.ApacheNginxUnderscoreFailureMessage.equals(invalid.getMessage()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected recommendations to be disabled completely.");
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable apache nginx via turning off recommendations")
    public void testApacheNginxWithDisabledRule(String in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableApacheNginxUnderscoreRecommendation(false);
        OpenApiParameterValidations validator = new OpenApiParameterValidations(config);

        Parameter parameter = new Parameter();
        parameter.setIn(in);
        parameter.setName(key);

        ValidationResult result = validator.validate(new ParameterWrapper(null, parameter));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> ValidationConstants.ApacheNginxUnderscoreFailureMessage.equals(invalid.getMessage()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected rule to be disabled.");
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "default apache nginx recommendation")
    public void testDefaultRecommendationApacheNginx(String in, String key, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiParameterValidations validator = new OpenApiParameterValidations(config);

        Parameter parameter = new Parameter();
        parameter.setIn(in);
        parameter.setName(key);

        ValidationResult result = validator.validate(new ParameterWrapper(null, parameter));
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
                {"header", "api_key", true},
                {"header", "apikey", false},
                {"cookie", "api_key", false},
                {"cookie", "apikey", false},
                {"query", "api_key", false},
                {"query", "apikey", false}
        };
    }
}
