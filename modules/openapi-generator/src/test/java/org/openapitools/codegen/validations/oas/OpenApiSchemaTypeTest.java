package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.stream.Collectors;

public class OpenApiSchemaTypeTest {
    @Test(dataProvider = "oas31RecommendationExpectations", description = "Warn when 3.1 features are present in a OAS 3.0 document")
    public void testOas30DocumentWithNullType(final OpenAPI openAPI, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator validator = new OpenApiEvaluator(config);
        ValidationResult result = validator.validate(openAPI);
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "Schema uses the 'null' type but OAS document is version 3.0." .equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        if (matches) {
            Assert.assertEquals(warnings.size() >= 1, true, "Expected to match recommendation.");
        } else {
            Assert.assertEquals(warnings.size(), 0, "Expected not to match recommendation.");
        }
    }

    @DataProvider(name = "oas31RecommendationExpectations")
    public Object[][] oas31RecommendationExpectations() {
        return new Object[][]{
            {TestUtils.parseFlattenSpec("src/test/resources/3_1/null-types.yaml"), true}
        };
    }
}