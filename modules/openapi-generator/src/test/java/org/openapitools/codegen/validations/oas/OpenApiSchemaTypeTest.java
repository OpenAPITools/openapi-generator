package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class OpenApiSchemaTypeTest {
    @Test(dataProvider = "oas31RecommendationExpectations", description = "Warn when a 'null' type is present in OAS 3.0 document")
    public void testOas30DocumentWithNullType(final OpenAPI openAPI) {
        /*
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator validator = new OpenApiEvaluator(config);
        ValidationResult result = validator.validate(openAPI);
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "Schema defines properties alongside oneOf." .equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        */
    }

    @Test(dataProvider = "oas31RecommendationExpectations", description = "Don't warn when a 'null' type is present in OAS 3.0 document")
    public void testOas31DocumentWithNullType(Schema schema, boolean matches) {
    }

    @DataProvider(name = "oas31RecommendationExpectations")
    public Object[][] oas31RecommendationExpectations() {
        return new Object[][]{
            //{TestUtils.parseSpec("src/test/resources/3_1/null-types.yaml")},
        };
    }
}