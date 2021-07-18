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

public class OpenApiEvaluatorTest {
    @Test(dataProvider = "missingRef", description = "Error when a reference target is missing.")
    public void testMissingRef(final OpenAPI openAPI) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator validator = new OpenApiEvaluator(config);

        // Perform validation.
        ValidationResult result = validator.validate(openAPI);

        // No warnings are expected.
        Assert.assertTrue(result.getWarnings().isEmpty());

        // Errors are expected.
        Assert.assertFalse(result.getErrors().isEmpty());

        // Valid results are expected.
        Assert.assertFalse(result.getValid().isEmpty());
    }

    @DataProvider(name = "missingRef")
    public Object[][] missingRef() {
        return new Object[][]{
                {TestUtils.parseFlattenSpec("src/test/resources/3_1/issue_9047.yaml")}
        };
    }
}
