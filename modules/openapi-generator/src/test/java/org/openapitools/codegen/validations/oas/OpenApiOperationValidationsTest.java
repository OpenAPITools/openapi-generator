package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.stream.Collectors;

public class OpenApiOperationValidationsTest {
    @DataProvider(name = "getOrHeadWithBodyExpectations")
    public Object[][] getOrHeadWithBodyExpectations() {
        return new Object[][]{
                /* method */ /* operationId */ /* ref */ /* content */ /* triggers warning */
                {PathItem.HttpMethod.GET, "opWithRef", "#/components/schemas/Animal", null, true},
                {PathItem.HttpMethod.GET, "opWithContent", null, new Content().addMediaType("a", new MediaType()), true},
                {PathItem.HttpMethod.GET, "opWithoutRefOrContent", null, null, false},
                {PathItem.HttpMethod.HEAD, "opWithRef", "#/components/schemas/Animal", null, true},
                {PathItem.HttpMethod.HEAD, "opWithContent", null, new Content().addMediaType("a", new MediaType()), true},
                {PathItem.HttpMethod.HEAD, "opWithoutRefOrContent", null, null, false},
                {PathItem.HttpMethod.POST, "opWithRef", "#/components/schemas/Animal", null, false},
                {PathItem.HttpMethod.POST, "opWithContent", null, new Content().addMediaType("a", new MediaType()), false},
                {PathItem.HttpMethod.POST, "opWithoutRefOrContent", null, null, false}
        };
    }

    @Test(dataProvider = "getOrHeadWithBodyExpectations")
    public void testGetOrHeadWithBody(PathItem.HttpMethod method, String operationId, String ref, Content content, boolean shouldTriggerFailure) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiOperationValidations validator = new OpenApiOperationValidations(config);

        Operation op = new Operation().operationId(operationId);
        RequestBody body = new RequestBody();
        if (StringUtils.isNotEmpty(ref) || content != null) {
            body.$ref(ref);
            body.content(content);

            op.setRequestBody(body);
        }

        ValidationResult result = validator.validate(new OperationWrapper(null, op, method));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "API GET/HEAD defined with request body".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        if (shouldTriggerFailure) {
            Assert.assertEquals(warnings.size(), 1, "Expected warnings to include recommendation.");
        } else {
            Assert.assertEquals(warnings.size(), 0, "Expected warnings not to include recommendation.");
        }
    }

    @Test(dataProvider = "getOrHeadWithBodyExpectations")
    public void testGetOrHeadWithBodyWithDisabledRecommendations(PathItem.HttpMethod method, String operationId, String ref, Content content, boolean shouldTriggerFailure) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(false);
        OpenApiOperationValidations validator = new OpenApiOperationValidations(config);

        Operation op = new Operation().operationId(operationId);
        RequestBody body = new RequestBody();
        if (StringUtils.isNotEmpty(ref) || content != null) {
            body.$ref(ref);
            body.content(content);

            op.setRequestBody(body);
        }

        ValidationResult result = validator.validate(new OperationWrapper(null, op, method));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "API GET/HEAD defined with request body".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected warnings not to include recommendation.");
    }

    @Test(dataProvider = "getOrHeadWithBodyExpectations")
    public void testGetOrHeadWithBodyWithDisabledRule(PathItem.HttpMethod method, String operationId, String ref, Content content, boolean shouldTriggerFailure) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableApiRequestUriWithBodyRecommendation(false);
        OpenApiOperationValidations validator = new OpenApiOperationValidations(config);

        Operation op = new Operation().operationId(operationId);
        RequestBody body = new RequestBody();
        if (StringUtils.isNotEmpty(ref) || content != null) {
            body.$ref(ref);
            body.content(content);

            op.setRequestBody(body);
        }

        ValidationResult result = validator.validate(new OperationWrapper(null, op, method));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "API GET/HEAD defined with request body".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected warnings not to include recommendation.");
    }
}
