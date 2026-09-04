package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class OpenApiEvaluatorTest {

    private static OpenAPI buildSpecWithEnumDefault(List<?> enumValues, Object defaultValue) {
        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");
        Components components = new Components();
        ObjectSchema obj = new ObjectSchema();
        StringSchema prop = new StringSchema();
        prop.setEnum(enumValues.stream()
                .filter(v -> v instanceof String)
                .map(v -> (String) v)
                .collect(Collectors.toList()));
        prop.setDefault(defaultValue);
        obj.addProperty("protocol", prop);
        components.addSchemas("Config", obj);
        openAPI.setComponents(components);
        return openAPI;
    }

    private static List<Invalid> getDefaultNotInEnumWarnings(ValidationResult result) {
        return result.getWarnings().stream()
                .filter(i -> i.getMessage().contains("not in enum"))
                .collect(Collectors.toList());
    }

    @Test(description = "warn when default is not in enum")
    public void testDefaultNotInEnum() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = buildSpecWithEnumDefault(Arrays.asList("udp", "tcp"), "http");
        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 1);
        Assert.assertTrue(warnings.get(0).getMessage().contains("'http'"));
        Assert.assertTrue(warnings.get(0).getMessage().contains("[udp, tcp]"));
    }

    @Test(description = "no warning when default is in enum")
    public void testDefaultInEnum() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = buildSpecWithEnumDefault(Arrays.asList("http", "https"), "http");
        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 0);
    }

    @Test(description = "no warning when rule is disabled individually")
    public void testDefaultNotInEnumDisabledRule() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        config.setEnableDefaultNotInEnumRecommendation(false);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = buildSpecWithEnumDefault(Arrays.asList("udp"), "http");
        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 0);
    }

    @Test(description = "no warning when all recommendations are disabled")
    public void testDefaultNotInEnumRecommendationsOff() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(false);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = buildSpecWithEnumDefault(Arrays.asList("udp"), "http");
        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 0);
    }

    @Test(description = "multiple schemas with default not in enum produce separate warnings")
    public void testDefaultNotInEnumMultipleOccurrences() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");
        Components components = new Components();

        ObjectSchema udpConfig = new ObjectSchema();
        StringSchema proto1 = new StringSchema();
        proto1.setEnum(Arrays.asList("udp"));
        proto1.setDefault("http");
        udpConfig.addProperty("protocol", proto1);

        ObjectSchema tcpConfig = new ObjectSchema();
        StringSchema proto2 = new StringSchema();
        proto2.setEnum(Arrays.asList("tcp"));
        proto2.setDefault("http");
        tcpConfig.addProperty("protocol", proto2);

        components.addSchemas("UdpConfig", udpConfig);
        components.addSchemas("TcpConfig", tcpConfig);
        openAPI.setComponents(components);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        // Two property schemas with distinct enum values → two unique messages
        Assert.assertEquals(warnings.size(), 2);
    }

    @Test(description = "warn for integer default not in integer enum")
    public void testDefaultNotInEnumInteger() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");
        Components components = new Components();
        ObjectSchema obj = new ObjectSchema();
        IntegerSchema prop = new IntegerSchema();
        prop.setEnum(Arrays.asList(1, 2, 3));
        prop.setDefault(99);
        obj.addProperty("code", prop);
        components.addSchemas("Config", obj);
        openAPI.setComponents(components);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 1);
        Assert.assertTrue(warnings.get(0).getMessage().contains("'99'"));
    }

    @Test(description = "no warning when schema has no enum")
    public void testNoEnum() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");
        Components components = new Components();
        ObjectSchema obj = new ObjectSchema();
        StringSchema prop = new StringSchema();
        prop.setDefault("http");
        obj.addProperty("protocol", prop);
        components.addSchemas("Config", obj);
        openAPI.setComponents(components);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 0);
    }

    @Test(description = "no warning when schema has no default")
    public void testNoDefault() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");
        Components components = new Components();
        ObjectSchema obj = new ObjectSchema();
        StringSchema prop = new StringSchema();
        prop.setEnum(Arrays.asList("udp", "tcp"));
        obj.addProperty("protocol", prop);
        components.addSchemas("Config", obj);
        openAPI.setComponents(components);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 0);
    }

    @Test(description = "warn for default not in enum in inline request body schema")
    public void testDefaultNotInEnumInlineRequestBody() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");

        // Build an inline schema in a request body (not in components/schemas)
        ObjectSchema bodySchema = new ObjectSchema();
        StringSchema prop = new StringSchema();
        prop.setEnum(Arrays.asList("udp", "tcp"));
        prop.setDefault("http");
        bodySchema.addProperty("protocol", prop);

        MediaType mediaType = new MediaType();
        mediaType.setSchema(bodySchema);
        Content content = new Content();
        content.addMediaType("application/json", mediaType);
        RequestBody requestBody = new RequestBody();
        requestBody.setContent(content);

        Operation operation = new Operation();
        operation.setRequestBody(requestBody);
        operation.setResponses(new ApiResponses());

        PathItem pathItem = new PathItem();
        pathItem.setPost(operation);
        Paths paths = new Paths();
        paths.addPathItem("/test", pathItem);
        openAPI.setPaths(paths);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 1);
        Assert.assertTrue(warnings.get(0).getMessage().contains("'http'"));
    }

    @Test(description = "warn for default not in enum in inline response schema")
    public void testDefaultNotInEnumInlineResponse() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiEvaluator evaluator = new OpenApiEvaluator(config);

        OpenAPI openAPI = new OpenAPI();
        openAPI.openapi("3.0.1");

        // Build an inline schema in a response (not in components/schemas)
        ObjectSchema responseSchema = new ObjectSchema();
        StringSchema prop = new StringSchema();
        prop.setEnum(Arrays.asList("tcp"));
        prop.setDefault("http");
        responseSchema.addProperty("protocol", prop);

        MediaType mediaType = new MediaType();
        mediaType.setSchema(responseSchema);
        Content content = new Content();
        content.addMediaType("application/json", mediaType);
        ApiResponse apiResponse = new ApiResponse();
        apiResponse.setContent(content);
        ApiResponses responses = new ApiResponses();
        responses.addApiResponse("200", apiResponse);

        Operation operation = new Operation();
        operation.setResponses(responses);

        PathItem pathItem = new PathItem();
        pathItem.setGet(operation);
        Paths paths = new Paths();
        paths.addPathItem("/test", pathItem);
        openAPI.setPaths(paths);

        ValidationResult result = evaluator.validate(openAPI);

        List<Invalid> warnings = getDefaultNotInEnumWarnings(result);
        Assert.assertEquals(warnings.size(), 1);
        Assert.assertTrue(warnings.get(0).getMessage().contains("'http'"));
    }
}
