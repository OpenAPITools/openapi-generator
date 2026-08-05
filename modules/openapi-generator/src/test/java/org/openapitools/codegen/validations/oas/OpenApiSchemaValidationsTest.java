package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.ValidationResult;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class OpenApiSchemaValidationsTest {
    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "default oneOf with sibling properties recommendation")
    public void testDefaultRecommendationOneOfWithSiblingProperties(Schema schema, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "Schema defines properties alongside oneOf.".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        if (matches) {
            Assert.assertEquals(warnings.size(), 1, "Expected to match recommendation.");
        } else {
            Assert.assertEquals(warnings.size(), 0, "Expected not to match recommendation.");
        }
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable oneOf with sibling properties recommendation via turning off recommendations")
    public void testOneOfWithSiblingPropertiesDisabledRecommendations(Schema schema, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(false);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "Schema defines properties alongside oneOf.".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected recommendations to be disabled completely.");
    }

    @Test(dataProvider = "apacheNginxRecommendationExpectations", description = "disable oneOf with sibling properties recommendation via turning off rule")
    public void testOneOfWithSiblingPropertiesDisabledRule(Schema schema, boolean matches) {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableOneOfWithPropertiesRecommendation(false);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        Assert.assertNotNull(result.getWarnings());

        List<Invalid> warnings = result.getWarnings().stream()
                .filter(invalid -> "Schema defines properties alongside oneOf.".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected rule to be disabled.");
    }

    /**
     * The validation warning for 'nullable: true' in an OAS 3.1 spec must fire.
     * The existing checkNullableAttribute only checked ModelUtils.isNullable(schema) which relies on
     * schema.getNullable() — but swagger-parser does not populate that field for 3.1 specs (it stores
     * the value in extensions["nullable"] instead). The fix must also check extensions["nullable"].
     */
    @Test(description = "nullable: true in OAS 3.1 spec must trigger the nullable-deprecated warning")
    public void testNullableAttributeInOas31_triggerWarning() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/nullable-deprecated-in-oas31.yaml");
        Schema<?> proxyUrl = (Schema<?>) openAPI.getComponents().getSchemas().get("TestModel").getProperties().get("proxyUrl");

        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        ValidationResult result = validator.validate(new SchemaWrapper(openAPI, proxyUrl));
        List<Invalid> nullableWarnings = result.getWarnings().stream()
                .filter(invalid -> "Schema uses the 'nullable' attribute.".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertEquals(nullableWarnings.size(), 1,
                "Expected exactly one 'nullable attribute deprecated' warning for a 3.1 spec using nullable: true");
    }

    /**
     * The nullable-deprecated warning must NOT fire for an OAS 3.1 spec using the correct 3.1 null type syntax.
     */
    @Test(description = "correct OAS 3.1 null type syntax (type: [string, null]) must NOT trigger the nullable-deprecated warning")
    public void testNullTypeInOas31_noWarning() {
        OpenAPI openAPI = TestUtils.parseSpec("src/test/resources/3_1/null-types-simple.yaml");
        Schema<?> stringDataOrNull = (Schema<?>) openAPI.getComponents().getSchemas().get("WithNullableType").getProperties().get("stringDataOrNull");

        RuleConfiguration config = new RuleConfiguration();
        config.setEnableRecommendations(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        ValidationResult result = validator.validate(new SchemaWrapper(openAPI, stringDataOrNull));
        List<Invalid> nullableWarnings = result.getWarnings().stream()
                .filter(invalid -> "Schema uses the 'nullable' attribute.".equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertEquals(nullableWarnings.size(), 0,
                "OAS 3.1 with type:[string,null] must not trigger the nullable-deprecated warning");
    }

    @DataProvider(name = "apacheNginxRecommendationExpectations")
    public Object[][] apacheNginxRecommendationExpectations() {
        return new Object[][]{
                {getOneOfSample(true), true},
                {getOneOfSample(false), false},
                {getAllOfSample(true), false},
                {getAllOfSample(false), false},
                {getAnyOfSample(true), false},
                {getAnyOfSample(false), false},
                {new StringSchema(), false},
                {new MapSchema(), false},
                {new ArraySchema(), false},
                {new ObjectSchema(), false}
        };
    }

    private ComposedSchema getOneOfSample(boolean withProperties) {
        ComposedSchema schema = new ComposedSchema();
        schema.oneOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperty("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }

    private ComposedSchema getAllOfSample(boolean withProperties) {
        // This doesn't matter if it's realistic; it's a structural check
        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperty("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }

    private ComposedSchema getAnyOfSample(boolean withProperties) {
        ComposedSchema schema = new ComposedSchema();
        schema.anyOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperty("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }
}