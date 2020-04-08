package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.media.*;
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
                .filter(invalid -> "Schema defines properties alongside oneOf." .equals(invalid.getRule().getDescription()))
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
                .filter(invalid -> "Schema defines properties alongside oneOf." .equals(invalid.getRule().getDescription()))
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
                .filter(invalid -> "Schema defines properties alongside oneOf." .equals(invalid.getRule().getDescription()))
                .collect(Collectors.toList());

        Assert.assertNotNull(warnings);
        Assert.assertEquals(warnings.size(), 0, "Expected rule to be disabled.");
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
        ComposedSchema schema = new ComposedSchema().oneOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperties("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }

    private ComposedSchema getAllOfSample(boolean withProperties) {
        // This doesn't matter if it's realistic; it's a structural check
        ComposedSchema schema = new ComposedSchema().allOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperties("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }

    private ComposedSchema getAnyOfSample(boolean withProperties) {
        ComposedSchema schema = new ComposedSchema().anyOf(Arrays.asList(
                new StringSchema(),
                new IntegerSchema().format("int64"))
        );

        if (withProperties) {
            schema.addProperties("other", new ArraySchema()
                    .items(new Schema().$ref("#/definitions/Other")));
        }

        return schema;
    }
}