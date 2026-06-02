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

    // -------------------------------------------------------------------------
    // allOf conflicting defaults lint rule
    // -------------------------------------------------------------------------

    @Test
    public void checkAllOfConflictingDefaults_distinctDefaults_firesWarning() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfConflictingDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("alpha");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("beta");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has conflicting default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 1, "Expected conflicting-defaults warning to fire.");
    }

    @Test
    public void checkAllOfConflictingDefaults_identicalDefaults_doesNotFireConflict() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfConflictingDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("same");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("same");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has conflicting default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Identical defaults must not fire the conflict rule.");
    }

    @Test
    public void checkAllOfConflictingDefaults_disabled_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfConflictingDefaultsRecommendation(false);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("alpha");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("beta");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has conflicting default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Rule must be suppressed when disabled.");
    }

    @Test
    public void checkAllOfConflictingDefaults_noAllOf_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfConflictingDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema schema = new StringSchema();
        schema.setDefault("only");

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has conflicting default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Plain schema must not trigger allOf rule.");
    }

    // -------------------------------------------------------------------------
    // allOf shadowed defaults lint rule
    // -------------------------------------------------------------------------

    @Test
    public void checkAllOfShadowedDefaults_rootAndBranchDefault_firesWarning() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfShadowedDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch = new StringSchema();
        branch.setDefault("nested");

        ComposedSchema schema = new ComposedSchema();
        schema.setDefault("root");
        schema.allOf(Arrays.asList(branch));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has a root-level default that shadows allOf branch defaults.");
        Assert.assertEquals(warnings.size(), 1, "Expected shadowed-defaults warning to fire.");
    }

    @Test
    public void checkAllOfShadowedDefaults_onlyRootDefault_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfShadowedDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch = new StringSchema(); // no default in branch

        ComposedSchema schema = new ComposedSchema();
        schema.setDefault("root");
        schema.allOf(Arrays.asList(branch));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has a root-level default that shadows allOf branch defaults.");
        Assert.assertEquals(warnings.size(), 0, "No shadowing when branch has no default.");
    }

    @Test
    public void checkAllOfShadowedDefaults_disabled_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfShadowedDefaultsRecommendation(false);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch = new StringSchema();
        branch.setDefault("nested");

        ComposedSchema schema = new ComposedSchema();
        schema.setDefault("root");
        schema.allOf(Arrays.asList(branch));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has a root-level default that shadows allOf branch defaults.");
        Assert.assertEquals(warnings.size(), 0, "Rule must be suppressed when disabled.");
    }

    // -------------------------------------------------------------------------
    // allOf redundant defaults lint rule
    // -------------------------------------------------------------------------

    @Test
    public void checkAllOfRedundantDefaults_identicalDefaults_firesWarning() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfRedundantDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("same");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("same");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has redundant identical default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 1, "Expected redundant-defaults warning to fire.");
    }

    @Test
    public void checkAllOfRedundantDefaults_distinctDefaults_doesNotFireRedundant() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfRedundantDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("alpha");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("beta");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has redundant identical default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Distinct defaults must not fire the redundant rule.");
    }

    @Test
    public void checkAllOfRedundantDefaults_disabled_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfRedundantDefaultsRecommendation(false);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("same");
        StringSchema branch2 = new StringSchema();
        branch2.setDefault("same");

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has redundant identical default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Rule must be suppressed when disabled.");
    }

    @Test
    public void checkAllOfRedundantDefaults_singleBranchDefault_doesNotFire() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfRedundantDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        StringSchema branch1 = new StringSchema();
        branch1.setDefault("only_one");
        StringSchema branch2 = new StringSchema(); // no default

        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch1, branch2));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> warnings = warningsForRule(result, "Schema has redundant identical default values across allOf branches.");
        Assert.assertEquals(warnings.size(), 0, "Single default cannot be redundant.");
    }

    // -------------------------------------------------------------------------
    // Clean schema — no warnings from any allOf default rule
    // -------------------------------------------------------------------------

    @Test
    public void allOfDefaultRules_cleanSchema_noWarnings() {
        RuleConfiguration config = new RuleConfiguration();
        config.setEnableAllOfConflictingDefaultsRecommendation(true);
        config.setEnableAllOfShadowedDefaultsRecommendation(true);
        config.setEnableAllOfRedundantDefaultsRecommendation(true);
        OpenApiSchemaValidations validator = new OpenApiSchemaValidations(config);

        // Clean: allOf with one branch that has a default, root has no default
        StringSchema branch = new StringSchema();
        branch.setDefault("clean");
        ComposedSchema schema = new ComposedSchema();
        schema.allOf(Arrays.asList(branch));

        ValidationResult result = validator.validate(new SchemaWrapper(null, schema));
        List<Invalid> allOfWarnings = result.getWarnings().stream()
                .filter(w -> w.getRule().getDescription() != null
                        && w.getRule().getDescription().contains("allOf"))
                .collect(Collectors.toList());
        Assert.assertEquals(allOfWarnings.size(), 0, "Clean schema should not fire any allOf default warnings.");
    }

    // -------------------------------------------------------------------------
    // Helper
    // -------------------------------------------------------------------------

    private List<Invalid> warningsForRule(ValidationResult result, String ruleDescription) {
        return result.getWarnings().stream()
                .filter(w -> ruleDescription.equals(w.getRule().getDescription()))
                .collect(Collectors.toList());
    }
}