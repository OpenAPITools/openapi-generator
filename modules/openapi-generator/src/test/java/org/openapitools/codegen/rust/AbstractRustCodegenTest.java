package org.openapitools.codegen.rust;

import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.languages.AbstractRustCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.function.Function;


public class AbstractRustCodegenTest {

    private final AbstractRustCodegen fakeRustCodegen = new P_AbstractRustCodegen();

    @Test
    public void testIsReservedWord() {
        Assert.assertTrue(fakeRustCodegen.isReservedWord("return"));
        Assert.assertTrue(fakeRustCodegen.isReservedWord("self"));
        Assert.assertTrue(fakeRustCodegen.isReservedWord("Self"));

        Assert.assertFalse(fakeRustCodegen.isReservedWord("Return"));
    }

    @Test
    public void testSanitizeIdentifier() {
        // Functions to make this less verbose
        Function<String, String> sanitizeSnakeCase = (String name) ->
                fakeRustCodegen.sanitizeIdentifier(name, AbstractRustCodegen.CasingType.SNAKE_CASE, "p", "Rust", true);
        Function<String, String> sanitizeCamelCase = (String name) ->
                fakeRustCodegen.sanitizeIdentifier(name, AbstractRustCodegen.CasingType.CAMEL_CASE, "p", "Rust", true);

        // Underscores should be allowed through
        Assert.assertEquals(sanitizeSnakeCase.apply("pet_name"), "pet_name");

        // Hyphens should be replaced (https://github.com/OpenAPITools/openapi-generator/commit/4cb7f1d6135aa3a42ff38cf89771105c40e7e5a9)
        Assert.assertEquals(sanitizeSnakeCase.apply("pet-name"), "pet_name");

        // Special character mappings are applied
        Assert.assertEquals(sanitizeSnakeCase.apply("@type"), "at_type");
        Assert.assertEquals(sanitizeCamelCase.apply("@type"), "AtType");

        // Trailing underscore removed when appropriate
        Assert.assertEquals(sanitizeSnakeCase.apply("pet@"), "pet_at");
        Assert.assertEquals(sanitizeSnakeCase.apply("pet_"), "pet_");
        Assert.assertEquals(sanitizeCamelCase.apply("*"), "Star");

        // Any other special characters are sanitized
        Assert.assertEquals(sanitizeSnakeCase.apply("Halloween\uD83C\uDF83"), "halloween");

        // Regular reserved words
        Assert.assertEquals(sanitizeSnakeCase.apply("return"), "r#return");
        Assert.assertEquals(sanitizeSnakeCase.apply("Return"), "r#return");
        Assert.assertEquals(sanitizeCamelCase.apply("return"), "Return");
        Assert.assertEquals(sanitizeCamelCase.apply("return"), "Return");

        // Special reserved words
        Assert.assertEquals(sanitizeCamelCase.apply("self"), "PSelf");
        Assert.assertEquals(sanitizeSnakeCase.apply("self"), "p_self");
        Assert.assertEquals(sanitizeCamelCase.apply("Self"), "PSelf");

        // Must not start with a number
        Assert.assertEquals(sanitizeSnakeCase.apply("1_PET"), "p_1_pet");
        Assert.assertEquals(sanitizeSnakeCase.apply("123_PET"), "p_123_pet");
        Assert.assertEquals(sanitizeCamelCase.apply("_12345AnyOf"), "P12345AnyOf");
        // Other numbers are allowed
        Assert.assertEquals(sanitizeSnakeCase.apply("PET_2"), "pet_2");

        // Check blank strings don't cause exceptions
        Assert.assertEquals(sanitizeSnakeCase.apply(""), "");
    }

    @Test
    public void testToVarName() {
        // Should be converted to snake case
        Assert.assertEquals(fakeRustCodegen.toVarName("PetName"), "pet_name");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toVarName("1PetName"), "param_1_pet_name");
    }

    @Test
    public void testToParamName() {
        // Should be converted to snake case
        Assert.assertEquals(fakeRustCodegen.toParamName("PetName"), "pet_name");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toParamName("1PetName"), "param_1_pet_name");
    }

    @Test
    public void testToOperationId() {
        // Should be converted to camel case
        Assert.assertEquals(fakeRustCodegen.toOperationId("createPet"), "create_pet");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toOperationId("1CreatePet"), "call_1_create_pet");
    }

    @Test
    public void testToModelName() {
        // Should be converted to camel case
        Assert.assertEquals(fakeRustCodegen.toModelName("pet_summary"), "PetSummary");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toModelName("1_pet_summary"), "Model1PetSummary");
    }

    @Test
    public void testToModelFileName() {
        // Should be converted to snake case
        Assert.assertEquals(fakeRustCodegen.toModelFilename("PetSummary"), "pet_summary");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toModelFilename("1PetSummary"), "model_1_pet_summary");
    }

    @Test
    public void testToEnumVarName() {
        // Should be converted to camel case
        Assert.assertEquals(fakeRustCodegen.toEnumVarName("pending", null), "Pending");
        // Enums are often represented in SCREAMING_SNAKE_CASE, check these are also converted to Rust enum camel case
        Assert.assertEquals(fakeRustCodegen.toEnumVarName("SCREAMING_SNAKE_CASE", null), "ScreamingSnakeCase");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toEnumVarName("1_pending", null), "Variant1Pending");
    }

    @Test
    public void testToEnumName() {
        Function<String, String> toEnumName = (String name) -> {
            CodegenProperty property = new CodegenProperty();
            property.name = name;
            return fakeRustCodegen.toEnumName(property);
        };
        // Should be converted to camel case
        Assert.assertEquals(toEnumName.apply("pet_status"), "PetStatusWithSuffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(toEnumName.apply("1_pet_status"), "Enum1PetStatusWithSuffix");
    }

    @Test
    public void testToEnumValue() {
        // Value should match spec
        Assert.assertEquals(fakeRustCodegen.toEnumValue("12345valueAbc#!", null), "12345valueAbc#!");
        // Quotes should be escaped so that the Rust string is valid
        Assert.assertEquals(fakeRustCodegen.toEnumValue("\"quote\"", null), "\\\"quote\\\"");
    }

    @Test
    public void testToApiName() {
        // Unnamed
        Assert.assertEquals(fakeRustCodegen.toApiName(""), "DefaultWithSuffix");
        // Should be camel case
        Assert.assertEquals(fakeRustCodegen.toApiName("pet"), "PetWithSuffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toApiName("1_pet"), "Api1PetWithSuffix");
    }

    @Test
    public void testToApiFilename() {
        // Unnamed
        Assert.assertEquals(fakeRustCodegen.toApiFilename(""), "default_with_suffix");
        // Should be snake case
        Assert.assertEquals(fakeRustCodegen.toApiFilename("Pet"), "pet_with_suffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(fakeRustCodegen.toApiFilename("1Pet"), "api_1_pet_with_suffix");
    }

    private static class P_AbstractRustCodegen extends AbstractRustCodegen {

        P_AbstractRustCodegen() {
            this.enumSuffix = "WithSuffix";
            this.apiNameSuffix = "WithSuffix";
        }

    }
}
