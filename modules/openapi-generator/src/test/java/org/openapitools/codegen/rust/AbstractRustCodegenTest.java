package org.openapitools.codegen.rust;

import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.languages.AbstractRustCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigInteger;
import java.util.function.Function;


public class AbstractRustCodegenTest {

    private final AbstractRustCodegen codegen = new P_AbstractRustCodegen();

    @Test
    public void testIsReservedWord() {
        Assert.assertTrue(codegen.isReservedWord("return"));
        Assert.assertTrue(codegen.isReservedWord("self"));
        Assert.assertTrue(codegen.isReservedWord("Self"));

        Assert.assertFalse(codegen.isReservedWord("Return"));
    }

    @Test
    public void testSanitizeIdentifier() {
        // Functions to make this less verbose
        Function<String, String> sanitizeSnakeCase = (String name) ->
                codegen.sanitizeIdentifier(name, AbstractRustCodegen.CasingType.SNAKE_CASE, "p", "Rust", true);
        Function<String, String> sanitizeCamelCase = (String name) ->
                codegen.sanitizeIdentifier(name, AbstractRustCodegen.CasingType.CAMEL_CASE, "p", "Rust", true);

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
        Assert.assertEquals(codegen.toVarName("PetName"), "pet_name");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toVarName("1PetName"), "param_1_pet_name");
    }

    @Test
    public void testToParamName() {
        // Should be converted to snake case
        Assert.assertEquals(codegen.toParamName("PetName"), "pet_name");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toParamName("1PetName"), "param_1_pet_name");
    }

    @Test
    public void testToOperationId() {
        // Should be converted to camel case
        Assert.assertEquals(codegen.toOperationId("createPet"), "create_pet");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toOperationId("1CreatePet"), "call_1_create_pet");
    }

    @Test
    public void testToModelName() {
        // Should be converted to camel case
        Assert.assertEquals(codegen.toModelName("pet_summary"), "PetSummary");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toModelName("1_pet_summary"), "Model1PetSummary");
    }

    @Test
    public void testToModelFileName() {
        // Should be converted to snake case
        Assert.assertEquals(codegen.toModelFilename("PetSummary"), "pet_summary");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toModelFilename("1PetSummary"), "model_1_pet_summary");
    }

    @Test
    public void testToEnumVarName() {
        // Should be converted to camel case
        Assert.assertEquals(codegen.toEnumVarName("pending", null), "Pending");
        // Enums are often represented in SCREAMING_SNAKE_CASE, check these are also converted to Rust enum camel case
        Assert.assertEquals(codegen.toEnumVarName("SCREAMING_SNAKE_CASE", null), "ScreamingSnakeCase");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toEnumVarName("1_pending", null), "Variant1Pending");
        // Empty strings need to be mapped to "Empty"
        // https://github.com/OpenAPITools/openapi-generator/issues/13453
        Assert.assertEquals(codegen.toEnumVarName("", null), "Empty");
        // Reserved words should be sanitized properly
        // https://github.com/OpenAPITools/openapi-generator/pull/15710
        Assert.assertEquals(codegen.toEnumVarName("type", null), "Type");
        Assert.assertEquals(codegen.toEnumVarName("Self", null), "VariantSelf");
    }

    @Test
    public void testToEnumName() {
        Function<String, String> toEnumName = (String name) -> {
            CodegenProperty property = new CodegenProperty();
            property.baseName = name;
            return codegen.toEnumName(property);
        };
        // Should be converted to camel case
        Assert.assertEquals(toEnumName.apply("pet_status"), "PetStatusWithSuffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(toEnumName.apply("1_pet_status"), "Enum1PetStatusWithSuffix");
    }

    @Test
    public void testToEnumValue() {
        // Value should match spec
        Assert.assertEquals(codegen.toEnumValue("12345valueAbc#!", null), "12345valueAbc#!");
        // Quotes should be escaped so that the Rust string is valid
        Assert.assertEquals(codegen.toEnumValue("\"quote\"", null), "\\\"quote\\\"");
    }

    @Test
    public void testToApiName() {
        // Unnamed
        Assert.assertEquals(codegen.toApiName(""), "DefaultWithSuffix");
        // Should be camel case
        Assert.assertEquals(codegen.toApiName("pet"), "PetWithSuffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toApiName("1_pet"), "Api1PetWithSuffix");
    }

    @Test
    public void testToApiFilename() {
        // Unnamed
        Assert.assertEquals(codegen.toApiFilename(""), "default_with_suffix");
        // Should be snake case
        Assert.assertEquals(codegen.toApiFilename("Pet"), "pet_with_suffix");
        // Prefix is added when starting with a number
        Assert.assertEquals(codegen.toApiFilename("1Pet"), "api_1_pet_with_suffix");
    }

    @Test
    public void testBestFittingIntegerType() {
        final BigInteger u8_MAX = BigInteger.valueOf(255L);
        final BigInteger u16_MAX = BigInteger.valueOf(65_535L);
        final BigInteger u32_MAX = BigInteger.valueOf(4_294_967_295L);

        final BigInteger i8_MIN = BigInteger.valueOf(Byte.MIN_VALUE);
        final BigInteger i16_MIN = BigInteger.valueOf(Short.MIN_VALUE);
        final BigInteger i32_MIN = BigInteger.valueOf(Integer.MIN_VALUE);

        final BigInteger i8_MAX = BigInteger.valueOf(Byte.MAX_VALUE);
        final BigInteger i16_MAX = BigInteger.valueOf(Short.MAX_VALUE);
        final BigInteger i32_MAX = BigInteger.valueOf(Integer.MAX_VALUE);

        // No range specified
        Assert.assertEquals(codegen.bestFittingIntegerType(null, false, null, false, true), "i32");

        // Test when only minimum specified (prefer unsigned)
        Assert.assertEquals(codegen.bestFittingIntegerType(i32_MIN.subtract(BigInteger.ONE), false, null, false, true), "i64");
        Assert.assertEquals(codegen.bestFittingIntegerType(i32_MIN, false, null, false, true), "i32");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.valueOf(-1), false, null, false, true), "i32");

        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, null, false, true), "u32");
        Assert.assertEquals(codegen.bestFittingIntegerType(u32_MAX, false, null, false, true), "u32");
        Assert.assertEquals(codegen.bestFittingIntegerType(u32_MAX.add(BigInteger.ONE), false, null, false, true), "u64");

        // Test when only minimum specified (disable unsigned)
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, null, false, false), "i32");
        Assert.assertEquals(codegen.bestFittingIntegerType(i32_MAX, false, null, false, false), "i32");
        Assert.assertEquals(codegen.bestFittingIntegerType(i32_MAX.add(BigInteger.ONE), false, null, false, false), "i64");

        // Test when only maximum specified
        Assert.assertEquals(codegen.bestFittingIntegerType(null, false, i32_MIN.subtract(BigInteger.ONE), false, true), "i64");
        Assert.assertEquals(codegen.bestFittingIntegerType(null, false, i32_MIN, false, true), "i32");
        Assert.assertEquals(codegen.bestFittingIntegerType(null, false, BigInteger.ZERO, false, true), "i32");

        // Test when maximum bits biggest (prefer unsigned)
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, u8_MAX, false, true), "u8");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, u8_MAX.add(BigInteger.ONE), false, true), "u16");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, u16_MAX, false, true), "u16");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, u16_MAX.add(BigInteger.ONE), false, true), "u32");

        // Test when maximum bits biggest (disable unsigned)
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, i8_MAX, false, false), "i8");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, i8_MAX.add(BigInteger.ONE), false, false), "i16");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, i16_MAX, false, false), "i16");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, false, i16_MAX.add(BigInteger.ONE), false, false), "i32");

        // Test when minimum bits biggest
        Assert.assertEquals(codegen.bestFittingIntegerType(i16_MIN.subtract(BigInteger.ONE), false, BigInteger.ZERO, false, true), "i32");
        Assert.assertEquals(codegen.bestFittingIntegerType(i16_MIN, false, BigInteger.ZERO, false, true), "i16");
        Assert.assertEquals(codegen.bestFittingIntegerType(i8_MIN.subtract(BigInteger.ONE), false, BigInteger.ZERO, false, true), "i16");
        Assert.assertEquals(codegen.bestFittingIntegerType(i8_MIN, false, BigInteger.ZERO, false, true), "i8");

        // Test when exclusive bounds
        Assert.assertEquals(codegen.bestFittingIntegerType(i8_MIN.subtract(BigInteger.ONE), true, BigInteger.ZERO, false, false), "i8");
        Assert.assertEquals(codegen.bestFittingIntegerType(BigInteger.ZERO, true, i8_MAX.add(BigInteger.ONE), true, false), "i8");
    }

    @Test
    public void testCanFitIntoUnsigned() {
        Assert.assertFalse(codegen.canFitIntoUnsigned(BigInteger.valueOf(-1), false));
        Assert.assertTrue(codegen.canFitIntoUnsigned(BigInteger.valueOf(0), false));
        Assert.assertTrue(codegen.canFitIntoUnsigned(BigInteger.valueOf(1), false));

        Assert.assertTrue(codegen.canFitIntoUnsigned(BigInteger.valueOf(-1), true));
    }


    private static class P_AbstractRustCodegen extends AbstractRustCodegen {

        P_AbstractRustCodegen() {
            this.enumSuffix = "WithSuffix";
            this.apiNameSuffix = "WithSuffix";
        }

    }
}
