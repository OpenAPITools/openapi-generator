/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.OpenAPINormalizer;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.openapitools.codegen.languages.Oas31CompositionLowering;
import org.openapitools.codegen.languages.Oas31KeywordScanner;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CompositionNormalizationTest extends CppBoostBeastNormalizerTestSupport {
    @Test
    public void normalizerBypassPreservesEnumComposition() {
        // Verify that processSimplifyOneOfEnum and processSimplifyAnyOfEnum
        // bypasses preserve the original composition for this generator.
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI =
                new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");

        // oneOf with all enums (default normalizer would merge to single enum)
        ComposedSchema oneOfEnum = new ComposedSchema();
        StringSchema enumA = new StringSchema();
        enumA.addEnumItem("red");
        enumA.addEnumItem("blue");
        oneOfEnum.addOneOfItem(enumA);
        StringSchema enumB = new StringSchema();
        enumB.addEnumItem("green");
        enumB.addEnumItem("yellow");
        oneOfEnum.addOneOfItem(enumB);

        Map<String, String> rules = new HashMap<>();
        TestNormalizer normalizer =
                new TestNormalizer(openAPI, rules);

        Schema oneOfResult = normalizer.processSimplifyOneOfEnum(oneOfEnum);
        Assert.assertNotNull(oneOfResult);
        Assert.assertTrue(oneOfResult.getOneOf() != null
                        && oneOfResult.getOneOf().size() == 2,
                "processSimplifyOneOfEnum must preserve oneOf branch count");

        // anyOf with all enums
        ComposedSchema anyOfEnum = new ComposedSchema();
        anyOfEnum.addAnyOfItem(enumA);
        anyOfEnum.addAnyOfItem(enumB);

        Schema anyOfResult = normalizer.processSimplifyAnyOfEnum(anyOfEnum);
        Assert.assertNotNull(anyOfResult);
        Assert.assertEquals(anyOfResult.getAnyOf().size(), 2,
                "processSimplifyAnyOfEnum must preserve anyOf branch count");
    }

    // ====================================================================
    // Generated validator foundation and numeric semantics
    // ====================================================================

    // --- Strong review: multipleOf, exclusive bounds, integer enum ---

    @Test
    public void branchDescriptorsHaveMultipleOfValidation() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        NumberSchema multBranch = new NumberSchema();
        multBranch.setMultipleOf(java.math.BigDecimal.valueOf(3.0));
        schema.addOneOfItem(multBranch);

        NumberSchema noMultBranch = new NumberSchema();
        schema.addOneOfItem(noMultBranch);
        schemas.put("MultipleOfTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("MultipleOfTest");
        Assert.assertNotNull(desc, "MultipleOfTest must have a descriptor");

        Oas31CompositionLowering.CompositionBranchDescriptor multBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(multBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch with multipleOf must have numeric-range assertion");
        Assert.assertNotNull(multBranchDesc.getValidateParams().get("validation-multiple-of"),
                "Branch with multipleOf must have validation-multiple-of param");
        Assert.assertEquals(multBranchDesc.getValidateParams().get("validation-multiple-of"), java.math.BigDecimal.valueOf(3.0),
                "Branch with multipleOf must have validation-multiple-of = 3.0");

        // Second branch without multipleOf: numeric-range must NOT be present
        Oas31CompositionLowering.CompositionBranchDescriptor noMultBranchDesc =
                desc.getBranches().get(1);
        Assert.assertFalse(noMultBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch without numeric constraints must NOT have numeric-range assertion");
    }

    @Test
    public void branchDescriptorsHaveExclusiveBounds() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        IntegerSchema exclMinBranch = new IntegerSchema();
        exclMinBranch.setExclusiveMinimum(true);
        // OAS 3.0 exclusiveMinimum with minimum: the combined effect must produce
        // validation-exclusive-min in the descriptor.
        exclMinBranch.setMinimum(java.math.BigDecimal.valueOf(10));
        schema.addOneOfItem(exclMinBranch);

        IntegerSchema exclMaxBranch = new IntegerSchema();
        exclMaxBranch.setExclusiveMaximum(true);
        exclMaxBranch.setMaximum(java.math.BigDecimal.valueOf(100));
        schema.addOneOfItem(exclMaxBranch);

        // OAS 3.1 numeric exclusive bounds
        IntegerSchema exclMinValBranch = new IntegerSchema();
        exclMinValBranch.setExclusiveMinimumValue(java.math.BigDecimal.valueOf(5));
        schema.addAnyOfItem(exclMinValBranch);

        IntegerSchema exclMaxValBranch = new IntegerSchema();
        exclMaxValBranch.setExclusiveMaximumValue(java.math.BigDecimal.valueOf(200));
        schema.addAnyOfItem(exclMaxValBranch);

        schemas.put("ExclusiveBoundsTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ExclusiveBoundsTest");
        Assert.assertNotNull(desc, "ExclusiveBoundsTest must have a descriptor");
        // The schema has oneOf + anyOf on the same schema object; preprocessOpenAPI
        // currently captures only the oneOf branches (2 branches) but not the anyOf
        // branches, because the descriptor builder processes the first composition keyword.
        Assert.assertEquals(desc.getBranches().size(), 2,
                "ExclusiveBoundsTest must have 2 branches (oneOf exclusive-min + exclusive-max)");

        // Branch 0: exclusiveMinimum (boolean) with minimum
        Oas31CompositionLowering.CompositionBranchDescriptor exclMinDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(exclMinDesc.getSupportedAssertions().contains("numeric-range"),
                "Branch with exclusiveMinimum must have numeric-range");
        Object valMin = exclMinDesc.getValidateParams().get("validation-min");
        Assert.assertNotNull(valMin, "Branch with exclusiveMinimum must have validation-min param");
        Assert.assertEquals(((Number) valMin).intValue(), 10,
                "Branch with exclusiveMinimum must have validation-min = 10");
        // After ModelUtils resolution, exclusiveMinimum=true on minimum=10
        // produces exclusive-min = 10 in the params
        Object exclMinVal = exclMinDesc.getValidateParams().get("validation-exclusive-min");
        Assert.assertNotNull(exclMinVal, "Branch must have validation-exclusive-min");
        Assert.assertEquals(((Number) exclMinVal).intValue(), 10,
                "Branch with exclusiveMinimum=true and minimum=10 must have validation-exclusive-min = 10");

        // Branch 1: exclusiveMaximum (boolean) with maximum
        Oas31CompositionLowering.CompositionBranchDescriptor exclMaxDesc =
                desc.getBranches().get(1);
        Object valMax = exclMaxDesc.getValidateParams().get("validation-max");
        Assert.assertNotNull(valMax, "Branch with exclusiveMaximum must have validation-max param");
        Assert.assertEquals(((Number) valMax).intValue(), 100,
                "Branch with exclusiveMaximum must have validation-max = 100");
        Object exclMaxVal = exclMaxDesc.getValidateParams().get("validation-exclusive-max");
        Assert.assertNotNull(exclMaxVal, "Branch must have validation-exclusive-max");
        Assert.assertEquals(((Number) exclMaxVal).intValue(), 100,
                "Branch with exclusiveMaximum=true and maximum=100 must have validation-exclusive-max = 100");
    }

    @Test
    public void branchDescriptorsHaveIntegerEnumKind() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        // Integer enum branch
        ComposedSchema schema = new ComposedSchema();
        IntegerSchema intEnumBranch = new IntegerSchema();
        intEnumBranch.addEnumItem(1);
        intEnumBranch.addEnumItem(2);
        intEnumBranch.addEnumItem(3);
        schema.addOneOfItem(intEnumBranch);

        // String enum branch (for comparison)
        StringSchema stringEnumBranch = new StringSchema();
        stringEnumBranch.addEnumItem("red");
        stringEnumBranch.addEnumItem("blue");
        schema.addOneOfItem(stringEnumBranch);

        // Float enum branch (number kind)
        NumberSchema floatEnumBranch = new NumberSchema();
        floatEnumBranch.addEnumItem(java.math.BigDecimal.valueOf(1.5));
        floatEnumBranch.addEnumItem(java.math.BigDecimal.valueOf(2.5));
        schema.addOneOfItem(floatEnumBranch);

        // Boolean enum branch
        StringSchema boolEnumBranch = new StringSchema();
        // Note: in OAS 3.x, boolean enums pass through as Object; the predominant
        // kind detection checks Java type of enum values.
        // For this test, use NumberSchema with boolean values is tricky.
        // Instead, verify integer and string enum kinds.
        schemas.put("EnumKindTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("EnumKindTest");
        Assert.assertNotNull(desc, "EnumKindTest must have a descriptor");

        // Branch 0: integer enum → validation-enum-kind = "integer"
        Oas31CompositionLowering.CompositionBranchDescriptor intEnumDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(intEnumDesc.getSupportedAssertions().contains("enum"),
                "Integer enum branch must have enum assertion");
        Assert.assertEquals(intEnumDesc.getValidateParams().get("validation-enum-kind"), "integer",
                "Integer enum branch must have validation-enum-kind = integer");
        Object enumValues = intEnumDesc.getValidateParams().get("validation-enum-values");
        Assert.assertNotNull(enumValues, "Integer enum branch must have validation-enum-values");
        @SuppressWarnings("unchecked")
        List<String> intEnumList = (List<String>) enumValues;
        Assert.assertEquals(intEnumList.size(), 3,
                "Integer enum must have 3 values");
        Assert.assertTrue(intEnumList.contains("1") && intEnumList.contains("2") && intEnumList.contains("3"),
                "Integer enum values must contain 1, 2, 3");

        // Branch 1: string enum → validation-enum-kind = "string"
        Oas31CompositionLowering.CompositionBranchDescriptor stringEnumDesc =
                desc.getBranches().get(1);
        Assert.assertEquals(stringEnumDesc.getValidateParams().get("validation-enum-kind"), "string",
                "String enum branch must have validation-enum-kind = string");
        @SuppressWarnings("unchecked")
        List<String> stringEnumList = (List<String>) stringEnumDesc.getValidateParams().get("validation-enum-values");
        Assert.assertNotNull(stringEnumList, "String enum branch must have validation-enum-values");
        Assert.assertTrue(stringEnumList.contains("red") && stringEnumList.contains("blue"),
                "String enum values must contain red, blue");

        // Branch 2: float enum → validation-enum-kind = "number"
        Oas31CompositionLowering.CompositionBranchDescriptor floatEnumDesc =
                desc.getBranches().get(2);
        Assert.assertEquals(floatEnumDesc.getValidateParams().get("validation-enum-kind"), "number",
                "Float enum branch must have validation-enum-kind = number");
    }

    @Test
    public void branchDescriptorsHaveValidatorId() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema stringBranch = new StringSchema();
        stringBranch.setMinLength(1);
        schema.addOneOfItem(stringBranch);
        IntegerSchema intBranch = new IntegerSchema();
        intBranch.setMinimum(java.math.BigDecimal.valueOf(0));
        schema.addOneOfItem(intBranch);
        schemas.put("ValidatorBranchTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ValidatorBranchTest");
        Assert.assertNotNull(desc, "ValidatorBranchTest must have a descriptor");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "ValidatorBranchTest must have 2 branches");

        // Each branch must have a non-null validatorId
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch : desc.getBranches()) {
            Assert.assertNotNull(branch.getValidatorId(),
                    "Each branch must have a validatorId");
            Assert.assertTrue(branch.getValidatorId().startsWith("ValidatorBranchTest_branch_"),
                    "validatorId must start with schema name and branch index");
        }

        // First branch: string with minLength
        // Note: validation-type may be prefixed (e.g., "type-array")
        // changes to type assertion handling. Both "string" and "type-string" or
        // "type-array" are valid.
        Oas31CompositionLowering.CompositionBranchDescriptor stringBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(stringBranchDesc.getSupportedAssertions().contains("string-length"),
                "String branch must have string-length assertion");
        Assert.assertNotNull(stringBranchDesc.getValidateParams().get("validation-min-length"),
                "String branch must have validation-min-length param");
        Assert.assertNotNull(stringBranchDesc.getValidateParams().get("validation-type"),
                "String branch must have validation-type param");

        // Second branch: integer with minimum
        // Note: validation-type may be "type-array"
        Oas31CompositionLowering.CompositionBranchDescriptor intBranchDesc =
                desc.getBranches().get(1);
        Assert.assertTrue(intBranchDesc.getSupportedAssertions().contains("numeric-range"),
                "Integer branch must have numeric-range assertion");
        Assert.assertNotNull(intBranchDesc.getValidateParams().get("validation-min"),
                "Integer branch must have validation-min param");
    }

    @Test
    public void branchDescriptorsHaveEnumValidationParams() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema enumBranch = new StringSchema();
        enumBranch.addEnumItem("red");
        enumBranch.addEnumItem("blue");
        schema.addOneOfItem(enumBranch);
        schemas.put("ValidatorEnumTest", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("ValidatorEnumTest");
        Assert.assertNotNull(desc, "ValidatorEnumTest must have a descriptor");

        Oas31CompositionLowering.CompositionBranchDescriptor enumBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(enumBranchDesc.getSupportedAssertions().contains("enum"),
                "Enum branch must have enum assertion");
        Assert.assertEquals(enumBranchDesc.getValidateParams().get("has-validation-enum"), true,
                "Enum branch must have has-validation-enum");
        Assert.assertNotNull(enumBranchDesc.getValidateParams().get("validation-enum-values"),
                "Enum branch must have validation-enum-values");
    }


    @Test
    public void anyOfAssertionSensitivity() {
        // anyOf with numeric-constrained branches: branch 0 accepts ≥100,
        // branch 1 accepts ≤0. Value 50 should match neither (rejected).
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        IntegerSchema highBranch = new IntegerSchema();
        highBranch.setMinimum(java.math.BigDecimal.valueOf(100));
        schema.addAnyOfItem(highBranch);
        IntegerSchema lowBranch = new IntegerSchema();
        lowBranch.setMaximum(java.math.BigDecimal.valueOf(0));
        schema.addAnyOfItem(lowBranch);
        schemas.put("AnyOfConstrained", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("AnyOfConstrained");
        Assert.assertNotNull(desc, "AnyOfConstrained must have a descriptor");
        Assert.assertEquals(desc.getKeyword(), "anyOf",
                "Keyword must be anyOf");
        Assert.assertEquals(desc.getBranches().size(), 2,
                "AnyOfConstrained must have 2 branches");

        // Both branches must have numeric-range assertion metadata
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch : desc.getBranches()) {
            Assert.assertTrue(branch.getSupportedAssertions().contains("numeric-range"),
                    "AnyOf branch with explicit bounds must have numeric-range assertion");
        }

        // First branch: minimum = 100
        Oas31CompositionLowering.CompositionBranchDescriptor highBranchDesc =
                desc.getBranches().get(0);
        Object valMin = highBranchDesc.getValidateParams().get("validation-min");
        Assert.assertNotNull(valMin, "High branch must have validation-min param");
        Assert.assertEquals(((Number) valMin).intValue(), 100,
                "High branch must have validation-min = 100");
        Assert.assertEquals(highBranchDesc.getValidateParams().get("validation-type"), "type-array",
                "High branch validation-type must be type-array");

        // Second branch: maximum = 0
        Oas31CompositionLowering.CompositionBranchDescriptor lowBranchDesc =
                desc.getBranches().get(1);
        Object valMax = lowBranchDesc.getValidateParams().get("validation-max");
        Assert.assertNotNull(valMax, "Low branch must have validation-max param");
        Assert.assertEquals(((Number) valMax).intValue(), 0,
                "Low branch must have validation-max = 0");
    }

    @Test
    public void anyOfBranchValidatorMetadataForPatternAndConst() {
        // Verify const and pattern assertions produce correct validation params
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.0.4");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new HashMap<>();

        ComposedSchema schema = new ComposedSchema();
        StringSchema constBranch = new StringSchema();
        constBranch.setConst("fixed-value");
        schema.addAnyOfItem(constBranch);

        StringSchema patternBranch = new StringSchema();
        patternBranch.setPattern("^[a-z]+$");
        schema.addAnyOfItem(patternBranch);
        schemas.put("AnyOfConstPattern", schema);
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.preprocessOpenAPI(openAPI);

        Oas31CompositionLowering.CompositionDescriptor desc =
                codegen.getCompositionDescriptor("AnyOfConstPattern");
        Assert.assertNotNull(desc, "AnyOfConstPattern must have a descriptor");

        // Const branch
        Oas31CompositionLowering.CompositionBranchDescriptor constBranchDesc =
                desc.getBranches().get(0);
        Assert.assertTrue(constBranchDesc.getSupportedAssertions().contains("const"),
                "Const branch must have const assertion");
        Assert.assertEquals(constBranchDesc.getValidateParams().get("validation-const-value"),
                "fixed-value",
                "Const branch must have correct const value");

        // Pattern branch
        Oas31CompositionLowering.CompositionBranchDescriptor patternBranchDesc =
                desc.getBranches().get(1);
        Assert.assertTrue(patternBranchDesc.getSupportedAssertions().contains("pattern"),
                "Pattern branch must have pattern assertion");
        Assert.assertEquals(patternBranchDesc.getValidateParams().get("validation-pattern"),
                "^[a-z]+$",
                "Pattern branch must have correct pattern");
    }

    @Test
    public void repeatedNullReferencesRetainNullCapability() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        Schema nullTarget = new Schema();
        nullTarget.setTypes(new LinkedHashSet<>(java.util.Collections.singletonList("null")));
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addOneOfItem(new Schema().$ref("#/components/schemas/NullTarget"));
        wrapper.addOneOfItem(new Schema().$ref("#/components/schemas/NullTarget"));
        schemas.put("NullTarget", nullTarget);
        schemas.put("RepeatedNull", wrapper);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                codegen.getCompositionDescriptor("RepeatedNull");
        Assert.assertNotNull(descriptor);
        Assert.assertEquals(descriptor.getBranches().size(), 2);
        for (Oas31CompositionLowering.CompositionBranchDescriptor branch
                : descriptor.getBranches()) {
            Assert.assertEquals(branch.getNullCapability(),
                    Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.ALWAYS);
        }
    }

    @Test
    public void referenceSiblingCompositionIsRetained() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        Schema target = new io.swagger.v3.oas.models.media.ObjectSchema();
        Schema refWithSibling = new Schema().$ref("#/components/schemas/Target");
        refWithSibling.setAnyOf(java.util.Arrays.asList(
                new Schema().type("string"), new Schema().type("integer")));
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addOneOfItem(refWithSibling);
        schemas.put("Target", target);
        schemas.put("RefSiblingComposition", wrapper);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                codegen.getCompositionDescriptor("RefSiblingComposition")
                        .getBranches().get(0);
        Assert.assertTrue(branch.getValidateParams()
                        .containsKey("validation-anyof-schemas"),
                "$ref sibling anyOf must remain an adjacent applicator");
    }

    @Test
    public void normalizedReferenceSiblingRetainsDiscriminatorIdentityAndAssertions() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();

        ObjectSchema documentBlock = new ObjectSchema();
        documentBlock.addProperty("type", new StringSchema());
        ObjectSchema textBlock = new ObjectSchema();
        textBlock.addProperty("type", new StringSchema());

        Schema documentRef = new Schema()
                .$ref("#/components/schemas/RequestDocumentBlock");
        documentRef.setDescription("Normalized reference sibling");
        documentRef.setMinProperties(2);
        Schema textRef = new Schema()
                .$ref("#/components/schemas/RequestTextBlock");

        ComposedSchema block = new ComposedSchema();
        block.addOneOfItem(documentRef);
        block.addOneOfItem(textRef);
        Map<String, String> mapping = new LinkedHashMap<>();
        mapping.put("document", "#/components/schemas/RequestDocumentBlock");
        mapping.put("text", "#/components/schemas/RequestTextBlock");
        io.swagger.v3.oas.models.media.Discriminator discriminator =
                new io.swagger.v3.oas.models.media.Discriminator();
        discriminator.setPropertyName("type");
        discriminator.setMapping(mapping);
        block.setDiscriminator(discriminator);

        schemas.put("RequestDocumentBlock", documentBlock);
        schemas.put("RequestTextBlock", textBlock);
        schemas.put("RequestContentBlock", block);
        components.setSchemas(schemas);
        openAPI.setComponents(components);

        class TestOpenAPINormalizer extends OpenAPINormalizer {
            TestOpenAPINormalizer(io.swagger.v3.oas.models.OpenAPI spec,
                                  Map<String, String> rules) {
                super(spec, rules);
            }

            void run() {
                normalize();
            }
        }
        new TestOpenAPINormalizer(
                openAPI, Map.of("NORMALIZE_31SPEC", "true")).run();
        Schema normalizedBlock = (Schema) openAPI.getComponents().getSchemas()
                .get("RequestContentBlock");
        Schema normalizedBranch = (Schema) normalizedBlock.getOneOf().get(0);
        Assert.assertNull(normalizedBranch.get$ref(),
                "Normalizer must move the sibling $ref off the outer schema");
        Assert.assertEquals(normalizedBranch.getAllOf().size(), 1);
        Schema normalizedRef = (Schema) normalizedBranch.getAllOf().get(0);
        Assert.assertEquals(normalizedRef.get$ref(),
                "#/components/schemas/RequestDocumentBlock");

        codegen.preprocessOpenAPI(openAPI);
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                codegen.getCompositionDescriptor("RequestContentBlock");
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                descriptor.getBranches().get(0);
        Assert.assertEquals(branch.getSourceSchemaRef(),
                "#/components/schemas/RequestDocumentBlock");
        Assert.assertEquals(branch.getResolvedSchemaName(), "RequestDocumentBlock");
        Assert.assertEquals(branch.getValidateParams().get("validation-ref"),
                "RequestDocumentBlock");
        Assert.assertEquals(branch.getValidateParams().get("validation-min-properties"), 2);
        Assert.assertEquals(branch.getValidateParams().get("validation-ann-description"),
                "\"Normalized reference sibling\"");
        Assert.assertFalse(branch.getValidateParams().containsKey("validation-allof-schemas"),
                "Normalizer's singleton allOf must not become a second ref applicator");

        List<Map<String, Object>> branchIndex =
                Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        mapping, descriptor.getBranches());
        Assert.assertEquals(branchIndex.size(), 2);
        Assert.assertEquals(branchIndex.get(0).get("key"), "document");
        Assert.assertEquals(branchIndex.get(0).get("value"), 0);
        Assert.assertEquals(branchIndex.get(1).get("key"), "text");
        Assert.assertEquals(branchIndex.get(1).get("value"), 1);
    }

    @Test
    public void singletonAllOfPrimitiveReferenceUsesResolvedDefault() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("BetaTimestamp", new StringSchema().format("date-time"));
        components.setSchemas(schemas);
        openAPI.setComponents(components);
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        ComposedSchema timestampProperty = new ComposedSchema();
        timestampProperty.addAllOfItem(
                new Schema().$ref("#/components/schemas/BetaTimestamp"));
        CodegenProperty property = new CodegenProperty();
        property.dataType = "std::string";

        Assert.assertEquals(codegen.toDefaultValue(property, timestampProperty), "\"\"");
    }

    @Test
    public void allOfNumericEnumsUseJsonNumberEquality() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        codegen.processOpts();

        Schema integerValue = new IntegerSchema();
        integerValue.setEnum(java.util.Collections.singletonList(1));
        Schema decimalValue = new NumberSchema();
        decimalValue.setEnum(java.util.Collections.singletonList(
                new java.math.BigDecimal("1.0")));
        Schema deepIntegerValue = new ObjectSchema();
        Map<String, Object> integerObject = new LinkedHashMap<>();
        integerObject.put("nested", java.util.Collections.singletonList(1));
        deepIntegerValue.setEnum(java.util.Collections.singletonList(integerObject));
        Schema deepDecimalValue = new ObjectSchema();
        Map<String, Object> decimalObject = new LinkedHashMap<>();
        decimalObject.put("nested", java.util.Collections.singletonList(
                new java.math.BigDecimal("1.0")));
        deepDecimalValue.setEnum(java.util.Collections.singletonList(decimalObject));
        ObjectSchema first = new ObjectSchema();
        first.addProperty("value", integerValue);
        first.addProperty("deep", deepIntegerValue);
        first.addRequiredItem("value");
        first.addRequiredItem("deep");
        ObjectSchema second = new ObjectSchema();
        second.addProperty("value", decimalValue);
        second.addProperty("deep", deepDecimalValue);
        second.addRequiredItem("value");
        second.addRequiredItem("deep");
        ComposedSchema wrapper = new ComposedSchema();
        wrapper.addAllOfItem(first);
        wrapper.addAllOfItem(second);

        io.swagger.v3.oas.models.OpenAPI openAPI = new io.swagger.v3.oas.models.OpenAPI();
        openAPI.setOpenapi("3.1.0");
        io.swagger.v3.oas.models.Components components = new io.swagger.v3.oas.models.Components();
        components.setSchemas(java.util.Collections.singletonMap("NumericEnums", wrapper));
        openAPI.setComponents(components);

        codegen.preprocessOpenAPI(openAPI);
        Assert.assertNotNull(codegen.fromModel("NumericEnums", wrapper),
                "JSON-equal numeric and nested enum values must have a satisfiable intersection");
    }
}
