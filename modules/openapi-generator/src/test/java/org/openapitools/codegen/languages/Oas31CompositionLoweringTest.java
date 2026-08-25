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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Oas31CompositionLoweringTest {
    @Test(expectedExceptions = CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException.class)
    public void rejectsUnsupportedAllOfAssertions() {
        Oas31CompositionLowering.CompositionBranchDescriptor branch =
                new Oas31CompositionLowering.CompositionBranchDescriptor(
                        0, null, "object", null, "all_of_branch_0",
                        Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.NEVER,
                        Collections.emptyList(), List.of("unsupported-assertion"),
                        Collections.emptyMap());
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                new Oas31CompositionLowering.CompositionDescriptor(
                        "AllOf", "#/components/schemas/AllOf", "allOf", List.of(branch), null);

        Oas31CompositionLowering.validateDescriptorAssertions(descriptor);
    }

    @Test
    public void anyOfWithDuplicateNullBranchesRemainsNullable() {
        List<CppBoostBeastClientCodegen.ComposedBranch> branches = List.of(
                new CppBoostBeastClientCodegen.ComposedBranch(
                        "std::nullptr_t", false, false, 0),
                new CppBoostBeastClientCodegen.ComposedBranch(
                        "std::nullptr_t", false, false, 1),
                new CppBoostBeastClientCodegen.ComposedBranch(
                        "std::string", false, true, 2));
        List<Oas31CompositionLowering.CompositionBranchDescriptor> descriptorBranches = List.of(
                branch(0, Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.ALWAYS),
                branch(1, Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.ALWAYS),
                branch(2, Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability.NEVER));
        Oas31CompositionLowering.CompositionDescriptor descriptor =
                new Oas31CompositionLowering.CompositionDescriptor(
                        "NullableString", "#/components/schemas/NullableString", "anyOf",
                        descriptorBranches, null);

        Assert.assertEquals(Oas31CompositionLowering.lowerComposedTypes(
                branches, "anyOf", descriptor, message -> Assert.fail(message)),
                "std::optional<std::string>");
    }


    @Test
    public void allOfKeepsTheTightestNumericBoundary() {
        ComposedSchema schema = new ComposedSchema();
        NumberSchema exclusiveFive = new NumberSchema();
        exclusiveFive.setExclusiveMinimumValue(new BigDecimal("5"));
        NumberSchema inclusiveTen = new NumberSchema();
        inclusiveTen.setMinimum(new BigDecimal("10"));
        schema.addAllOfItem(exclusiveFive);
        schema.addAllOfItem(inclusiveTen);

        Oas31CompositionLowering.AllOfIntersection intersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "AllOfNumber", schema, new OpenAPI(), Collections.emptyMap(), new HashSet<>());

        Assert.assertEquals(intersection.getRootMinimum(), new BigDecimal("10"));
        Assert.assertNull(intersection.getRootExclusiveMinimum(),
                "A smaller exclusive minimum must not make a larger inclusive bound strict");
        Assert.assertNull(intersection.getRootExclusiveMinimumValue());
    }

    @Test
    public void allOfClosesAdditionalPropertiesWhenAnyContributorDoes() {
        ComposedSchema schema = new ComposedSchema();
        ObjectSchema typedAdditionalProperties = new ObjectSchema();
        typedAdditionalProperties.setAdditionalProperties(new StringSchema());
        ObjectSchema closedObject = new ObjectSchema();
        closedObject.setAdditionalProperties(false);
        schema.addAllOfItem(typedAdditionalProperties);
        schema.addAllOfItem(closedObject);

        Oas31CompositionLowering.AllOfIntersection intersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "ClosedObject", schema, new OpenAPI(), Collections.emptyMap(), new HashSet<>());

        Assert.assertEquals(intersection.getAdditionalProperties(), Boolean.FALSE);
    }

    @Test
    public void allOfIntersectsReferenceSiblingsAndTerminatesRecursiveProperties() {
        Schema<?> constrained = new NumberSchema();
        constrained.setMinimum(new BigDecimal("1"));
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("Constrained", constrained);

        Schema<?> referencedProperty = new Schema<>().$ref("#/components/schemas/Constrained");
        referencedProperty.setMaximum(new BigDecimal("10"));
        ObjectSchema first = new ObjectSchema();
        first.addProperties("value", referencedProperty);
        ObjectSchema second = new ObjectSchema();
        NumberSchema secondProperty = new NumberSchema();
        secondProperty.setMinimum(new BigDecimal("5"));
        second.addProperties("value", secondProperty);
        ComposedSchema schema = new ComposedSchema();
        schema.addAllOfItem(first);
        schema.addAllOfItem(second);

        OpenAPI openAPI = new OpenAPI().components(new Components().schemas(schemas));
        Oas31CompositionLowering.AllOfIntersection intersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "ReferenceSiblings", schema, openAPI, schemas, new HashSet<>());
        Schema<?> value = intersection.getProperties().get("value");
        Assert.assertEquals(value.getMinimum(), new BigDecimal("5"));
        Assert.assertEquals(value.getMaximum(), new BigDecimal("10"),
                "Reference-node sibling assertions must survive target resolution");

        Schema<?> recursive = new ObjectSchema();
        Schema<?> childReference = new Schema<>().$ref("#/components/schemas/Recursive");
        recursive.setProperties(Collections.singletonMap("child", childReference));
        schemas.put("Recursive", recursive);
        ComposedSchema cyclicSchema = new ComposedSchema();
        cyclicSchema.addAllOfItem(new Schema<>().$ref("#/components/schemas/Recursive"));
        cyclicSchema.addAllOfItem(new Schema<>().$ref("#/components/schemas/Recursive"));

        Assert.assertNotNull(Oas31CompositionLowering.computeAllOfIntersection(
                "RecursiveAllOf", cyclicSchema, openAPI, schemas, new HashSet<>()));
    }

    @Test
    public void allOfPreservesReferenceTypeArrayNullableAndRequiredSiblings() {
        Schema<?> target = new Schema<>();
        target.setRequired(Arrays.asList("fromReference"));
        Map<String, Schema> schemas = new LinkedHashMap<>();
        schemas.put("Referenced", target);

        Schema<?> referenceWithSiblings = new Schema<>().$ref("#/components/schemas/Referenced");
        referenceWithSiblings.addType("object");
        referenceWithSiblings.addType("null");
        referenceWithSiblings.setNullable(true);
        referenceWithSiblings.setRequired(Arrays.asList("fromReferenceSibling"));
        ObjectSchema first = new ObjectSchema();
        first.addProperties("payload", referenceWithSiblings);

        Schema<?> nestedConstraint = new Schema<>();
        nestedConstraint.setRequired(Arrays.asList("fromOtherContributor"));
        ObjectSchema second = new ObjectSchema();
        second.addProperties("payload", nestedConstraint);

        ComposedSchema schema = new ComposedSchema();
        schema.addAllOfItem(first);
        schema.addAllOfItem(second);
        Oas31CompositionLowering.AllOfIntersection intersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "ReferenceSiblingTypes", schema,
                        new OpenAPI().components(new Components().schemas(schemas)),
                        schemas, new HashSet<>());

        Schema<?> payload = intersection.getProperties().get("payload");
        Assert.assertEquals(new HashSet<>(payload.getTypes()),
                new HashSet<>(Arrays.asList("object", "null")));
        Assert.assertTrue(Boolean.TRUE.equals(payload.getNullable()));
        Assert.assertEquals(new HashSet<>(payload.getRequired()),
                new HashSet<>(Arrays.asList(
                        "fromReference", "fromReferenceSibling", "fromOtherContributor")));

        Schema<?> syntheticPayload = (Schema<?>) Oas31CompositionLowering
                .buildSyntheticAllOfSchema("ReferenceSiblingTypes", intersection)
                .getProperties().get("payload");
        Assert.assertEquals(new HashSet<>(syntheticPayload.getTypes()),
                new HashSet<>(Arrays.asList("object", "null")));
        Assert.assertTrue(Boolean.TRUE.equals(syntheticPayload.getNullable()));
        Assert.assertEquals(new HashSet<>(syntheticPayload.getRequired()),
                new HashSet<>(Arrays.asList(
                        "fromReference", "fromReferenceSibling", "fromOtherContributor")));
    }

    @Test
    public void allOfRequiresEveryPropertySchemaToAllowNull() {
        ObjectSchema nullableProperty = new ObjectSchema();
        nullableProperty.setNullable(true);
        ObjectSchema nonNullableProperty = new ObjectSchema();
        ObjectSchema first = new ObjectSchema();
        first.addProperties("payload", nullableProperty);
        ObjectSchema second = new ObjectSchema();
        second.addProperties("payload", nonNullableProperty);
        ComposedSchema schema = new ComposedSchema();
        schema.addAllOfItem(first);
        schema.addAllOfItem(second);

        Oas31CompositionLowering.AllOfIntersection intersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "NullableIntersection", schema, new OpenAPI(),
                        Collections.emptyMap(), new HashSet<>());
        Schema<?> payload = intersection.getProperties().get("payload");
        Assert.assertFalse(Boolean.TRUE.equals(payload.getNullable()));

        Schema<?> syntheticPayload = (Schema<?>) Oas31CompositionLowering
                .buildSyntheticAllOfSchema("NullableIntersection", intersection)
                .getProperties().get("payload");
        Assert.assertFalse(Boolean.TRUE.equals(syntheticPayload.getNullable()));
    }

    @Test
    public void allOfNullabilityRespectsEnumAndConst() {
        Schema<?> nullableProperty = new Schema<>();
        nullableProperty.setNullable(true);

        Schema<Object> enumProperty = new Schema<>();
        enumProperty.setEnum(Collections.singletonList("non-null"));
        ObjectSchema enumFirst = new ObjectSchema();
        enumFirst.addProperties("payload", nullableProperty);
        ObjectSchema enumSecond = new ObjectSchema();
        enumSecond.addProperties("payload", enumProperty);
        ComposedSchema enumSchema = new ComposedSchema();
        enumSchema.addAllOfItem(enumFirst);
        enumSchema.addAllOfItem(enumSecond);

        Oas31CompositionLowering.AllOfIntersection enumIntersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "EnumIntersection", enumSchema, new OpenAPI(),
                        Collections.emptyMap(), new HashSet<>());
        Schema<?> enumPayload = (Schema<?>) Oas31CompositionLowering
                .buildSyntheticAllOfSchema("EnumIntersection", enumIntersection)
                .getProperties().get("payload");
        Assert.assertFalse(Boolean.TRUE.equals(enumPayload.getNullable()));

        Schema<?> constProperty = new Schema<>();
        constProperty.setConst("non-null");
        ObjectSchema constFirst = new ObjectSchema();
        constFirst.addProperties("payload", nullableProperty);
        ObjectSchema constSecond = new ObjectSchema();
        constSecond.addProperties("payload", constProperty);
        ComposedSchema constSchema = new ComposedSchema();
        constSchema.addAllOfItem(constFirst);
        constSchema.addAllOfItem(constSecond);

        Oas31CompositionLowering.AllOfIntersection constIntersection =
                Oas31CompositionLowering.computeAllOfIntersection(
                        "ConstIntersection", constSchema, new OpenAPI(),
                        Collections.emptyMap(), new HashSet<>());
        Schema<?> constPayload = (Schema<?>) Oas31CompositionLowering
                .buildSyntheticAllOfSchema("ConstIntersection", constIntersection)
                .getProperties().get("payload");
        Assert.assertFalse(Boolean.TRUE.equals(constPayload.getNullable()));
    }

    private static Oas31CompositionLowering.CompositionBranchDescriptor branch(
            int index, Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability nullCapability) {
        return new Oas31CompositionLowering.CompositionBranchDescriptor(
                index, null, "string", null, "branch_" + index, nullCapability,
                Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());
    }

}
