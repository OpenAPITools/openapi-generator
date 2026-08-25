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
    private static Oas31CompositionLowering.CompositionBranchDescriptor branch(
            int index, Oas31CompositionLowering.CompositionBranchDescriptor.NullCapability nullCapability) {
        return new Oas31CompositionLowering.CompositionBranchDescriptor(
                index, null, "string", null, "branch_" + index, nullCapability,
                Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());
    }

}
