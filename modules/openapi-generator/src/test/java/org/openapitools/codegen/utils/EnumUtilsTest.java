/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.media.JsonSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class EnumUtilsTest {

    @Test
    public void testMergeEnum() {
        StringSchema existing = stringEnum("b", "a", "b");
        existing.setDescription("Existing description");
        StringSchema added = stringEnum("a", "c", "c");
        added.setDescription("New description");

        assertTrue(EnumUtils.mergeEnum(existing, added, EnumUtils.EnumMergeOperation.UNION));

        assertEquals(existing.getEnum(), Arrays.asList("b", "a", "c"));
        assertEquals(existing.getDescription(), "Existing description");
        assertEquals(added.getEnum(), Arrays.asList("a", "c", "c"));
        assertEquals(added.getDescription(), "New description");
    }

    @Test
    public void testMergeEnumWithOpenApi31Types() {
        JsonSchema existing = jsonEnum("a", "string");
        JsonSchema added = jsonEnum("b", "string");

        assertTrue(EnumUtils.mergeEnum(existing, added, EnumUtils.EnumMergeOperation.UNION));
        assertEquals(existing.getEnum(), Arrays.asList("a", "b"));
        assertEquals(added.getEnum(), Collections.singletonList("b"));
    }

    @DataProvider
    public Object[][] incompatibleEnumSchemas() {
        StringSchema existing = stringEnum("a");
        StringSchema emptyEnum = stringEnum();
        Schema<Integer> integerEnum = integerEnum(1);
        JsonSchema multiTypeEnum = jsonEnum("b", "string", "null");
        JsonSchema jsonIntegerEnum = jsonEnum(1, "integer");

        return new Object[][]{
                {null, existing},
                {existing, null},
                {existing, new StringSchema()},
                {new StringSchema(), existing},
                {existing, emptyEnum},
                {emptyEnum, existing},
                {existing, integerEnum},
                {integerEnum, existing},
                {existing, multiTypeEnum},
                {multiTypeEnum, existing},
                {existing, jsonIntegerEnum}
        };
    }

    @Test(dataProvider = "incompatibleEnumSchemas")
    public void testMergeEnumLeavesIncompatibleSchemasUnchanged(Schema<?> existing, Schema<?> added) {
        List<?> existingValues = copyEnumValues(existing);
        List<?> addedValues = copyEnumValues(added);

        assertFalse(EnumUtils.mergeEnum(existing, added, EnumUtils.EnumMergeOperation.UNION));

        if (existing != null) {
            assertEquals(existing.getEnum(), existingValues);
        }
        if (added != null) {
            assertEquals(added.getEnum(), addedValues);
        }
    }

    @Test
    public void testMergeEnumValuesDeduplicatesBothInputsInOrder() {
        List<Object> existing = mutableValues("b", null, "a", "b", null);
        List<Object> added = mutableValues("a", "c", null, "c", 1);

        List<Object> merged = EnumUtils.mergeEnumValues(existing, added, EnumUtils.EnumMergeOperation.UNION);

        assertEquals(merged, Arrays.asList("b", null, "a", "c", 1));
        assertEquals(existing, Arrays.asList("b", null, "a", "b", null));
        assertEquals(added, Arrays.asList("a", "c", null, "c", 1));
        merged.add("d");
        assertEquals(merged, Arrays.asList("b", null, "a", "c", 1, "d"));
    }

    @Test
    public void testMergeEnumValuesWithNullInputs() {
        assertEquals(EnumUtils.mergeEnumValues(null, null, EnumUtils.EnumMergeOperation.UNION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(Arrays.asList("a", "a"), null, EnumUtils.EnumMergeOperation.UNION), Collections.singletonList("a"));
        assertEquals(EnumUtils.mergeEnumValues(null, Arrays.asList("b", "b"), EnumUtils.EnumMergeOperation.UNION), Collections.singletonList("b"));
    }

    @Test
    public void testMergeEnumValuesIntersection() {
        List<Object> existing = mutableValues("b", null, "a", "b", null, 1);
        List<Object> added = mutableValues("a", null, "b", "b", 1.0);

        List<Object> merged = EnumUtils.mergeEnumValues(existing, added, EnumUtils.EnumMergeOperation.INTERSECTION);

        assertEquals(merged, Arrays.asList("b", null, "a"));
        assertEquals(existing, Arrays.asList("b", null, "a", "b", null, 1));
        assertEquals(added, Arrays.asList("a", null, "b", "b", 1.0));
        merged.add("c");
        assertEquals(merged, Arrays.asList("b", null, "a", "c"));
    }

    @Test
    public void testMergeEnumValuesIntersectionWithEmptyOrNullInputs() {
        List<String> values = Arrays.asList("a", "b");
        assertEquals(EnumUtils.mergeEnumValues(null, null, EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(null, values, EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(values, null, EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(values, Collections.emptyList(), EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(Collections.emptyList(), values, EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(values, Collections.singletonList("c"), EnumUtils.EnumMergeOperation.INTERSECTION), Collections.emptyList());
    }

    @Test
    public void testMergeEnumValuesExplicitUnion() {
        assertEquals(EnumUtils.mergeEnumValues(Arrays.asList("b", "a", "b"), Arrays.asList("a", "c"),
                EnumUtils.EnumMergeOperation.UNION), Arrays.asList("b", "a", "c"));
    }

    private static StringSchema stringEnum(String... values) {
        StringSchema schema = new StringSchema();
        schema.setEnum(Arrays.asList(values));
        return schema;
    }

    private static Schema<Integer> integerEnum(int value) {
        Schema<Integer> schema = new Schema<Integer>().type("integer");
        schema.setEnum(Collections.singletonList(value));
        return schema;
    }

    private static JsonSchema jsonEnum(Object value, String... types) {
        JsonSchema schema = new JsonSchema();
        for (String type : types) {
            schema.addType(type);
        }
        schema.setEnum(Collections.singletonList(value));
        return schema;
    }

    private static List<Object> mutableValues(Object... values) {
        return new ArrayList<>(Arrays.asList(values));
    }

    private static List<?> copyEnumValues(Schema<?> schema) {
        return schema == null || schema.getEnum() == null ? null : new ArrayList<>(schema.getEnum());
    }
}
