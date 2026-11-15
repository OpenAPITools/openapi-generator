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

import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.testng.Assert.assertEquals;

public class EnumUtilsTest {

    @Test
    public void testMergeEnumValuesDeduplicatesBothInputsInOrder() {
        List<Object> existing = new ArrayList<>(Arrays.asList("b", null, "a", "b", null));
        List<Object> added = new ArrayList<>(Arrays.asList("a", "c", null, "c", 1));

        List<Object> merged = EnumUtils.mergeEnumValues(existing, added);

        assertEquals(merged, Arrays.asList("b", null, "a", "c", 1));
        assertEquals(existing, Arrays.asList("b", null, "a", "b", null));
        assertEquals(added, Arrays.asList("a", "c", null, "c", 1));
        merged.add("d");
        assertEquals(merged, Arrays.asList("b", null, "a", "c", 1, "d"));
    }

    @Test
    public void testMergeEnumValuesWithNullInputs() {
        assertEquals(EnumUtils.mergeEnumValues(null, null), Collections.emptyList());
        assertEquals(EnumUtils.mergeEnumValues(Arrays.asList("a", "a"), null), Collections.singletonList("a"));
        assertEquals(EnumUtils.mergeEnumValues(null, Arrays.asList("b", "b")), Collections.singletonList("b"));
    }
}
