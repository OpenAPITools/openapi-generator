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

package org.openapitools.codegen;

import org.testng.annotations.Test;

import java.util.Collections;

import static org.testng.Assert.assertEquals;

public class CodegenEncodingTest {

    @Test
    public void equalValuesAreEqualWhenTheStringsAreDifferentObjects() {
        // A parsed spec gives each encoding its own String objects, never the same instance.
        CodegenEncoding a = new CodegenEncoding(new String("image/png"), Collections.emptyList(), new String("form"), true, false);
        CodegenEncoding b = new CodegenEncoding(new String("image/png"), Collections.emptyList(), new String("form"), true, false);

        assertEquals(a.hashCode(), b.hashCode());
        assertEquals(a, b);
    }
}
