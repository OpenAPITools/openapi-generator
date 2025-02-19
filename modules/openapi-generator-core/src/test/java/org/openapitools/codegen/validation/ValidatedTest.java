/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.validation;

import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class ValidatedTest {
    @Test
    public void isValidTrueForValidType(){
        boolean isValid = Validated.valid(ValidationRule.empty()).isValid();
        assertTrue(isValid);
    }

    @Test
    public void isValidFalseForInvalidType(){
        boolean isValid = Validated.invalid(ValidationRule.empty(), "test").isValid();
        assertFalse(isValid);
    }
}
