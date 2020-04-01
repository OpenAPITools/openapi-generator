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

public class ValidationRuleTest {
    class Sample {
        private String name;

        public Sample(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    private static ValidationRule.Result checkName(Sample input) {
        return (input.getName() != null && input.getName().length() > 7) ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private static ValidationRule.Result checkPattern(Sample input) {
        String pattern = "^[A-Z][a-z]*$";
        return (input.getName() != null && input.getName().matches(pattern))  ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    @Test
    public void createMethodUsingMethodReference(){
        Sample nil = new Sample(null);
        Sample six = new Sample("123456");
        Sample seven = new Sample("1234567");
        Sample eight = new Sample("12345678");
        ValidationRule result = ValidationRule.error("test", ValidationRuleTest::checkName);
        assertFalse(result.evaluate(nil).passed());
        assertFalse(result.evaluate(six).passed());
        assertFalse(result.evaluate(seven).passed());
        assertTrue(result.evaluate(eight).passed());
    }

    @Test
    public void createMethodUsingLambda(){
        Sample nil = new Sample(null);
        Sample lowercase = new Sample("jim");
        Sample titlecase = new Sample("Jim");
        ValidationRule result = ValidationRule.error("test", i -> checkPattern((Sample)i));
        assertFalse(result.evaluate(nil).passed());
        assertFalse(result.evaluate(lowercase).passed());
        assertTrue(result.evaluate(titlecase).passed());
    }
}
