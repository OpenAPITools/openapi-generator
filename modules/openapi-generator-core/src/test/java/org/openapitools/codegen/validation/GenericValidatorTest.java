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

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class GenericValidatorTest {
    static class Person {
        private int age;
        private String name;

        Person(String name, int age) {
            this.age = age;
            this.name = name;
        }
    }

    private static ValidationRule.Result checkAge(Person person) {
        return person.age > 0 ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private static ValidationRule.Result checkAdult(Person person) {
        return person.age > 18 ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private static ValidationRule.Result checkName(Person person) {
        return (person.name != null && person.name.length() > 0) ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private static ValidationRule.Result checkNamePattern(Person person) {
        String pattern = "^[A-Z][a-z]*$";
        return person.name.matches(pattern) ? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private static ValidationRule.Result checkNameNormalLength(Person person) {
        return person.name.length() < 10? ValidationRule.Pass.empty() : ValidationRule.Fail.empty();
    }

    private List<ValidationRule> validationRules = Arrays.asList(
            ValidationRule.error("Age must be positive and more than zero", GenericValidatorTest::checkAge),
            ValidationRule.error("Only adults (18 years old and older)", GenericValidatorTest::checkAdult),
            ValidationRule.error("Name isn't set!", GenericValidatorTest::checkName),
            ValidationRule.error("Name isn't formatted correct", GenericValidatorTest::checkNamePattern),
            ValidationRule.warn("Name too long?", "Name may be too long.", GenericValidatorTest::checkNameNormalLength)
    );

    @Test
    public void testGenericValidatorSuccesses(){
        Person person = new Person("Jim", 23);
        GenericValidator<Person> validator = new GenericValidator<>(validationRules);
        ValidationResult result = validator.validate(person);
        List<Validated> validated = result.getAll();
        List<Valid> valid = result.getValid();
        List<Invalid> invalid = result.getErrors();

        assertEquals(validated.size(), 5, "Expected 5 validations to run.");
        assertEquals(valid.size(), 5, "Expected all validations to succeed");
        assertEquals(invalid.size(), 0, "Expected zero validations to fail.");
    }

    @Test
    public void testGenericValidatorSingleConditionFails(){
        Person person = new Person("Jim", 3);
        GenericValidator<Person> validator = new GenericValidator<>(validationRules);
        ValidationResult result = validator.validate(person);
        List<Validated> validated = result.getAll();
        List<Valid> valid = result.getValid();
        List<Invalid> errors = result.getErrors();
        List<Invalid> warnings = result.getWarnings();

        assertEquals(validated.size(), 5, "Expected 5 validations to run.");
        assertEquals(valid.size(), 4, "Expected 4 validations to succeed");
        assertEquals(errors.size(), 1, "Expected 1 validation to fail.");
        assertEquals(warnings.size(), 0, "Expected no warnings to be triggered.");

        Invalid failed = errors.get(0);
        assertEquals(failed.getMessage(), "Only adults (18 years old and older)");
    }

    @Test
    public void testGenericValidatorMultipleConditionsFail(){
        Person person = new Person("asdf", 3);
        GenericValidator<Person> validator = new GenericValidator<>(validationRules);
        ValidationResult result = validator.validate(person);
        List<Validated> validated = result.getAll();
        List<Valid> valid = result.getValid();
        List<Invalid> errors = result.getErrors();
        List<Invalid> warnings = result.getWarnings();

        assertEquals(validated.size(), 5, "Expected 5 validations to run.");
        assertEquals(valid.size(), 3, "Expected 3 validations to succeed");
        assertEquals(errors.size(), 2, "Expected 2 validations to fail.");
        assertEquals(warnings.size(), 0, "Expected no warnings to be triggered.");

        Optional<Invalid> nameValidation = errors.stream().filter(it -> it.getMessage().contains("formatted")).findFirst();
        Optional<Invalid> ageValidation = errors.stream().filter(it -> it.getMessage().contains("adults")).findFirst();

        assertTrue(nameValidation.isPresent(), "Expected validation on name formatting to fail.");
        assertTrue(ageValidation.isPresent(), "Expected validation on age requirements to fail.");
        assertEquals(nameValidation.get().getMessage(), "Name isn't formatted correct");
        assertEquals(ageValidation.get().getMessage(), "Only adults (18 years old and older)");
    }

    @Test
    public void testGenericValidatorErrorsAndWarnings(){
        Person person = new Person("0123456789asdfghjkl", 3);
        GenericValidator<Person> validator = new GenericValidator<>(validationRules);
        ValidationResult result = validator.validate(person);
        List<Validated> validated = result.getAll();
        List<Valid> valid = result.getValid();
        List<Invalid> errors = result.getErrors();
        List<Invalid> warnings = result.getWarnings();

        assertEquals(validated.size(), 5, "Expected 5 validations to run.");
        assertEquals(valid.size(), 2, "Expected 2 validations to succeed");
        assertEquals(errors.size(), 2, "Expected 2 validations to fail.");
        assertEquals(warnings.size(), 1, "Expected 1 warning to be triggered.");

        Optional<Invalid> nameValidation = errors.stream().filter(it -> it.getMessage().contains("formatted")).findFirst();
        Optional<Invalid> ageValidation = errors.stream().filter(it -> it.getMessage().contains("adults")).findFirst();
        Invalid nameLengthWarning = warnings.get(0);

        assertTrue(nameValidation.isPresent(), "Expected validation on name formatting to fail.");
        assertTrue(ageValidation.isPresent(), "Expected validation on age requirements to fail.");
        assertEquals(nameValidation.get().getMessage(), "Name isn't formatted correct");
        assertEquals(ageValidation.get().getMessage(), "Only adults (18 years old and older)");
        assertEquals(nameLengthWarning.getMessage(), "Name may be too long.");
    }
}
