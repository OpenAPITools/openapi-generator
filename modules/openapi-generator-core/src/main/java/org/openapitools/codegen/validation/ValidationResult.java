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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Encapsulates details about the result of a validation test.
 */
public final class ValidationResult {
    private final List<Validated> validations;

    /**
     * Constructs a new {@link ValidationResult} instance, backed by the provided validations (useful for testing).
     *
     * @param validations A pre-defined set of validations to begin with.
     */
    private ValidationResult(List<Validated> validations) {
        this.validations = Collections.synchronizedList(validations);
    }

    /**
     * Constructs a new {@link ValidationResult} instance.
     */
    public ValidationResult() {
        this(new ArrayList<>());
    }

    /**
     * Gets all the validated states resulting from the evaluation. This includes all {@link Valid} and {@link Invalid} instances.
     *
     * @return All validated results.
     */
    public List<Validated> getAll() {
        return  validations;
    }

    /**
     * Gets a filtered list of {@link Valid} states.
     *
     * @return A list containing only {@link Valid} states.
     */
    public List<Valid> getValid(){
        return validations.stream().filter(Validated::isValid).map(it -> (Valid)it).collect(Collectors.toList());
    }

    /**
     * Gets a filters list of {@link Invalid} states with the level of {@link Severity#ERROR}
     *
     * @return A list of all validation errors.
     */
    public List<Invalid> getErrors(){
        return validations.stream()
                .filter(it -> !it.isValid())
                .map(it -> (Invalid)it)
                .filter(it -> it.getSeverity().equals(Severity.ERROR))
                .collect(Collectors.toList());
    }

    /**
     * Gets a filtered list of {@link Invalid} states with the level of {@link Severity#WARNING}
     *
     * @return A list of all validation warnings.
     */
    public List<Invalid> getWarnings(){
        return validations.stream()
                .filter(it -> !it.isValid())
                .map(it -> (Invalid)it)
                .filter(it -> it.getSeverity().equals(Severity.WARNING))
                .collect(Collectors.toList());
    }

    /**
     * Adds a validation state to the final results.
     *
     * @param validated The {@link Valid} or {@link Invalid} instance to add to validations.
     */
    public void addResult(Validated validated) {
        synchronized (validations) {
            ValidationRule rule = validated.getRule();
            if (rule != null && !rule.equals(ValidationRule.empty()) && !validations.contains(validated)) {
                validations.add(validated);
            }
        }
    }

    public ValidationResult consume(ValidationResult other) {
        synchronized (validations) {
            validations.addAll(other.validations);
        }
        return this;
    }
}
