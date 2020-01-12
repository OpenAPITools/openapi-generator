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

/**
 * Represents a {@link Validated} state which is "Invalid" to some degree of {@link Severity}.
 */
@SuppressWarnings({"WeakerAccess"})
public final class Invalid extends Validated {
    private String message;
    private ValidationRule rule;
    private String details;

    /**
     * Constructs a new {@link Invalid} instance.
     *
     * @param rule The rule which was evaluated and resulted in this state.
     * @param message The message to be displayed for this invalid state.
     */
    Invalid(ValidationRule rule, String message) {
        this.rule = rule;
        this.message = message;
    }

    /**
     * Constructs a new {@link Invalid} instance.
     *
     * @param rule The rule which was evaluated and resulted in this state.
     * @param message The message to be displayed for this invalid state.
     * @param details Additional contextual details related to the invalid state.
     */
    public Invalid(ValidationRule rule, String message, String details) {
        this(rule, message);
        this.details = details;
    }

    public String getDetails() {
        return details;
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public ValidationRule getRule() {
        return rule;
    }

    /**
     * Get details about the severity of this invalid state.
     * For instance, is this an {@link Severity#ERROR} or simply a {@link Severity#WARNING}.
     *
     * @return The {@link Severity} enum detailing this state's severity.
     */
    public Severity getSeverity() {
        return rule.getSeverity();
    }
}
