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
 * Provides details about the state of a completed validation.
 */
public abstract class Validated {
    /**
     * Defines whether or not the validation resulted in a "valid" condition.
     *
     * @return <code>true</code> if the instance passed validation of the rule returned by {@link Validated#getRule()}.
     */
    boolean isValid() {
        return false;
    }

    /**
     * Gets the rule which was evaluated and resulted in this state.
     *
     * @return The instance of {@link ValidationRule} which was evaluated.
     */
    abstract ValidationRule getRule();

    /**
     * Gets the message with details about this validated state.
     *
     * @return A string intended to be displayed to a user.
     */
    abstract String getMessage();

    /**
     * Creates an instance of an {@link Invalid} validation state.
     *
     * @param rule The rule which was evaluated.
     * @param message The message to display to a user.
     *
     * @return A {@link Validated} instance representing an invalid state according to the rule.
     */
    public static Validated invalid(ValidationRule rule, String message) {
        return new Invalid(rule, message);
    }
    /**
     * Creates an instance of an {@link Invalid} validation state.
     *
     * @param rule The rule which was evaluated.
     * @param message The message to display to a user.
     * @param details Additional contextual details related to the invalid state.
     *
     * @return A {@link Validated} instance representing an invalid state according to the rule.
     */
    public static Validated invalid(ValidationRule rule, String message, String details) {
        return new Invalid(rule, message, details);
    }

    /**
     * Creates an instance of an {@link Valid} validation state.
     *
     * @param rule The rule which was evaluated.
     *
     * @return A {@link Validated} instance representing a valid state according to the rule.
     */
    public static Validated valid(ValidationRule rule) {
        return new Valid(rule);
    }
}
