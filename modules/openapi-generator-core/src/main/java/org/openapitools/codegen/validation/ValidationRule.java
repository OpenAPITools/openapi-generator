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

import java.util.function.Function;

/**
 * Defines a rule to be evaluated against some target object.
 */
@SuppressWarnings("WeakerAccess")
public class ValidationRule {
    private Severity severity;
    private String description;
    private String failureMessage;
    private Function<Object, Result> test;

    /**
     * Constructs a new instance of {@link ValidationRule}
     *
     * @param severity The declared severity if this validation rule fails.
     * @param description A description to help differentiate this rule from others (not intended to be user-facing).
     * @param failureMessage The message to be displayed in the event of a test failure (intended to be user-facing).
     * @param test The test condition to be applied as a part of this rule, when this function returns <code>true</code>,
     *             the evaluated instance will be considered "valid" according to this rule.
     */
    ValidationRule(Severity severity, String description, String failureMessage, Function<Object, Result> test) {
        this.severity = severity;
        this.description = description;
        this.failureMessage = failureMessage;
        this.test = test;
    }

    /**
     * Gets the message to be displayed in the event of a test failure (intended to be user-facing).
     *
     * @return A string message
     */
    public String getFailureMessage() {
        return failureMessage;
    }

    /**
     * Evalute an instance of an object against this rule.
     *
     * @param input The instance to be evaluated.
     *
     * @return <code>true</code> if the object state is valid according to this rule, otherwise <code>false</code>.
     */
    public Result evaluate(Object input) {
        return test.apply(input);
    }

    /**
     * Get the level of severity which this rule considers a failure in evaluation. For example, if this is {@link Severity#WARNING} and
     * a call to {@link ValidationRule#evaluate(Object)} returns <code>false</code>, a user should not expect an error to be thrown under
     * normal operation.
     *
     * @return An enum defining how severe a failure to evaluate this rule should be considered by the caller.
     */
    public Severity getSeverity() {
        return severity;
    }

    /**
     * Gets a description to help differentiate this rule from others (not intended to be user-facing).
     *
     * @return A string description.
     */
    public String getDescription() {
        return description;
    }

    /**
     * Constructs an empty rule (useful for testing).
     *
     * @return An "empty" rule.
     */
    static ValidationRule empty() {
        return new ValidationRule(Severity.ERROR, "empty", "failure message", (i) -> Fail.empty() );
    }

    /**
     * Create an instance of a {@link ValidationRule}
     *
     * @param severity The declared severity if this validation rule fails.
     * @param description A description to help differentiate this rule from others (not intended to be user-facing).
     * @param failureMessage The message to be displayed in the event of a test failure (intended to be user-facing).
     * @param fn The test condition to be applied as a part of this rule, when this function returns <code>true</code>,
     *             the evaluated instance will be considered "valid" according to this rule.
     * @param <T> The type of the object being evaluated.
     *
     * @return A new instance of a {@link ValidationRule}
     */
    @SuppressWarnings("unchecked")
    public static <T> ValidationRule create(Severity severity, String description, String failureMessage, Function<T, Result> fn) {
        return new ValidationRule(severity, description, failureMessage, (Function<Object, Result>) fn);
    }

    /**
     * Create an instance of a {@link ValidationRule} which should result in an error should the evaluate of this rule fail.
     *
     * @param failureMessage The message to be displayed in the event of a test failure (intended to be user-facing).
     * @param fn The test condition to be applied as a part of this rule, when this function returns <code>true</code>,
     *             the evaluated instance will be considered "valid" according to this rule.
     * @param <T> The type of the object being evaluated.
     *
     * @return A new instance of a {@link ValidationRule}
     */
    @SuppressWarnings("unchecked")
    public static <T> ValidationRule error(String failureMessage, Function<T, Result> fn) {
        return new ValidationRule(Severity.ERROR, null, failureMessage, (Function<Object, Result>) fn);
    }

    /**
     * Create an instance of a {@link ValidationRule} which should result in a warning should the evaluate of this rule fail.
     *
     * @param description A description to help differentiate this rule from others (not intended to be user-facing).
     * @param failureMessage The message to be displayed in the event of a test failure (intended to be user-facing).
     * @param fn The test condition to be applied as a part of this rule, when this function returns <code>true</code>,
     *             the evaluated instance will be considered "valid" according to this rule.
     * @param <T> The type of the object being evaluated.
     *
     * @return A new instance of a {@link ValidationRule}
     */
    @SuppressWarnings("unchecked")
    public static <T> ValidationRule warn(String description, String failureMessage, Function<T, Result> fn) {
        return new ValidationRule(Severity.WARNING, description, failureMessage, (Function<Object, Result>) fn);
    }

    @Override
    public String toString() {
        return "ValidationRule{" +
                "severity=" + severity +
                ", description='" + description + '\'' +
                ", failureMessage='" + failureMessage + '\'' +
                '}';
    }

    public static abstract class Result {
        protected String details = null;
        protected Throwable throwable = null;

        public String getDetails() {
            return details;
        }

        public void setDetails(String details) {
            assert this.details == null;
            this.details = details;
        }

        public abstract boolean passed();
        public final boolean failed() { return !passed(); }

        public Throwable getThrowable() {
            return throwable;
        }

        public boolean thrown() { return this.throwable == null; }
    }

    public static final class Pass extends Result {
        public static Result empty() { return new Pass(); }

        public Pass() {
            super();
        }

        public Pass(String details) {
            this();
            this.details = details;
        }

        @Override
        public boolean passed() {
            return true;
        }
    }

    public static final class Fail extends Result {
        public static Result empty() { return new Fail(); }

        public Fail() {
            super();
        }

        public Fail(String details) {
            this();
            this.details = details;
        }

        public Fail(String details, Throwable throwable) {
            this();
            this.throwable = throwable;
            this.details = details;
        }

        @Override
        public boolean passed() {
            return false;
        }
    }
}
