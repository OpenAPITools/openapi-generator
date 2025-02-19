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
 * Defines a contract allowing some input to be validated.
 *
 * @param <TInput> The type of the input object.
 */
@FunctionalInterface
public interface Validator<TInput> {
    /**
     * Validates input, resulting in a instance of {@link ValidationResult} which provides details on all validations performed (success, error, warning).
     *
     * @param input The object instance to be validated.
     *
     * @return A {@link ValidationResult} which details the success, error, and warning validation results.
     */
    ValidationResult validate(TInput input);
}