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
 * Defines different levels of severity to be used during validation.
 */
public enum Severity {
    /**
     * Lower severity indicating that the target state may be unpredictable, no longer supported, or known to have issues.
     * Marking a type with this value should not result in application exceptions under normal operating circumstances.
     */
    WARNING,
    /**
     * Higher severity indicating that the target state is not supported, or is known to cause problems with the application.
     * Marking a type with this value should result in an application exception or error exit code under normal operating circumstances.
     */
    ERROR
}