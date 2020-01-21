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

package org.openapitools.codegen.meta;

/**
 * Represents the "stability index" of a generator or feature, based on the stability indexes defined in the node.js ecosystem.
 */
public enum Stability {
    /**
     * The feature or features are considered complete and "production-ready".
     */
    STABLE("stable"),
    /**
     * The feature may be partially incomplete, but breaking changes will be avoided between major releases.
     */
    BETA("beta"),
    /**
     * The feature is still under active development and subject to non-backward compatible changes or removal in any
     * future version. Use of the feature is not recommended in production environments.
     */
    EXPERIMENTAL("experimental"),
    /**
     * The feature may emit warnings. Backward compatibility is not guaranteed. Removal is likely to occur in a subsequent major release.
     */
    DEPRECATED("deprecated");

    private String description;

    Stability(String description) {
        this.description = description;
    }

    /**
     * Returns a value for this stability index.
     *
     * @return The descriptive value of this enum.
     */
    public String value() { return description; }

    public static Stability forDescription(String description) {
        for (Stability value: values()) {
            if (value.description.equals(description)) {
                return value;
            }
        }

        throw new IllegalArgumentException("description not found in the available values.");
    }
}
