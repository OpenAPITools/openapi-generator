/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * Shared resolution/validation logic for the {@code x-jackson-json-include-policy} vendor extension and the
 * {@code optionalNonNullPropertyJsonInclude} config option, used by both the {@code spring} and
 * {@code kotlin-spring} generators (issue #24401).
 */
public final class JsonIncludePolicyUtils {

    /** Sentinel value meaning "emit no {@code @JsonInclude} annotation". */
    public static final String NONE = "NONE";

    /**
     * Valid {@code com.fasterxml.jackson.annotation.JsonInclude.Include} constant names accepted for a
     * manual per-property {@code x-jackson-json-include-policy} override. {@code NONE} is a sentinel
     * meaning "emit no annotation" and is handled separately (the extension is dropped).
     */
    private static final Set<String> VALID_JSON_INCLUDE_CONSTANTS = new HashSet<>(Arrays.asList(
            "ALWAYS", "NON_NULL", "NON_ABSENT", "NON_EMPTY", "NON_DEFAULT", "USE_DEFAULTS", "CUSTOM"));

    /** Valid values for the {@code optionalNonNullPropertyJsonInclude} config option. */
    private static final Set<String> VALID_OPTIONAL_NON_NULL_POLICIES = new HashSet<>(Arrays.asList(
            "NON_NULL", "NON_EMPTY", "NON_DEFAULT", "NONE"));

    private JsonIncludePolicyUtils() {
    }

    /**
     * Whether the given policy value (a manual override, or an already-resolved automatic policy) should
     * result in an emitted {@code @JsonInclude} annotation. {@code null}, blank/whitespace-only, and the
     * {@code NONE} sentinel (after trimming, case-insensitive) all mean "emit nothing".
     */
    public static boolean isJsonIncludePolicyEmitted(Object policy) {
        if (policy == null) {
            return false;
        }
        String trimmed = policy.toString().trim();
        return !trimmed.isEmpty() && !NONE.equalsIgnoreCase(trimmed);
    }

    /**
     * Validate and normalize a manual per-property {@code x-jackson-json-include-policy} override.
     *
     * @param rawPolicy       the raw vendor extension value set directly on the property in the spec
     * @param extensionName   the vendor extension key, used in the error message (kept generator-agnostic)
     * @return the normalized (upper-case) policy to emit, or {@code null} when the override means
     * "emit no annotation" ({@code NONE}/blank), in which case the caller must drop the extension.
     * @throws IllegalArgumentException when the override is not a valid {@code JsonInclude.Include} value.
     */
    public static String resolveManualJsonIncludePolicy(Object rawPolicy, String extensionName) {
        if (!isJsonIncludePolicyEmitted(rawPolicy)) {
            return null;
        }
        String normalized = rawPolicy.toString().trim().toUpperCase(Locale.ROOT);
        if (!VALID_JSON_INCLUDE_CONSTANTS.contains(normalized)) {
            throw new IllegalArgumentException(extensionName
                    + " must be a valid com.fasterxml.jackson.annotation.JsonInclude.Include value "
                    + "(ALWAYS, NON_NULL, NON_ABSENT, NON_EMPTY, NON_DEFAULT, USE_DEFAULTS, CUSTOM), or NONE to emit "
                    + "no annotation, but was: " + rawPolicy);
        }
        return normalized;
    }

    /**
     * Validate and normalize the {@code optionalNonNullPropertyJsonInclude} config option value.
     *
     * @param policy      the raw config option value (may be {@code null}, in which case the default
     *                    {@code NON_NULL} is returned)
     * @param optionName  the config option name, used in the error message (kept generator-agnostic)
     * @return the normalized (upper-case) policy: one of {@code NON_NULL}, {@code NON_EMPTY},
     * {@code NON_DEFAULT}, {@code NONE}.
     * @throws IllegalArgumentException when the value is not one of the supported policies.
     */
    public static String normalizeJsonIncludePolicy(String policy, String optionName) {
        if (policy == null) {
            return "NON_NULL";
        }
        String normalized = policy.trim().toUpperCase(Locale.ROOT);
        if (!VALID_OPTIONAL_NON_NULL_POLICIES.contains(normalized)) {
            throw new IllegalArgumentException(optionName
                    + " must be one of NON_NULL, NON_EMPTY, NON_DEFAULT, NONE but was: " + policy);
        }
        return normalized;
    }
}
