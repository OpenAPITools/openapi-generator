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

import org.openapitools.codegen.CodegenConstants;

/**
 * Shared resolution/validation logic for the {@code x-jackson-json-include-policy} vendor extension and the
 * {@code optionalNonNullPropertyJsonInclude} config option, used by both the {@code spring} and
 * {@code kotlin-spring} generators (issue #24401).
 */
public final class JsonIncludePolicyUtils {

    /** Shared {@code CliOption} description for {@code optionalNonNullPropertyJsonInclude}. */
    public static final String OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE_DESC =
            "The Jackson @JsonInclude policy emitted for optional, non-nullable model properties when "
                    + CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS + " is true. "
                    + "NONE emits no annotation, deferring fully to the global ObjectMapper inclusion policy.";

    /** Shared {@code CliOption} description for {@code generateJsonIncludeAnnotations}. */
    public static final String GENERATE_JSON_INCLUDE_ANNOTATIONS_DESC =
            "Whether to generate policy @JsonInclude annotations on model properties. When true, emits "
                    + "spec-honest annotations (required-field protection and the optional non-nullable policy from "
                    + CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE + "). When false, none are generated and the global "
                    + "ObjectMapper owns inclusion. When left unset it defaults to false (7.23.0-equivalent output) and "
                    + "logs a warning; set it explicitly to silence the warning. A per-property override set via the "
                    + "`x-jackson-json-include-policy` vendor extension is always honored regardless of this flag.";

    private JsonIncludePolicyUtils() {
    }

    /**
     * Validate and normalize a manual per-property {@code x-jackson-json-include-policy} override.
     *
     * @param rawPolicy       the raw vendor extension value set directly on the property in the spec
     * @param extensionName   the vendor extension key, used in the error message (kept generator-agnostic)
     * @return the normalized (upper-case) policy name to emit, or {@code null} when the override means
     * "emit no annotation" ({@code NONE}/blank), in which case the caller must drop the extension.
     * @throws IllegalArgumentException when the override is not a valid {@code JsonInclude.Include} value.
     */
    public static String resolveManualJsonIncludePolicy(Object rawPolicy, String extensionName) {
        JsonIncludePolicy parsed;
        try {
            parsed = JsonIncludePolicy.parse(rawPolicy);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(extensionName
                    + " must be a valid com.fasterxml.jackson.annotation.JsonInclude.Include value "
                    + "(ALWAYS, NON_NULL, NON_ABSENT, NON_EMPTY, NON_DEFAULT, USE_DEFAULTS, CUSTOM), or NONE to emit "
                    + "no annotation, but was: " + rawPolicy);
        }
        if (parsed == null || !parsed.isEmitted()) {
            return null;
        }
        return parsed.name();
    }

    /**
     * Validate and normalize the {@code optionalNonNullPropertyJsonInclude} config option value.
     *
     * @param policy      the raw config option value (may be {@code null}, in which case the default
     *                    {@code NON_NULL} is returned)
     * @param optionName  the config option name, used in the error message (kept generator-agnostic)
     * @return the normalized (upper-case) policy name: one of {@code NON_NULL}, {@code NON_EMPTY},
     * {@code NON_DEFAULT}, {@code NONE}.
     * @throws IllegalArgumentException when the value is not one of the supported policies.
     */
    public static String normalizeJsonIncludePolicy(String policy, String optionName) {
        if (policy == null) {
            return JsonIncludePolicy.NON_NULL.name();
        }
        JsonIncludePolicy parsed;
        try {
            parsed = JsonIncludePolicy.valueOf(policy.trim().toUpperCase(java.util.Locale.ROOT));
        } catch (IllegalArgumentException e) {
            parsed = null;
        }
        if (parsed == null || !parsed.isValidOptionalNonNullPolicy()) {
            throw new IllegalArgumentException(optionName
                    + " must be one of " + JsonIncludePolicy.OPTIONAL_NON_NULL_POLICIES + " but was: " + policy);
        }
        return parsed.name();
    }
}
