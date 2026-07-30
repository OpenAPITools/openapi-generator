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
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.VendorExtension;
import org.slf4j.Logger;

import java.util.Map;

/**
 * Shared resolution/validation logic for the Jackson {@code @JsonInclude}/{@code @JsonSetter(nulls = ...)}
 * config options and the {@code x-jackson-json-include-policy} vendor extension, used by both the
 * {@code spring} and {@code kotlin-spring} generators (issue #24401).
 */
public final class JsonAnnotationPolicyUtils {

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

    /** Resolved {@code @JsonSetter(nulls = ...)} annotation to emit for a property, if any. */
    public enum JsonSetterNullsMode {
        /** No {@code @JsonSetter(nulls = ...)} annotation should be emitted. */
        NONE,
        /** Emit {@code @JsonSetter(nulls = Nulls.SKIP)}: silently ignore an explicit JSON null. */
        SKIP,
        /** Emit {@code @JsonSetter(nulls = Nulls.FAIL)}: reject an explicit JSON null. */
        FAIL
    }

    private JsonAnnotationPolicyUtils() {
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

    /**
     * Read back the {@code optionalNonNullPropertyJsonInclude} config option from {@code additionalProperties},
     * validating/normalizing it via {@link #normalizeJsonIncludePolicy}. Returns {@code current} unchanged when
     * the option was not present in {@code additionalProperties} (i.e. the generator's existing/default value is
     * kept). Callers are responsible for writing the resolved value back via their own
     * {@code writePropertyBack}/{@code additionalProperties.put} convention.
     *
     * @param additionalProperties the generator's additional properties map
     * @param current               the generator's current {@code optionalNonNullPropertyJsonInclude} value
     * @return the resolved {@link JsonIncludePolicy} to assign to the generator's field
     */
    public static JsonIncludePolicy resolveOptionalNonNullPropertyJsonInclude(Map<String, Object> additionalProperties, JsonIncludePolicy current) {
        if (!additionalProperties.containsKey(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE)) {
            return current;
        }
        return JsonIncludePolicy.valueOf(normalizeJsonIncludePolicy(
                additionalProperties.get(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE).toString(),
                CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE));
    }

    /**
     * Log the shared "option left unset, defaulting to false" warnings for
     * {@code generateJsonIncludeAnnotations}/{@code generateJsonSetterNullsAnnotations} on the caller's own
     * {@link Logger}. No-op for each option that is not {@link TriStateBoolean#UNSET}.
     *
     * @param logger                              the calling generator's own {@code LOGGER}
     * @param generateJsonIncludeAnnotations      the resolved {@code generateJsonIncludeAnnotations} state
     * @param generateJsonSetterNullsAnnotations  the resolved {@code generateJsonSetterNullsAnnotations} state
     */
    public static void warnIfUnset(Logger logger, TriStateBoolean generateJsonIncludeAnnotations, TriStateBoolean generateJsonSetterNullsAnnotations) {
        if (generateJsonIncludeAnnotations.isUnset()) {
            logger.warn("'{}' is not set. Defaulting to false: no @JsonInclude annotations are generated and property "
                    + "inclusion is governed entirely by the global ObjectMapper (7.23.0-equivalent output). "
                    + "Set '{}=false' to keep this behavior and silence this warning, or '{}=true' to emit spec-honest "
                    + "@JsonInclude annotations (see '{}'). Note: before 7.24.0 released output had no field-level "
                    + "@JsonInclude, so leaving this unset preserves that behavior.",
                    CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS,
                    CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS, CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE);
        }
        if (generateJsonSetterNullsAnnotations.isUnset()) {
            logger.warn("'{}' is not set. Defaulting to false: no @JsonSetter(nulls = ...) annotations are generated and "
                    + "deserialization null-handling is governed entirely by the global ObjectMapper (7.23.0-equivalent "
                    + "output). Set '{}=false' to keep this behavior and silence this warning, or '{}=true' to emit "
                    + "@JsonSetter(nulls = ...) on optional non-nullable fields.",
                    CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS, CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS,
                    CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS);
        }
    }

    /**
     * Resolve the {@code @JsonInclude} policy into the single universal
     * {@code x-jackson-json-include-policy} vendor extension the template emits, mutating {@code model.imports}
     * and {@code property.vendorExtensions} in place. Precedence:
     * <ol>
     *   <li>A value set directly on the property (manual override in the spec) always wins.</li>
     *   <li>Otherwise, when {@code generateJsonIncludeAnnotations=true}, apply the automatic matrix:
     *     <ul>
     *       <li>required &amp; non-nullable &rarr; {@code requiredNonNullablePolicy}</li>
     *       <li>required &amp; nullable &rarr; {@code requiredNullablePolicy}</li>
     *       <li>optional &amp; non-nullable &rarr; {@code optionalNonNullPropertyJsonInclude}
     *           (default {@code NON_NULL}, {@code NONE} = omit)</li>
     *       <li>optional &amp; nullable &rarr; none (JsonNullable module already governs inclusion)</li>
     *     </ul>
     *   </li>
     * </ol>
     *
     * <p>The two "required" policies are supplied by the caller since the spring and kotlin-spring generators
     * apply slightly different matrices there: kotlin-spring's non-nullable Kotlin types can never hold null,
     * so it always uses {@code ALWAYS} for required properties, whereas spring uses {@code NON_NULL} for
     * required-non-nullable (contract protection) and {@code ALWAYS} for required-nullable.
     *
     * @param model                      the model owning {@code property}, whose imports may be extended
     * @param property                   the property to resolve the policy for
     * @param generateJsonIncludeAnnotations the resolved {@code generateJsonIncludeAnnotations} state
     * @param optionalNonNullPropertyJsonInclude the resolved {@code optionalNonNullPropertyJsonInclude} policy
     * @param requiredNonNullablePolicy  the policy to apply to required, non-nullable properties
     * @param requiredNullablePolicy     the policy to apply to required, nullable properties
     */
    public static void resolveJsonIncludePolicy(CodegenModel model, CodegenProperty property,
            TriStateBoolean generateJsonIncludeAnnotations, JsonIncludePolicy optionalNonNullPropertyJsonInclude,
            JsonIncludePolicy requiredNonNullablePolicy, JsonIncludePolicy requiredNullablePolicy) {
        if (property.vendorExtensions.containsKey(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName())) {
            String manualPolicy = resolveManualJsonIncludePolicy(
                    property.vendorExtensions.get(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName()), VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName());
            if (manualPolicy != null) {
                property.vendorExtensions.put(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName(), manualPolicy);
                model.imports.add("JsonInclude");
            } else {
                // NONE / empty means "emit nothing"; drop the extension so the template renders no annotation.
                property.vendorExtensions.remove(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName());
            }
            return;
        }
        if (!generateJsonIncludeAnnotations.isTrue()) {
            return;
        }
        JsonIncludePolicy policy = null;
        if (property.required) {
            policy = property.isNullable ? requiredNullablePolicy : requiredNonNullablePolicy;
        } else if (!property.isNullable) {
            policy = optionalNonNullPropertyJsonInclude;
        }
        if (policy != null && policy.isEmitted()) {
            property.vendorExtensions.put(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY.getName(), policy.name());
            model.imports.add("JsonInclude");
        }
    }

    /**
     * Resolve which {@code @JsonSetter(nulls = ...)} annotation, if any, should be emitted for an optional,
     * non-nullable property.
     *
     * @param generateJsonSetterNullsAnnotations the resolved {@code generateJsonSetterNullsAnnotations} state
     * @param required          whether the property is required
     * @param nullable          whether the property is nullable
     * @param openApiNullable   whether the generator's {@code openApiNullable} option is enabled
     * @param failModeSupported whether this generator supports {@link JsonSetterNullsMode#FAIL} (kotlin-spring
     *                          does; spring currently only ever emits {@link JsonSetterNullsMode#SKIP})
     * @return {@link JsonSetterNullsMode#NONE} unless the property is optional and non-nullable and
     * {@code generateJsonSetterNullsAnnotations} is explicitly enabled; otherwise {@link JsonSetterNullsMode#FAIL}
     * when {@code openApiNullable && failModeSupported}, {@link JsonSetterNullsMode#SKIP} when
     * {@code !openApiNullable}, or {@link JsonSetterNullsMode#NONE} when {@code openApiNullable && !failModeSupported}.
     */
    public static JsonSetterNullsMode resolveJsonSetterNullsMode(TriStateBoolean generateJsonSetterNullsAnnotations,
            boolean required, boolean nullable, boolean openApiNullable, boolean failModeSupported) {
        if (!generateJsonSetterNullsAnnotations.isTrue() || required || nullable) {
            return JsonSetterNullsMode.NONE;
        }
        if (openApiNullable) {
            return failModeSupported ? JsonSetterNullsMode.FAIL : JsonSetterNullsMode.NONE;
        }
        return JsonSetterNullsMode.SKIP;
    }
}
