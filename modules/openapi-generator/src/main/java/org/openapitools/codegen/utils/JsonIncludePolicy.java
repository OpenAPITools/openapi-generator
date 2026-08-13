package org.openapitools.codegen.utils;

import lombok.Getter;

import java.util.EnumSet;
import java.util.Locale;
import java.util.Set;

/**
 * Mirrors {@code com.fasterxml.jackson.annotation.JsonInclude.Include} (the set of policies a
 * generated {@code @JsonInclude} annotation can be given), plus a {@link #NONE} sentinel meaning
 * "emit no {@code @JsonInclude} annotation at all", used by the {@code spring} and
 * {@code kotlin-spring} generators (issue #24401) for:
 *
 * <ul>
 *   <li>the {@code optionalNonNullPropertyJsonInclude} config option (restricted to
 *       {@link #NON_NULL}, {@link #NON_EMPTY}, {@link #NON_DEFAULT}, {@link #NONE} — see
 *       {@link #OPTIONAL_NON_NULL_POLICIES})</li>
 *   <li>the resolved {@code x-jackson-json-include-policy} vendor extension value, whether set
 *       manually on a property in the spec or computed by the automatic required/nullable matrix
 *       (any value is allowed there)</li>
 * </ul>
 */
@Getter
public enum JsonIncludePolicy {
    ALWAYS,
    NON_NULL("Omit the property when its value is null (default, spec-safe for non-nullable fields)."),
    NON_ABSENT,
    NON_EMPTY("Omit the property when its value is null or considered empty."),
    NON_DEFAULT("Omit the property when its value equals the default."),
    USE_DEFAULTS,
    CUSTOM,
    /** Sentinel meaning "emit no {@code @JsonInclude} annotation". */
    NONE("Emit no @JsonInclude annotation; defer to the global ObjectMapper.");

    /**
     * CLI-facing description, only populated for the values in {@link #OPTIONAL_NON_NULL_POLICIES}
     * (the only ones ever offered as a {@code CliOption} enum choice); {@code null} otherwise.
     */
    private final String description;

    JsonIncludePolicy() {
        this(null);
    }

    JsonIncludePolicy(String description) {
        this.description = description;
    }

    /** The subset of policies accepted for the {@code optionalNonNullPropertyJsonInclude} config option. */
    public static final Set<JsonIncludePolicy> OPTIONAL_NON_NULL_POLICIES =
            EnumSet.of(NON_NULL, NON_EMPTY, NON_DEFAULT, NONE);

    /** Whether this policy should result in an emitted {@code @JsonInclude} annotation. */
    public boolean isEmitted() {
        return this != NONE;
    }

    /** Whether this policy is a valid value for the {@code optionalNonNullPropertyJsonInclude} config option. */
    public boolean isValidOptionalNonNullPolicy() {
        return OPTIONAL_NON_NULL_POLICIES.contains(this);
    }

    /**
     * Parse a raw string (trimmed, case-insensitive) into a {@link JsonIncludePolicy}, or return
     * {@code null} if blank/whitespace-only (callers treat that the same as {@link #NONE}).
     *
     * @throws IllegalArgumentException if non-blank but not a valid {@link JsonIncludePolicy} name
     */
    public static JsonIncludePolicy parse(Object rawPolicy) {
        if (rawPolicy == null) {
            return null;
        }
        String trimmed = rawPolicy.toString().trim();
        if (trimmed.isEmpty()) {
            return null;
        }
        return JsonIncludePolicy.valueOf(trimmed.toUpperCase(Locale.ROOT));
    }
}
