package org.openapitools.codegen.utils;

/**
 * A tri-state toggle for boolean-like config options that need to distinguish "the user never set
 * this" from an explicit {@code true}/{@code false}, e.g. so a weak default can be applied along
 * with a one-time warning when the option was left unset, while an explicit {@code false} silently
 * keeps the same default behavior.
 *
 * <p>This replaces the previous convention of using a boxed {@code Boolean} field where
 * {@code null} meant "unset", which requires a code comment at every declaration site to be
 * understandable.
 */
public enum TriStateBoolean {
    /** The option was never explicitly set by the user (neither {@code true} nor {@code false}). */
    UNSET,
    /** The option was explicitly set to {@code false}. */
    FALSE,
    /** The option was explicitly set to {@code true}. */
    TRUE;

    /** Whether this state represents an explicit, affirmative opt-in ({@link #TRUE}). */
    public boolean isTrue() {
        return this == TRUE;
    }

    /** Whether the option was never explicitly set ({@link #UNSET}). */
    public boolean isUnset() {
        return this == UNSET;
    }

    /**
     * Convert a nullable {@link Boolean} (the CLI/config-option convention where {@code null} means
     * "not present in additionalProperties") into a {@link TriStateBoolean}.
     *
     * @param value {@code null} for unset, {@code Boolean.TRUE}/{@code Boolean.FALSE} for an explicit value
     * @return {@link #UNSET}, {@link #TRUE}, or {@link #FALSE}
     */
    public static TriStateBoolean fromNullableBoolean(Boolean value) {
        if (value == null) {
            return UNSET;
        }
        return value ? TRUE : FALSE;
    }
}
