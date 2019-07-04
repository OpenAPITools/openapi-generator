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
}
