package org.openapitools.codegen;

/**
 * Internal capability implemented by generator families that support isolated forced-schema
 * shadow generation.
 */
public interface ForcedSchemaSupport {

    /**
     * Clears every cache whose values depend on schema or import mappings.
     */
    void clearModelNameCache();
}
