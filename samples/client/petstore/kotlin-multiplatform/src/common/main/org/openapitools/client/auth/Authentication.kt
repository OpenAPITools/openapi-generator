package org.openapitools.client.auth

import org.openapitools.client.infrastructure.Queries

interface Authentication {
    /**
     * Apply authentication settings to header and query params.
     *
     * @param queries Query parameters.
     * @param headers Header parameters.
     */
    fun apply(queries: Queries, headers: MutableMap<String, String?>)
}
