package org.openapitools.client.auth

import io.ktor.client.request.*

public interface Authentication {
    /**
     * Apply authentication settings to header and query params.
     */
    public fun configure(builder: HttpRequestBuilder)

    /**
     * Signals if the [Authentication] is configured and can be used to configure the HTTP request
     */
    public val isConfigured: Boolean
}
