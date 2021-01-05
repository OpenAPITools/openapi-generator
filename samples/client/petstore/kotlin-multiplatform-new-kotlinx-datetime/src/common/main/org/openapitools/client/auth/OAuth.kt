package org.openapitools.client.auth

import io.ktor.client.request.*
import io.ktor.http.*

public class OAuth : Authentication {
    @Suppress("MemberVisibilityCanBePrivate")
    public var accessToken: String? = null

    public override val isConfigured: Boolean
        get() = accessToken != null

    override fun configure(builder: HttpRequestBuilder) {
        val token = accessToken ?: throw IllegalStateException("OAuth not configured")
        builder.header(HttpHeaders.Authorization, "Bearer $token")
    }

    public interface Configurer {
        public fun token(value: String)
    }
}
