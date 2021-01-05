package org.openapitools.client.auth

import io.ktor.client.request.*
import io.ktor.http.*
import org.openapitools.client.infrastructure.toBase64

public class HttpBasicAuth : Authentication {
    public var username: String? = null
    public var password: String? = null

    override val isConfigured: Boolean
        get() = username != null || password != null

    override fun configure(builder: HttpRequestBuilder) {
        if (username == null && password == null) {
            throw IllegalStateException("HttpBasicAuth not configured")
        }
        val str = "${username ?: ""}:${password ?: ""}"
        val auth = str.toBase64()
        builder.header(HttpHeaders.Authorization, "Basic $auth")
    }
}
