package org.openapitools.client.auth

import io.ktor.client.request.*

@Suppress("MemberVisibilityCanBePrivate")
public class HttpBearerAuth(public val scheme: String?) : Authentication {
    public var bearerToken: String? = null

    override val isConfigured: Boolean
        get() = bearerToken != null

    override fun configure(builder: HttpRequestBuilder) {
        val token = bearerToken ?: throw IllegalStateException("HttpBearerAuth not configured")
        builder.header("Authorization", "${if (scheme != null) "${upperCaseBearer(scheme)} " else ""}$token")
    }

    private fun upperCaseBearer(scheme: String): String {
        return if ("bearer".equals(scheme, ignoreCase = true)) "Bearer" else scheme
    }

    public interface Configurer {
        public fun token(value: String)
    }
}
