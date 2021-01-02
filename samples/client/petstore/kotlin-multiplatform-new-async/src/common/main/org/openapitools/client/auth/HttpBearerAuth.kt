package org.openapitools.client.auth

import org.openapitools.client.infrastructure.Queries

@Suppress("MemberVisibilityCanBePrivate")
public class HttpBearerAuth(public val scheme: String?) : Authentication {
    public var bearerToken: String? = null

    override fun apply(queries: Queries, headers: MutableMap<String, String?>) {
        val token: String = bearerToken ?: return
        headers["Authorization"] = (if (scheme != null) upperCaseBearer(scheme)!! + " " else "") + token
    }

    private fun upperCaseBearer(scheme: String): String? {
        return if ("bearer".equals(scheme, ignoreCase = true)) "Bearer" else scheme
    }
}
