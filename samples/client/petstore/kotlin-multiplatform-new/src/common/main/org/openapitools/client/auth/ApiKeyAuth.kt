package org.openapitools.client.auth

import org.openapitools.client.infrastructure.Queries

@Suppress("MemberVisibilityCanBePrivate")
public class ApiKeyAuth(public val location: String, public val paramName: String) : Authentication {
    public var apiKey: String? = null
    public var apiKeyPrefix: String? = null

    override fun apply(queries: Queries, headers: MutableMap<String, String?>) {
            val key: String = apiKey ?: return
            val prefix: String? = apiKeyPrefix
            val value: String = if (prefix != null) "$prefix $key" else key
            when (location) {
                "query" -> queries.add(paramName, value)
                "header" -> headers[paramName] = value
            }
        }
}
