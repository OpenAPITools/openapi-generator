package org.openapitools.client.auth

import org.openapitools.client.infrastructure.Queries

class ApiKeyAuth(private val location: String, val paramName: String) : Authentication {
    var apiKey: String? = null
    var apiKeyPrefix: String? = null

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
