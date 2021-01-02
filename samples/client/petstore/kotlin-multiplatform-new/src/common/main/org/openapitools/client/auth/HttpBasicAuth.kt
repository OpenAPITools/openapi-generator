package org.openapitools.client.auth

import org.openapitools.client.infrastructure.Queries
import org.openapitools.client.infrastructure.toBase64

public class HttpBasicAuth : Authentication {
    public var username: String? = null
    public var password: String? = null

    override fun apply(queries: Queries, headers: MutableMap<String, String?>) {
        if (username == null && password == null) return
        val str = (username ?: "") + ":" + (password ?: "")
        val auth = str.toBase64()
        headers["Authorization"] = "Basic $auth"
    }
}
