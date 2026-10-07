package org.openapitools.client.auth

import kotlin.io.encoding.Base64

class HttpBasicAuth : Authentication {
    var username: String? = null
    var password: String? = null

    override fun apply(query: MutableMap<String, List<String>>, headers: MutableMap<String, String>) {
        if (username == null && password == null) return
        val str = (username ?: "") + ":" + (password ?: "")
        val auth = Base64.encode(str.encodeToByteArray())
        headers["Authorization"] = "Basic $auth"
    }
}
