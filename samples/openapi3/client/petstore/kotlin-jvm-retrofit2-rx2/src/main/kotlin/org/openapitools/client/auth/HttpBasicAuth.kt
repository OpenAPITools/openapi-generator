package org.openapitools.client.auth

import java.io.IOException

import kotlin.jvm.Throws
import okhttp3.Interceptor
import okhttp3.Interceptor.Chain
import okhttp3.Response
import okhttp3.Credentials

class HttpBasicAuth(
    private var username: String = "",
    private var password: String = ""
) : Interceptor {

    fun setCredentials(username: String, password: String) {
        this.username = username
        this.password = password
    }

    @Throws(IOException::class)
    override fun intercept(chain: Chain): Response {
        var request = chain.request()

        // If the request already have an authorization (eg. Basic auth), do nothing
        if (request.header("Authorization") == null && username.isNotBlank() && password.isNotBlank()) {
            request = request.newBuilder()
                .addHeader("Authorization", Credentials.basic(username, password))
                .build()
        }
        return chain.proceed(request)
    }
}
