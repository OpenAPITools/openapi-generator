package org.openapitools.client.infrastructure

import io.vertx.core.Vertx
import io.vertx.core.buffer.Buffer
import java.nio.charset.StandardCharsets
import com.google.gson.reflect.TypeToken

open class ApiClient(val basePath: kotlin.String = defaultBasePath, val accessToken: String? = null, val apiKey: MutableMap<String, String> = mutableMapOf(), val apiKeyPrefix: MutableMap<String, String> = mutableMapOf(), var username: String? = null, var password: String? = null, val vertx: Vertx) {
    companion object {
        const val baseUrlKey = "org.openapitools.client.baseUrl"

        @JvmStatic
        val defaultBasePath: String by lazy {
            System.getProperties().getProperty(baseUrlKey, "http://petstore.swagger.io/v2")
        }
    }

    protected inline fun <reified T: Any?> handleResponse(response: io.vertx.ext.web.client.HttpResponse<Buffer>): ApiResponse<T?> {
        val code = response.statusCode()
        val headers = response.headers().associate { it.key to listOf(it.value) }
        val contentType = headers["Content-Type"]?.firstOrNull()?.substringBefore(";")?.lowercase(java.util.Locale.getDefault())

        return when (code) {
            in 100..199 -> Informational(
                response.statusMessage(),
                code,
                headers
            )
            in 200 .. 299 -> Success(
                responseBody<T>(response.body(), contentType),
                code,
                headers
            )
            in 300..399 -> Redirection(
                code,
                headers
            )
            in 400..499 -> ClientError(
                response.statusMessage(),
                response.bodyAsString(),
                code,
                headers
            )
            else -> ServerError(
                response.statusMessage(),
                response.bodyAsString(),
                code,
                headers
            )
        }
    }

    protected inline fun <reified T: Any?> responseBody(body: Buffer?, mediaType: String? = "application/json"): T? {
        body ?: return null

        val bodyContent = String(body.bytes, StandardCharsets.UTF_8)
        if (bodyContent.isEmpty()) {
            return null
        }

        return when {
            mediaType==null || (mediaType.startsWith("application/") && mediaType.endsWith("json")) ->
                Serializer.gson.fromJson(bodyContent, (object: TypeToken<T>(){}).getType())
            else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body.")
        }
    }

    protected fun encodeURIComponent(parameter: String): String {
        return try {
            java.net.URLEncoder.encode(parameter, java.nio.charset.StandardCharsets.UTF_8.name())
        } catch (e: java.io.UnsupportedEncodingException) {
            parameter
        }
    }

    protected inline fun <reified T: Any> parseDateToQueryString(value : T): String {
        /*
        .replace("\"", "") converts the json object string to an actual string for the query parameter.
        The moshi or gson adapter allows a more generic solution instead of trying to use a native
        formatter. It also easily allows to provide a simple way to define a custom date format pattern
        inside a gson/moshi adapter.
        */
        return Serializer.gson.toJson(value, T::class.java).replace("\"", "")
    }

}
