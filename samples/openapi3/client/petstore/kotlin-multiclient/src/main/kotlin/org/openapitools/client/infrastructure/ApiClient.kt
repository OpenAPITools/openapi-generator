package org.openapitools.client.infrastructure

import okhttp3.OkHttpClient
import okhttp3.RequestBody
import okhttp3.RequestBody.Companion.asRequestBody
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.FormBody
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.Request
import okhttp3.Response
import java.io.File

open class ApiClient(val baseUrl: String) {
    companion object {
        const val ContentType = "Content-Type"
        protected const val Accept = "Accept"
        protected const val Authorization = "Authorization"
        const val JsonMediaType = "application/json"
        protected const val FormDataMediaType = "multipart/form-data"
        protected const val FormUrlEncMediaType = "application/x-www-form-urlencoded"
        protected const val XmlMediaType = "application/xml"

        val apiKey: MutableMap<String, String> = mutableMapOf()
        val apiKeyPrefix: MutableMap<String, String> = mutableMapOf()
        var username: String? = null
        var password: String? = null
        var accessToken: String? = null

        @JvmStatic
        val client: OkHttpClient by lazy {
            builder.build()
        }

        @JvmStatic
        val builder: OkHttpClient.Builder = OkHttpClient.Builder()


        inline fun <reified T> parseResponse(resp: Response): T? {
            val accept = resp.header(ContentType)?.substringBefore(";")?.toLowerCase() ?: JsonMediaType
            val body = resp.body ?: return null
            val bodyContent = body.string()
            if (bodyContent.isEmpty()) {
                return null
            }
            return when(accept) {
                JsonMediaType -> Serializer.moshi.adapter(T::class.java).fromJson(bodyContent)
                else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body.")
            }
        }
    }

    protected inline fun <reified T> requestBody(content: T, mediaType: String = JsonMediaType): RequestBody =
        when {
            content is File -> content.asRequestBody(
                mediaType.toMediaTypeOrNull()
            )
            mediaType == FormDataMediaType || mediaType == FormUrlEncMediaType -> {
                FormBody.Builder().apply {
                    // content's type *must* be Map<String, Any>
                    @Suppress("UNCHECKED_CAST")
                    (content as Map<String,String>).forEach { (key, value) ->
                        add(key, value)
                    }
                }.build()
            }
            mediaType == JsonMediaType -> Serializer.moshi.adapter(T::class.java).toJson(content).toRequestBody(
                mediaType.toMediaTypeOrNull()
            )
            mediaType == XmlMediaType -> throw UnsupportedOperationException("xml not currently supported.")
            // TODO: this should be extended with other serializers
            else -> throw UnsupportedOperationException("requestBody currently only supports JSON body and File body.")
        }


    protected fun rawRequest(requestConfig: RequestConfig, body : Any? = null): Response {
        val httpUrl = baseUrl.toHttpUrlOrNull() ?: throw IllegalStateException("baseUrl is invalid.")

        val url = httpUrl.newBuilder()
                .addPathSegments(requestConfig.path.trimStart('/'))
                .apply {
                    requestConfig.query.forEach { query ->
                        query.value.forEach { queryValue ->
                            addQueryParameter(query.key, queryValue)
                        }
                    }
                }.build()

        // take content-type/accept from spec or set to default (application/json) if not defined
        if (requestConfig.headers[ContentType].isNullOrEmpty()) {
            requestConfig.headers[ContentType] = JsonMediaType
        }
        if (requestConfig.headers[Accept].isNullOrEmpty()) {
            requestConfig.headers[Accept] = JsonMediaType
        }
        val headers = requestConfig.headers

        if(headers[ContentType] ?: "" == "") {
            throw kotlin.IllegalStateException("Missing Content-Type header. This is required.")
        }

        if(headers[Accept] ?: "" == "") {
            throw kotlin.IllegalStateException("Missing Accept header. This is required.")
        }

        // TODO: support multiple contentType options here.
        val contentType = (headers[ContentType] as String).substringBefore(";").toLowerCase()

        val request = when (requestConfig.method) {
            RequestMethod.DELETE -> Request.Builder().url(url).delete()
            RequestMethod.GET -> Request.Builder().url(url).get()
            RequestMethod.HEAD -> Request.Builder().url(url).head()
            RequestMethod.PATCH -> Request.Builder().url(url).patch(requestBody(body, contentType))
            RequestMethod.PUT -> Request.Builder().url(url).put(requestBody(body, contentType))
            RequestMethod.POST -> Request.Builder().url(url).post(requestBody(body, contentType))
            RequestMethod.OPTIONS -> Request.Builder().url(url).method("OPTIONS", null)
        }.apply {
            headers.forEach { header -> addHeader(header.key, header.value) }
        }.build()

        return client.newCall(request).execute()
    }
}
