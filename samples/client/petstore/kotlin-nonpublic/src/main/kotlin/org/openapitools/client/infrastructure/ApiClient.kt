package org.openapitools.client.infrastructure

import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.RequestBody
import okhttp3.RequestBody.Companion.asRequestBody
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.FormBody
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.ResponseBody
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.Request
import java.io.File

internal open class ApiClient(val baseUrl: String) {
    internal companion object {
        protected const val ContentType = "Content-Type"
        protected const val Accept = "Accept"
        protected const val Authorization = "Authorization"
        protected const val JsonMediaType = "application/json"
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

    protected inline fun <reified T: Any?> responseBody(body: ResponseBody?, mediaType: String? = JsonMediaType): T? {
        if(body == null) {
            return null
        }
        val bodyContent = body.string()
        if (bodyContent.isEmpty()) {
            return null
        }
        return when(mediaType) {
            JsonMediaType -> Serializer.moshi.adapter(T::class.java).fromJson(bodyContent)
            else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body.")
        }
    }

    protected fun updateAuthParams(requestConfig: RequestConfig) {
        if (requestConfig.headers["api_key"].isNullOrEmpty()) {
            if (apiKey["api_key"] != null) {
                if (apiKeyPrefix["api_key"] != null) {
                    requestConfig.headers["api_key"] = apiKeyPrefix["api_key"]!! + " " + apiKey["api_key"]!!
                } else {
                    requestConfig.headers["api_key"] = apiKey["api_key"]!!
                }
            }
        }
        if (requestConfig.headers[Authorization].isNullOrEmpty()) {
            accessToken?.let { accessToken ->
                requestConfig.headers[Authorization] = "Bearer " + accessToken
            }
        }
    }

    protected inline fun <reified T: Any?> request(requestConfig: RequestConfig, body : Any? = null): ApiInfrastructureResponse<T?> {
        val httpUrl = baseUrl.toHttpUrlOrNull() ?: throw IllegalStateException("baseUrl is invalid.")

        // take authMethod from operation
        updateAuthParams(requestConfig)

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
            RequestMethod.DELETE -> Request.Builder().url(url).delete(requestBody(body, contentType))
            RequestMethod.GET -> Request.Builder().url(url)
            RequestMethod.HEAD -> Request.Builder().url(url).head()
            RequestMethod.PATCH -> Request.Builder().url(url).patch(requestBody(body, contentType))
            RequestMethod.PUT -> Request.Builder().url(url).put(requestBody(body, contentType))
            RequestMethod.POST -> Request.Builder().url(url).post(requestBody(body, contentType))
            RequestMethod.OPTIONS -> Request.Builder().url(url).method("OPTIONS", null)
        }.apply {
            headers.forEach { header -> addHeader(header.key, header.value) }
        }.build()

        val response = client.newCall(request).execute()
        val accept = response.header(ContentType)?.substringBefore(";")?.toLowerCase()

        // TODO: handle specific mapping types. e.g. Map<int, Class<?>>
        when {
            response.isRedirect -> return Redirection(
                    response.code,
                    response.headers.toMultimap()
            )
            response.isInformational -> return Informational(
                    response.message,
                    response.code,
                    response.headers.toMultimap()
            )
            response.isSuccessful -> return Success(
                    responseBody(response.body, accept),
                    response.code,
                    response.headers.toMultimap()
            )
            response.isClientError -> return ClientError(
                    response.body?.string(),
                    response.code,
                    response.headers.toMultimap()
            )
            else -> return ServerError(
                    null,
                    response.body?.string(),
                    response.code,
                    response.headers.toMultimap()
            )
        }
    }

    protected inline fun <reified T: Any> parseDateToQueryString(value : T): String {
        /*
        .replace("\"", "") converts the json object string to an actual string for the query parameter.
        The moshi or gson adapter allows a more generic solution instead of trying to use a native
        formatter. It also easily allows to provide a simple way to define a custom date format pattern
        inside a gson/moshi adapter.
        */
        return Serializer.moshi.adapter(T::class.java).toJson(value).replace("\"", "")
    }
}
