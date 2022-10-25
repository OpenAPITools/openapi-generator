package org.openapitools.client.infrastructure

import okhttp3.OkHttpClient
import okhttp3.RequestBody
import okhttp3.RequestBody.Companion.asRequestBody
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.FormBody
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.ResponseBody
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.Request
import okhttp3.Headers
import okhttp3.Headers.Companion.toHeaders
import okhttp3.MultipartBody
import okhttp3.Call
import okhttp3.Callback
import okhttp3.Response
import okhttp3.internal.EMPTY_REQUEST
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.net.URLConnection
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.util.Locale
import com.squareup.moshi.adapter

open class ApiClient(val baseUrl: String, val client: OkHttpClient = defaultClient) {
    companion object {
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
        const val baseUrlKey = "org.openapitools.client.baseUrl"

        @JvmStatic
        val defaultClient: OkHttpClient by lazy {
            builder.build()
        }

        @JvmStatic
        val builder: OkHttpClient.Builder = OkHttpClient.Builder()
    }

    /**
     * Guess Content-Type header from the given file (defaults to "application/octet-stream").
     *
     * @param file The given file
     * @return The guessed Content-Type
     */
    protected fun guessContentTypeFromFile(file: File): String {
        val contentType = URLConnection.guessContentTypeFromName(file.name)
        return contentType ?: "application/octet-stream"
    }

    protected inline fun <reified T> requestBody(content: T, mediaType: String?): RequestBody =
        when {
            content is File -> content.asRequestBody((mediaType ?: guessContentTypeFromFile(content)).toMediaTypeOrNull())
            mediaType == FormDataMediaType ->
                MultipartBody.Builder()
                    .setType(MultipartBody.FORM)
                    .apply {
                        // content's type *must* be Map<String, PartConfig<*>>
                        @Suppress("UNCHECKED_CAST")
                        (content as Map<String, PartConfig<*>>).forEach { (name, part) ->
                            if (part.body is File) {
                                val partHeaders = part.headers.toMutableMap() +
                                    ("Content-Disposition" to "form-data; name=\"$name\"; filename=\"${part.body.name}\"")
                                val fileMediaType = guessContentTypeFromFile(part.body).toMediaTypeOrNull()
                                addPart(
                                    partHeaders.toHeaders(),
                                    part.body.asRequestBody(fileMediaType)
                                )
                            } else {
                                val partHeaders = part.headers.toMutableMap() +
                                    ("Content-Disposition" to "form-data; name=\"$name\"")
                                addPart(
                                    partHeaders.toHeaders(),
                                    parameterToString(part.body).toRequestBody(null)
                                )
                            }
                        }
                    }.build()
            mediaType == FormUrlEncMediaType -> {
                FormBody.Builder().apply {
                    // content's type *must* be Map<String, PartConfig<*>>
                    @Suppress("UNCHECKED_CAST")
                    (content as Map<String, PartConfig<*>>).forEach { (name, part) ->
                        add(name, parameterToString(part.body))
                    }
                }.build()
            }
            mediaType == null || mediaType.startsWith("application/") && mediaType.endsWith("json") ->
                if (content == null) {
                    EMPTY_REQUEST
                } else {
                    Serializer.moshi.adapter(T::class.java).toJson(content)
                        .toRequestBody((mediaType ?: JsonMediaType).toMediaTypeOrNull())
                }
            mediaType == XmlMediaType -> throw UnsupportedOperationException("xml not currently supported.")
            // TODO: this should be extended with other serializers
            else -> throw UnsupportedOperationException("requestBody currently only supports JSON body and File body.")
        }

    @OptIn(ExperimentalStdlibApi::class)
    protected inline fun <reified T: Any?> responseBody(body: ResponseBody?, mediaType: String? = JsonMediaType): T? {
        if(body == null) {
            return null
        }
        if (T::class.java == File::class.java) {
            // return tempFile
            // Attention: if you are developing an android app that supports API Level 25 and bellow, please check flag supportAndroidApiLevel25AndBelow in https://openapi-generator.tech/docs/generators/kotlin#config-options
            val tempFile = java.nio.file.Files.createTempFile("tmp.org.openapitools.client", null).toFile()
            tempFile.deleteOnExit()
            body.byteStream().use { inputStream ->
                tempFile.outputStream().use { tempFileOutputStream ->
                    inputStream.copyTo(tempFileOutputStream)
                }
            }
            return tempFile as T
        }
        val bodyContent = body.string()
        if (bodyContent.isEmpty()) {
            return null
        }
        return when {
            mediaType==null || (mediaType.startsWith("application/") && mediaType.endsWith("json")) ->
                Serializer.moshi.adapter<T>().fromJson(bodyContent)
            else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body.")
        }
    }


    protected inline fun <reified I, reified T: Any?> request(requestConfig: RequestConfig<I>): ApiResponse<T?> {
        val httpUrl = baseUrl.toHttpUrlOrNull() ?: throw IllegalStateException("baseUrl is invalid.")

        val url = httpUrl.newBuilder()
            .addEncodedPathSegments(requestConfig.path.trimStart('/'))
            .apply {
                requestConfig.query.forEach { query ->
                    query.value.forEach { queryValue ->
                        addQueryParameter(query.key, queryValue)
                    }
                }
            }.build()

        // take content-type/accept from spec or set to default (application/json) if not defined
        if (requestConfig.body != null && requestConfig.headers[ContentType].isNullOrEmpty()) {
            requestConfig.headers[ContentType] = JsonMediaType
        }
        if (requestConfig.headers[Accept].isNullOrEmpty()) {
            requestConfig.headers[Accept] = JsonMediaType
        }
        val headers = requestConfig.headers

        if (headers[Accept].isNullOrEmpty()) {
            throw kotlin.IllegalStateException("Missing Accept header. This is required.")
        }

        val contentType = if (headers[ContentType] != null) {
            // TODO: support multiple contentType options here.
            (headers[ContentType] as String).substringBefore(";").lowercase(Locale.getDefault())
        } else {
            null
        }

        val request = when (requestConfig.method) {
            RequestMethod.DELETE -> Request.Builder().url(url).delete(requestBody(requestConfig.body, contentType))
            RequestMethod.GET -> Request.Builder().url(url)
            RequestMethod.HEAD -> Request.Builder().url(url).head()
            RequestMethod.PATCH -> Request.Builder().url(url).patch(requestBody(requestConfig.body, contentType))
            RequestMethod.PUT -> Request.Builder().url(url).put(requestBody(requestConfig.body, contentType))
            RequestMethod.POST -> Request.Builder().url(url).post(requestBody(requestConfig.body, contentType))
            RequestMethod.OPTIONS -> Request.Builder().url(url).method("OPTIONS", null)
        }.apply {
            headers.forEach { header -> addHeader(header.key, header.value) }
        }.build()

        val response = client.newCall(request).execute()

        val accept = response.header(ContentType)?.substringBefore(";")?.lowercase(Locale.getDefault())

        // TODO: handle specific mapping types. e.g. Map<int, Class<?>>
        return when {
            response.isRedirect -> Redirection(
                response.code,
                response.headers.toMultimap()
            )
            response.isInformational -> Informational(
                response.message,
                response.code,
                response.headers.toMultimap()
            )
            response.isSuccessful -> Success(
                responseBody(response.body, accept),
                response.code,
                response.headers.toMultimap()
            )
            response.isClientError -> ClientError(
                response.message,
                response.body?.string(),
                response.code,
                response.headers.toMultimap()
            )
            else -> ServerError(
                response.message,
                response.body?.string(),
                response.code,
                response.headers.toMultimap()
            )
        }
    }

    protected fun parameterToString(value: Any?): String = when (value) {
        null -> ""
        is Array<*> -> toMultiValue(value, "csv").toString()
        is Iterable<*> -> toMultiValue(value, "csv").toString()
        is OffsetDateTime, is OffsetTime, is LocalDateTime, is LocalDate, is LocalTime ->
            parseDateToQueryString(value)
        else -> value.toString()
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
