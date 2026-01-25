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
import okhttp3.Headers.Builder
import okhttp3.Headers.Companion.toHeaders
import okhttp3.MultipartBody
import okhttp3.Call
import okhttp3.Callback
import okhttp3.Response
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
import java.util.regex.Pattern
import com.squareup.moshi.adapter

val EMPTY_REQUEST: RequestBody = ByteArray(0).toRequestBody()

open class ApiClient(val baseUrl: String, val client: Call.Factory = defaultClient) {
    companion object {
        protected const val ContentType: String = "Content-Type"
        protected const val Accept: String = "Accept"
        protected const val Authorization: String = "Authorization"
        protected const val JsonMediaType: String = "application/json"
        protected const val FormDataMediaType: String = "multipart/form-data"
        protected const val FormUrlEncMediaType: String = "application/x-www-form-urlencoded"
        protected const val XmlMediaType: String = "application/xml"
        protected const val OctetMediaType: String = "application/octet-stream"
        protected const val TextMediaType: String = "text/plain"

        val apiKey: MutableMap<String, String> = mutableMapOf()
        val apiKeyPrefix: MutableMap<String, String> = mutableMapOf()
        var username: String? = null
        var password: String? = null
        var accessToken: String? = null
        const val baseUrlKey: String = "org.openapitools.client.baseUrl"

        @JvmStatic
        val defaultClient: OkHttpClient by lazy {
            builder.build()
        }

        @JvmStatic
        val builder: OkHttpClient.Builder = OkHttpClient.Builder()
    }

    /**
     * Guess Content-Type header from the given byteArray (defaults to "application/octet-stream").
     *
     * @param byteArray The given file
     * @return The guessed Content-Type
     */
    protected fun guessContentTypeFromByteArray(byteArray: ByteArray): String {
        val contentType = try {
            URLConnection.guessContentTypeFromStream(byteArray.inputStream())
        } catch (io: IOException) {
            "application/octet-stream"
        }
        return contentType
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

    /**
     * Adds a File to a MultipartBody.Builder
     * Defined a helper in the requestBody method to not duplicate code
     * It will be used when the content is a FormDataMediaType and the body of the PartConfig is a File
     *
     * @param name The field name to add in the request
     * @param headers The headers that are in the PartConfig
     * @param file The file that will be added as the field value
     * @return The method returns Unit but the new Part is added to the Builder that the extension function is applying on
     * @see requestBody
     */
    protected fun MultipartBody.Builder.addPartToMultiPart(name: String, headers: Map<String, String>, file: File) {
        val partHeaders = headers.toMutableMap() +
            ("Content-Disposition" to "form-data; name=\"$name\"; filename=\"${file.name}\"")
        val fileMediaType = guessContentTypeFromFile(file).toMediaTypeOrNull()
        addPart(
            partHeaders.toHeaders(),
            file.asRequestBody(fileMediaType)
        )
    }

    /**
     * Adds any type to a MultipartBody.Builder
     * Defined a helper in the requestBody method to not duplicate code
     * It will be used when the content is a FormDataMediaType and the body of the PartConfig is not a File.
     *
     * @param name The field name to add in the request
     * @param headers The headers that are in the PartConfig
     * @param obj The field name to add in the request
     * @return The method returns Unit but the new Part is added to the Builder that the extension function is applying on
     * @see requestBody
     */
    protected fun <T> MultipartBody.Builder.addPartToMultiPart(name: String, headers: Map<String, String>, obj: T?) {
        val partHeaders = headers.toMutableMap() +
            ("Content-Disposition" to "form-data; name=\"$name\"")
        addPart(
            partHeaders.toHeaders(),
            parameterToString(obj).toRequestBody(null)
        )
    }

    protected inline fun <reified T> requestBody(content: T, mediaType: String?): RequestBody =
        when {
            content is ByteArray -> content.toRequestBody((mediaType ?: guessContentTypeFromByteArray(content)).toMediaTypeOrNull())
            content is File -> content.asRequestBody((mediaType ?: guessContentTypeFromFile(content)).toMediaTypeOrNull())
            mediaType == FormDataMediaType ->
                MultipartBody.Builder()
                    .setType(MultipartBody.FORM)
                    .apply {
                        // content's type *must* be Map<String, PartConfig<*>>
                        @Suppress("UNCHECKED_CAST")
                        (content as Map<String, PartConfig<*>>).forEach { (name, part) ->
                            when (part.body) {
                                is File -> addPartToMultiPart(name, part.headers, part.body)
                                is List<*> -> {
                                    part.body.forEach {
                                        if (it is File) {
                                            addPartToMultiPart(name, part.headers, it)
                                        } else {
                                            addPartToMultiPart(name, part.headers, it)
                                        }
                                    }
                               }
                               else -> addPartToMultiPart(name, part.headers, part.body)
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
            mediaType == TextMediaType && content is String ->
                content.toRequestBody(TextMediaType.toMediaTypeOrNull())
            // TODO: this should be extended with other serializers
            else -> throw UnsupportedOperationException("requestBody currently only supports JSON body, text body, byte body and File body.")
        }

    @OptIn(ExperimentalStdlibApi::class)
    protected inline fun <reified T: Any?> responseBody(response: Response, mediaType: String? = JsonMediaType): T? {
        val body = response.body
        if(body == null) {
            return null
        } else if (T::class.java == Unit::class.java) {
            // No need to parse the body when we're not interested in the body
            // Useful when API is returning other Content-Type
            return null
        } else if (T::class.java == File::class.java) {
            // return tempFile
            val contentDisposition = response.header("Content-Disposition")

            val fileName = if (contentDisposition != null) {
                // Get filename from the Content-Disposition header.
                val pattern = Pattern.compile("filename=['\"]?([^'\"\\s]+)['\"]?")
                val matcher = pattern.matcher(contentDisposition)
                if (matcher.find()) {
                    matcher.group(1)
                        ?.replace(".*[/\\\\]", "")
                        ?.replace(";", "")
                } else {
                    null
                }
            } else {
                null
            }

            var prefix: String?
            val suffix: String?
            if (fileName == null) {
                prefix = "download"
                suffix = ""
            } else {
                val pos = fileName.lastIndexOf(".")
                if (pos == -1) {
                    prefix = fileName
                    suffix = null
                } else {
                    prefix = fileName.substring(0, pos)
                    suffix = fileName.substring(pos)
                }
                // Files.createTempFile requires the prefix to be at least three characters long
                if (prefix.length < 3) {
                    prefix = "download"
                }
            }

            // Attention: if you are developing an android app that supports API Level 25 and below, please check flag supportAndroidApiLevel25AndBelow in https://openapi-generator.tech/docs/generators/kotlin#config-options
            val tempFile = java.nio.file.Files.createTempFile(prefix, suffix).toFile()
            tempFile.deleteOnExit()
            body.byteStream().use { inputStream ->
                tempFile.outputStream().use { tempFileOutputStream ->
                    inputStream.copyTo(tempFileOutputStream)
                }
            }
            return tempFile as T
        }

        return when {
            mediaType == null || (mediaType.startsWith("application/") && mediaType.endsWith("json")) -> {
                val bodyContent = body.string()
                if (bodyContent.isEmpty()) {
                    return null
                }
                Serializer.moshi.adapter<T>().fromJson(bodyContent)
            }
            mediaType == OctetMediaType -> body.bytes() as? T
            mediaType == TextMediaType -> body.string() as? T
            else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body, text body and byte body.")
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
            (headers[ContentType] as String).substringBefore(";").lowercase(Locale.US)
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
            val headersBuilder = Headers.Builder()
            headers.forEach { header ->
                headersBuilder.add(header.key, header.value)
            }
            this.headers(headersBuilder.build())
        }.build()

        val response = client.newCall(request).execute()

        val accept = response.header(ContentType)?.substringBefore(";")?.lowercase(Locale.US)

        // TODO: handle specific mapping types. e.g. Map<int, Class<?>>
        @Suppress("UNNECESSARY_SAFE_CALL")
        return response.use {
            when {
                it.isRedirect -> Redirection(
                    it.code,
                    it.headers.toMultimap()
                )
                it.isInformational -> Informational(
                    it.message,
                    it.code,
                    it.headers.toMultimap()
                )
                it.isSuccessful -> Success(
                    responseBody(it, accept),
                    it.code,
                    it.headers.toMultimap()
                )
                it.isClientError -> ClientError(
                    it.message,
                    it.body?.string(),
                    it.code,
                    it.headers.toMultimap()
                )
                else -> ServerError(
                    it.message,
                    it.body?.string(),
                    it.code,
                    it.headers.toMultimap()
                )
            }
        }
    }

    protected fun parameterToString(value: Any?): String = when (value) {
        null -> ""
        is Array<*> -> toMultiValue(value, "csv").toString()
        is Iterable<*> -> toMultiValue(value, "csv").toString()
        is OffsetDateTime -> parseDateToQueryString(value)
        is OffsetTime -> parseDateToQueryString(value)
        is LocalDateTime -> parseDateToQueryString(value)
        is LocalDate -> parseDateToQueryString(value)
        is LocalTime -> parseDateToQueryString(value)
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
