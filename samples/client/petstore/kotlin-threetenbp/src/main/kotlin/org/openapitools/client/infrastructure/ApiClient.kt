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
import java.util.Locale
import java.util.regex.Pattern
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.LocalTime
import org.threeten.bp.OffsetDateTime
import org.threeten.bp.OffsetTime
import com.squareup.moshi.adapter

val EMPTY_REQUEST: RequestBody = ByteArray(0).toRequestBody()

open class ApiClient(val baseUrl: String, val client: Call.Factory = defaultClient) {
    companion object {
        protected const val CONTENT_TYPE: String = "Content-Type"
        @Deprecated(
          message = "Please use the capitalized constant `CONTENT_TYPE` instead.",
          replaceWith = ReplaceWith("CONTENT_TYPE")
        )
        protected const val ContentType: String = CONTENT_TYPE

        protected const val ACCEPT: String = "Accept"
        @Deprecated(
          message = "Please use the capitalized constant `ACCEPT` instead.",
          replaceWith = ReplaceWith("ACCEPT")
        )
        protected const val Accept: String = ACCEPT

        protected const val AUTHORIZATION: String = "Authorization"
        @Deprecated(
          message = "Please use the capitalized constant `AUTHORIZATION` instead.",
          replaceWith = ReplaceWith("AUTHORIZATION")
        )
        protected const val Authorization: String = AUTHORIZATION

        protected const val JSON_MEDIA_TYPE: String = "application/json"
        @Deprecated(
          message = "Please use the capitalized constant `JSON_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("JSON_MEDIA_TYPE")
        )
        protected const val JsonMediaType: String = JSON_MEDIA_TYPE

        protected const val FORM_DATA_MEDIA_TYPE: String = "multipart/form-data"
        @Deprecated(
          message = "Please use the capitalized constant `FORM_DATA_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("FORM_DATA_MEDIA_TYPE")
        )
        protected const val FormDataMediaType: String = FORM_DATA_MEDIA_TYPE

        protected const val FORM_URL_ENC_MEDIA_TYPE: String = "application/x-www-form-urlencoded"
        @Deprecated(
          message = "Please use the capitalized constant `FORM_URL_ENC_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("FORM_URL_ENC_MEDIA_TYPE")
        )
        protected const val FormUrlEncMediaType: String = FORM_URL_ENC_MEDIA_TYPE

        protected const val XML_MEDIA_TYPE: String = "application/xml"
        @Deprecated(
          message = "Please use the capitalized constant `XML_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("XML_MEDIA_TYPE")
        )
        protected const val XmlMediaType: String = XML_MEDIA_TYPE

        protected const val OCTET_MEDIA_TYPE: String = "application/octet-stream"
        @Deprecated(
          message = "Please use the capitalized constant `OCTET_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("OCTET_MEDIA_TYPE")
        )
        protected const val OctetMediaType: String = OCTET_MEDIA_TYPE

        protected const val TEXT_MEDIA_TYPE: String = "text/plain"
        @Deprecated(
          message = "Please use the capitalized constant `TEXT_MEDIA_TYPE` instead.",
          replaceWith = ReplaceWith("TEXT_MEDIA_TYPE")
        )
        protected const val TextMediaType: String = TEXT_MEDIA_TYPE

        const val BASE_URL_KEY: String = "org.openapitools.client.baseUrl"
        @Deprecated(
          message = "Please use the capitalized constant `BASE_URL_KEY` instead.",
          replaceWith = ReplaceWith("BASE_URL_KEY")
        )
        const val baseUrlKey: String = BASE_URL_KEY

        val apiKey: MutableMap<String, String> = mutableMapOf()
        val apiKeyPrefix: MutableMap<String, String> = mutableMapOf()
        var username: String? = null
        var password: String? = null
        var accessToken: String? = null

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
        } catch (_: IOException) {
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
     * Builds headers for a multipart form-data part.
     * OkHttp requires Content-Type to be passed via the RequestBody parameter, not in headers.
     * This function filters out Content-Type and builds the appropriate Content-Disposition header.
     *
     * @param name The field name
     * @param headers The headers from the PartConfig (may include Content-Type)
     * @param filename Optional filename for file uploads
     * @return Headers object ready for addPart()
     */
    protected fun buildPartHeaders(name: String, headers: Map<String, String>, filename: String? = null): Headers {
        val disposition = if (filename != null) {
            "form-data; name=\"$name\"; filename=\"$filename\""
        } else {
            "form-data; name=\"$name\""
        }
        return (headers.filterKeys { it != "Content-Type" } + ("Content-Disposition" to disposition)).toHeaders()
    }

    /**
     * Adds a File to a MultipartBody.Builder
     * Defined a helper in the requestBody method to not duplicate code
     * It will be used when the content is a `FORM_DATA_MEDIA_TYPE` and the body of the PartConfig is a File
     *
     * @param name The field name to add in the request
     * @param headers The headers that are in the PartConfig
     * @param file The file that will be added as the field value
     * @return The method returns Unit but the new Part is added to the Builder that the extension function is applying on
     * @see requestBody
     */
    protected fun MultipartBody.Builder.addPartToMultiPart(name: String, headers: Map<String, String>, file: File) {
        val fileMediaType = guessContentTypeFromFile(file).toMediaTypeOrNull()
        addPart(
            buildPartHeaders(name, headers, file.name),
            file.asRequestBody(fileMediaType)
        )
    }

    /**
     * Serializes a multipart body part based on its content type.
     * Uses JSON serialization for application/json content types, otherwise converts to string.
     *
     * @param obj The object to serialize
     * @param contentType The Content-Type header value, if any
     * @param serializer Optional custom serializer (used for kotlinx.serialization to preserve type info)
     * @return The serialized string representation
     */
    protected fun serializePartBody(obj: Any?, contentType: String?, serializer: ((Any?) -> String)?): String {
        // Use custom serializer if provided (for kotlinx.serialization with captured type info)
        if (serializer != null) {
            return serializer(obj)
        }

        return if (contentType?.contains("json") == true) {
            Serializer.moshi.adapter(Any::class.java).toJson(obj)
        } else {
            parameterToString(obj)
        }
    }

    /**
     * Adds any type to a MultipartBody.Builder
     * Defined a helper in the requestBody method to not duplicate code
     * It will be used when the content is a `FORM_DATA_MEDIA_TYPE` and the body of the PartConfig is not a File.
     *
     * @param name The field name to add in the request
     * @param headers The headers that are in the PartConfig
     * @param obj The field name to add in the request
     * @param serializer Optional custom serializer for this part
     * @return The method returns Unit but the new Part is added to the Builder that the extension function is applying on
     * @see requestBody
     */
    protected fun MultipartBody.Builder.addPartToMultiPart(name: String, headers: Map<String, String>, obj: Any?, serializer: ((Any?) -> String)? = null) {
        val partContentType = headers["Content-Type"]
        val partMediaType = partContentType?.toMediaTypeOrNull()
        val partBody = serializePartBody(obj, partContentType, serializer)
        addPart(
            buildPartHeaders(name, headers),
            partBody.toRequestBody(partMediaType)
        )
    }

    protected inline fun <reified T> requestBody(content: T, mediaType: String?): RequestBody =
        when {
            content is ByteArray -> content.toRequestBody((mediaType ?: guessContentTypeFromByteArray(content)).toMediaTypeOrNull())
            content is File -> content.asRequestBody((mediaType ?: guessContentTypeFromFile(content)).toMediaTypeOrNull())
            mediaType == FORM_DATA_MEDIA_TYPE ->
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
                                            addPartToMultiPart(name, part.headers, it, part.serializer)
                                        }
                                    }
                               }
                               else -> addPartToMultiPart(name, part.headers, part.body, part.serializer)
                            }
                        }
                    }.build()
            mediaType == FORM_URL_ENC_MEDIA_TYPE -> {
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
                        .toRequestBody((mediaType ?: JSON_MEDIA_TYPE).toMediaTypeOrNull())
                }
            mediaType == XML_MEDIA_TYPE -> throw UnsupportedOperationException("xml not currently supported.")
            mediaType == TEXT_MEDIA_TYPE -> {
                val textualContent = when (content) {
                    is Char, is CharSequence -> content.toString()
                    is Number -> content.toString()
                    is Boolean -> content.toString()
                    else -> throw UnsupportedOperationException("requestBody currently only supports text body containing primitive types: characters, numbers, or booleans.")
                }
                textualContent.toRequestBody(mediaType.toMediaTypeOrNull())
            }
            // TODO: this should be extended with other serializers
            else -> throw UnsupportedOperationException("requestBody currently only supports JSON body, text body, byte body and File body.")
        }

    @OptIn(ExperimentalStdlibApi::class)
    protected inline fun <reified T: Any?> responseBody(response: Response, mediaType: String? = JSON_MEDIA_TYPE): T? {
        val body = response.body ?: return null

        if (T::class.java == Unit::class.java) {
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
            mediaType == OCTET_MEDIA_TYPE -> body.bytes() as? T
            mediaType == TEXT_MEDIA_TYPE -> body.string() as? T
            else ->  throw UnsupportedOperationException("responseBody currently only supports JSON body, text body and byte body.")
        }
    }

    protected fun <T> updateAuthParams(requestConfig: RequestConfig<T>) {
        if (requestConfig.headers[AUTHORIZATION].isNullOrEmpty()) {
            accessToken?.let { accessToken ->
                requestConfig.headers[AUTHORIZATION] = "Bearer $accessToken "
            }
        }
        if (requestConfig.headers["api_key"].isNullOrEmpty()) {
            if (apiKey["api_key"] != null) {
                if (apiKeyPrefix["api_key"] != null) {
                    requestConfig.headers["api_key"] = apiKeyPrefix["api_key"]!! + " " + apiKey["api_key"]!!
                } else {
                    requestConfig.headers["api_key"] = apiKey["api_key"]!!
                }
            }
        }
    }

    protected inline fun <reified I, reified T: Any?> request(requestConfig: RequestConfig<I>): ApiResponse<T?> {
        val httpUrl = baseUrl.toHttpUrlOrNull() ?: throw IllegalStateException("baseUrl is invalid.")

        // take authMethod from operation
        updateAuthParams(requestConfig)

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
        if (requestConfig.body != null && requestConfig.headers[CONTENT_TYPE].isNullOrEmpty()) {
            requestConfig.headers[CONTENT_TYPE] = JSON_MEDIA_TYPE
        }
        if (requestConfig.headers[ACCEPT].isNullOrEmpty()) {
            requestConfig.headers[ACCEPT] = JSON_MEDIA_TYPE
        }
        val headers = requestConfig.headers

        if (headers[ACCEPT].isNullOrEmpty()) {
            throw kotlin.IllegalStateException("Missing ACCEPT header. This is required.")
        }

        val contentType = if (headers[CONTENT_TYPE] != null) {
            // TODO: support multiple contentType options here.
            (headers[CONTENT_TYPE] as String).substringBefore(";").lowercase(Locale.US)
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

        val accept = response.header(CONTENT_TYPE)?.substringBefore(";")?.lowercase(Locale.US)

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
        is OffsetDateTime -> parseDateToQueryString<OffsetDateTime>(value)
        is OffsetTime -> parseDateToQueryString<OffsetTime>(value)
        is LocalDateTime -> parseDateToQueryString<LocalDateTime>(value)
        is LocalDate -> parseDateToQueryString<LocalDate>(value)
        is LocalTime -> parseDateToQueryString<LocalTime>(value)
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
