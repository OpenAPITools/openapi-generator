package org.openapitools.client.request

import com.android.volley.NetworkResponse
import com.android.volley.ParseError
import com.android.volley.Request
import com.android.volley.Response
import com.android.volley.toolbox.HttpHeaderParser
import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.JsonSyntaxException
import java.io.UnsupportedEncodingException
import java.nio.charset.Charset
import java.net.HttpURLConnection
import java.lang.reflect.Type
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime

import org.openapitools.client.infrastructure.OffsetDateTimeAdapter
import org.openapitools.client.infrastructure.LocalDateTimeAdapter
import org.openapitools.client.infrastructure.LocalDateAdapter
import org.openapitools.client.infrastructure.ByteArrayAdapter

class GsonRequest<T>(
    method: Int,
    url: String,
    private val body: Any?,
    private val headers: Map<String, String>?,
    private val params: MutableMap<String, String>?,
    private val contentTypeForBody: String?,
    private val encodingForParams: String?,
    private val gsonAdapters: Map<Type, Any>?,
    private val type: Type,
    private val listener: Response.Listener<T>,
    errorListener: Response.ErrorListener
) : Request<T>(method, url, errorListener) {

    val gsonBuilder: GsonBuilder = GsonBuilder()
        .registerTypeAdapter(OffsetDateTime::class.java, OffsetDateTimeAdapter())
        .registerTypeAdapter(LocalDateTime::class.java, LocalDateTimeAdapter())
        .registerTypeAdapter(LocalDate::class.java, LocalDateAdapter())
        .registerTypeAdapter(ByteArray::class.java, ByteArrayAdapter())
        .apply {
            gsonAdapters?.forEach {
                this.registerTypeAdapter(it.key, it.value)
            }
        }

    val gson: Gson by lazy {
        gsonBuilder.create()
    }

    private var response: NetworkResponse? = null

    override fun deliverResponse(response: T?) {
        listener.onResponse(response)
    }

    override fun getParams(): MutableMap<String, String>? = params ?: super.getParams()

    override fun getBodyContentType(): String = contentTypeForBody ?: super.getBodyContentType()

    override fun getParamsEncoding(): String = encodingForParams ?: super.getParamsEncoding()

    override fun getHeaders(): MutableMap<String, String> {
        val combined = HashMap<String, String>()
        combined.putAll(super.getHeaders())
        if (headers != null) {
            combined.putAll(headers)
        }
        return combined
    }

    override fun getBody(): ByteArray? {
        if (body != null) {
            return gson.toJson(body).toByteArray(Charsets.UTF_8)
    }
        return super.getBody()
    }

    override fun parseNetworkResponse(response: NetworkResponse?): Response<T> {
        return try {
            this.response = copyTo(response)
            val json = String(
                response?.data ?: ByteArray(0),
                Charset.forName(HttpHeaderParser.parseCharset(response?.headers))
            )
            Response.success(
                gson.fromJson<T>(json, type),
                HttpHeaderParser.parseCacheHeaders(response)
            )
        } catch (e: UnsupportedEncodingException) {
            Response.error(ParseError(e))
        } catch (e: JsonSyntaxException) {
            Response.error(ParseError(e))
        }
    }

    private fun copyTo(response: NetworkResponse?): NetworkResponse {
        return if (response != null) {
            NetworkResponse(
                response.statusCode,
                response.data,
                response.notModified,
                response.networkTimeMs,
                response.allHeaders
            )
        } else {
            // Return an empty response.
            NetworkResponse(
                HttpURLConnection.HTTP_BAD_METHOD,
                ByteArray(0),
                false,
                0,
                emptyList()
            )
        }
    }
}