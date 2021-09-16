package org.openapitools.client.request

import com.android.volley.Request
import com.android.volley.Response
import java.io.UnsupportedEncodingException
import java.lang.reflect.Type
import java.net.URLEncoder
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*

interface IRequestFactory {

    companion object {
        /**
         * ISO 8601 date time format.
         * @see https://en.wikipedia.org/wiki/ISO_8601
         */
        val DATE_TIME_FORMAT = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

        /**
         * ISO 8601 date format.
         * @see https://en.wikipedia.org/wiki/ISO_8601
         */
        val DATE_FORMAT = SimpleDateFormat("yyyy-MM-dd")

        init {
            DATE_TIME_FORMAT.timeZone = TimeZone.getTimeZone("UTC")
            DATE_FORMAT.timeZone = TimeZone.getTimeZone("UTC")
        }

        fun formatDateTime(datetime: Date): String {
            return DATE_TIME_FORMAT.format(datetime)
        }

        fun formatDate(date: Date): String {
            return DATE_FORMAT.format(date)
        }

        fun escapeString(str: String): String {
            return try {
                URLEncoder.encode(str, "UTF-8")
            } catch (e: UnsupportedEncodingException) {
                str
            }
        }

        fun parameterToString(param: Any?) =
            when (param) {
                null -> ""
                is Date -> formatDateTime(param)
                is Collection<*> -> {
                  val b = StringBuilder()
                  for (o in param) {
                      if (b.isNotEmpty()) {
                          b.append(",")
                      }
                      b.append(o.toString())
                  }
                  b.toString()
                }
                else -> param.toString()
            }
        }


    fun <T> build(
        method: Int,
        url : String,
        body: Any?,
        headers: Map<String, String>?,
        queryParams: Map<String, String>?,
        formParams: Map<String, String>?,
        contentTypeForBody: String?,
        type: Type,
        responseListener: Response.Listener<T>,
        errorListener: Response.ErrorListener): Request<T>
}