package org.openapitools.client.request

import com.android.volley.Request
import com.android.volley.Response
import java.io.UnsupportedEncodingException
import java.lang.reflect.Type
import java.net.URLEncoder
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*
import java.time.format.DateTimeFormatter
import java.time.OffsetDateTime
import java.time.LocalDate


interface IRequestFactory {

    companion object {
        /**
         * ISO 8601 date time format.
         * @see https://en.wikipedia.org/wiki/ISO_8601
         */
        fun formatDateTime(datetime: OffsetDateTime) = DateTimeFormatter.ISO_INSTANT.format(datetime)
        fun formatDate(date: LocalDate) = DateTimeFormatter.ISO_LOCAL_DATE.format(date)

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
                is OffsetDateTime -> formatDateTime(param)
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