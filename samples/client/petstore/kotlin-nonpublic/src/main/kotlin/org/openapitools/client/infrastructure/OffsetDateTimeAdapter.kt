package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

internal class OffsetDateTimeAdapter {
    @ToJson
    fun toJson(value: OffsetDateTime): String {
        return DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value)
    }

    @FromJson
    fun fromJson(value: String): OffsetDateTime {
        return OffsetDateTime.parse(tryFormatDateTime(value), DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    private fun tryFormatDateTime(stringValue: String): String {
        val offset = stringValue.substringAfter("+")
        return when (offset.length) {
            4 -> fixOffsetValue(stringValue, offset)
            else -> stringValue
        }
    }

    private fun fixOffsetValue(stringValue: String = "0000", offset: String): String  {
        val offsetChars = offset.toCharArray().toMutableList()
        offsetChars.add(2, ':')
        return stringValue.replaceAfter("+", offsetChars.joinToString(""))
    }
}
