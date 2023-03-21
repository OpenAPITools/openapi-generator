package org.openapitools.client.infrastructure

import com.google.gson.TypeAdapter
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter
import com.google.gson.stream.JsonToken.NULL
import java.io.IOException
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class OffsetDateTimeAdapter(private val formatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME) : TypeAdapter<OffsetDateTime>() {
    @Throws(IOException::class)
    override fun write(out: JsonWriter?, value: OffsetDateTime?) {
        if (value == null) {
            out?.nullValue()
        } else {
            out?.value(formatter.format(value))
        }
    }

    @Throws(IOException::class)
    override fun read(out: JsonReader?): OffsetDateTime? {
        out ?: return null

        return when (out.peek()) {
            NULL -> {
                out.nextNull()
                return null
            }
            else -> OffsetDateTime.parse(tryFormatDateTime(out.nextString()), formatter)
        }
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
