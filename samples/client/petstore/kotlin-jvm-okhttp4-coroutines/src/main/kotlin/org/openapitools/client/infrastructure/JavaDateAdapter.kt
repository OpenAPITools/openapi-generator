package org.openapitools.client.infrastructure

import com.google.gson.TypeAdapter
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter
import com.google.gson.stream.JsonToken.NULL
import java.io.IOException
import java.util.Date

class JavaDateAdapter : TypeAdapter<Date>() {
    @Throws(IOException::class)
    override fun write(out: JsonWriter?, value: Date?) {
        if (value == null) {
            out?.nullValue()
        } else {
            out?.value(value.time)
        }
    }

    @Throws(IOException::class)
    override fun read(out: JsonReader?): Date? {
        out ?: return null

        when (out.peek()) {
            NULL -> {
                out.nextNull()
                return null
            }
            else -> {
                return Date(out.nextLong())
            }
        }
    }
}
