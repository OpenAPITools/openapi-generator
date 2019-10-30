package org.openapitools.client.infrastructure

import com.google.gson.TypeAdapter
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter
import org.json.JSONObject
import java.io.IOException

class ByteArrayAdapter : TypeAdapter<ByteArray>() {
    @Throws(IOException::class)
    override fun write(out: JsonWriter?, value: ByteArray?) {
        if (value == null) {
            out?.nullValue()
        } else {
            out?.value(String(value))
        }
    }

    @Throws(IOException::class)
    override fun read(out: JsonReader?): ByteArray? {
        out ?: return null

        when (out.peek()) {
            JSONObject.NULL -> {
                out.nextNull()
                return null
            }
            else -> {
                return out.nextString().toByteArray()
            }
        }
    }
}
