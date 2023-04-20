package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.util.Date

class JavaDateAdapter {
    @ToJson
    fun toJson(value: Date): String = value.time.toString()

    @FromJson
    fun fromJson(value: String): Date = Date(value.toLong())
}
