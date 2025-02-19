package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import kotlinx.datetime.LocalTime

class LocalTimeAdapter {
    @ToJson
    fun toJson(value: LocalTime): String {
        return value.toString()
    }

    @FromJson
    fun fromJson(value: String): LocalTime {
        return LocalTime.parse(value)
    }

}
