package org.openapitools.client.infrastructure

import com.squareup.moshi.FromJson
import com.squareup.moshi.ToJson
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

internal class LocalDateTimeAdapter {
    @ToJson
    fun toJson(value: LocalDateTime): String {
        return DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(value)
    }

    @FromJson
    fun fromJson(value: String): LocalDateTime {
        return LocalDateTime.parse(value, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    }

}
