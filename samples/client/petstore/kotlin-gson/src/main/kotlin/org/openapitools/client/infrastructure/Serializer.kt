package org.openapitools.client.infrastructure

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.UUID
import java.util.Date

object Serializer {
    @JvmStatic
    val gson: Gson = GsonBuilder()
        .registerTypeAdapter(Date::class.java, DateAdapter())
        .registerTypeAdapter(LocalDateTime::class.java, LocalDateTimeAdapter())
        .registerTypeAdapter(LocalDate::class.java, LocalDateAdapter())
        .registerTypeAdapter(UUID::class.java, UUIDAdapter())
        .registerTypeAdapter(ByteArray::class.java, ByteArrayAdapter())
        .create()
}
