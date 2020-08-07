package org.openapitools.client.infrastructure

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.JsonElement
import io.gsonfire.GsonFireBuilder
import io.gsonfire.TypeSelector
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.util.UUID
import org.openapitools.client.models.*
import kotlin.collections.HashMap
import kotlin.collections.MutableMap
import kotlin.collections.get
import kotlin.collections.set
import kotlin.collections.toTypedArray
import java.util.Date

object Serializer {
    @JvmStatic
    var gsonBuilder = GsonFireBuilder()
        .createGsonBuilder()
        .registerTypeAdapter(Date::class.java, DateAdapter())
        .registerTypeAdapter(OffsetDateTime::class.java, OffsetDateTimeAdapter())
        .registerTypeAdapter(LocalDateTime::class.java, LocalDateTimeAdapter())
        .registerTypeAdapter(LocalDate::class.java, LocalDateAdapter())
        .registerTypeAdapter(ByteArray::class.java, ByteArrayAdapter())

    @JvmStatic
    val gson: Gson by lazy {
        gsonBuilder.create()
    }

    fun getDiscriminatorValue(readElement: JsonElement, discriminatorField: String?): String? {
        val element: JsonElement = readElement.getAsJsonObject().get(discriminatorField)
            ?: throw IllegalArgumentException("missing discriminator field: <$discriminatorField>")
        return element.getAsString()
    }

    fun getClassByDiscriminator(
        classByDiscriminatorValue: Map<*, *>,
        discriminatorValue: String?
    ): Class<*>? {
        return classByDiscriminatorValue[discriminatorValue?.toUpperCase()] as Class<*>?
            ?: throw IllegalArgumentException("cannot determine model class of name: <$discriminatorValue>")
    }
}
