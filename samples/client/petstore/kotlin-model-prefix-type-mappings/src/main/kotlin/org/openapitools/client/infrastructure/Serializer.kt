package org.openapitools.client.infrastructure

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.util.UUID

object Serializer {
    @JvmStatic
    val gsonBuilder: GsonBuilder = GsonBuilder()
        .registerTypeAdapter(OffsetDateTime::class.java, OffsetDateTimeAdapter())
        .registerTypeAdapter(LocalDateTime::class.java, LocalDateTimeAdapter())
        .registerTypeAdapter(LocalDate::class.java, LocalDateAdapter())
        .registerTypeAdapter(ByteArray::class.java, ByteArrayAdapter())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiAnnotation.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiAnyOfUserOrPet.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiAnyOfUserOrPetOrArrayString.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiApiResponse.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiCategory.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiOrder.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiPet.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiTag.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiUser.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiUserOrPet.CustomTypeAdapterFactory())
        .registerTypeAdapterFactory(org.openapitools.client.models.ApiUserOrPetOrArrayString.CustomTypeAdapterFactory())

    @JvmStatic
    val gson: Gson by lazy {
        gsonBuilder.create()
    }
}
