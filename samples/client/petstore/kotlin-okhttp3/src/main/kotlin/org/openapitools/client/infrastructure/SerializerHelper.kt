package org.openapitools.client.infrastructure

import com.squareup.moshi.Moshi
import com.squareup.moshi.adapters.EnumJsonAdapter

object SerializerHelper {
    fun addEnumUnknownDefaultCase(moshiBuilder: Moshi.Builder): Moshi.Builder {
        return moshiBuilder
            .add(org.openapitools.client.models.Order.Status::class.java, EnumJsonAdapter.create(org.openapitools.client.models.Order.Status::class.java)
                .withUnknownFallback(org.openapitools.client.models.Order.Status.unknownDefaultOpenApi))
            .add(org.openapitools.client.models.Pet.Status::class.java, EnumJsonAdapter.create(org.openapitools.client.models.Pet.Status::class.java)
                .withUnknownFallback(org.openapitools.client.models.Pet.Status.unknownDefaultOpenApi))
    }
}
