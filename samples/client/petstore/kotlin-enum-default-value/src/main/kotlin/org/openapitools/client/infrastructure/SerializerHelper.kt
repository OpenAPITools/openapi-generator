package org.openapitools.client.infrastructure

import com.squareup.moshi.Moshi
import com.squareup.moshi.adapters.EnumJsonAdapter

object SerializerHelper {
    fun addEnumUnknownDefaultCase(moshiBuilder: Moshi.Builder): Moshi.Builder {
        return moshiBuilder
            .add(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName::class.java, EnumJsonAdapter.create(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName::class.java)
                .withUnknownFallback(org.openapitools.client.models.ModelWithEnumPropertyHavingDefault.PropertyName.unknownDefaultOpenApi))
    }
}
