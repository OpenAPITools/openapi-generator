package org.openapitools.client.infrastructure

import com.squareup.moshi.Moshi
import com.squareup.moshi.adapters.EnumJsonAdapter
import org.openapitools.client.models.*
import com.squareup.moshi.kotlin.reflect.KotlinJsonAdapterFactory

object Serializer {
    @JvmStatic
    val moshiBuilder: Moshi.Builder = Moshi.Builder()
        .add(OffsetDateTimeAdapter())
        .add(LocalDateTimeAdapter())
        .add(LocalDateAdapter())
        .add(UUIDAdapter())
        .add(ByteArrayAdapter())
        .add(URIAdapter())
        .add(KotlinJsonAdapterFactory())
        .add(BigDecimalAdapter())
        .add(BigIntegerAdapter())
        .add(ModelWithEnumPropertyHavingDefault.PropertyName::class.java, EnumJsonAdapter.create(ModelWithEnumPropertyHavingDefault.PropertyName::class.java)
            .withUnknownFallback(ModelWithEnumPropertyHavingDefault.PropertyName.unknownDefaultOpenApi))

    @JvmStatic
    val moshi: Moshi by lazy {
        moshiBuilder.build()
    }
}
