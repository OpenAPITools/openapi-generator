package org.openapitools.client.infrastructure

import java.util.Date
import java.math.BigDecimal
import java.math.BigInteger
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.util.UUID
import kotlinx.serialization.KSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.modules.serializersModuleOf
import kotlin.reflect.KClass
import java.net.URI
import java.net.URL
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

object Serializer {
    @JvmStatic
    val kotlinSerializationAdapters = serializersModuleOf(
        mapOf<KClass<*>, KSerializer<*>>(
            BigDecimal::class to BigDecimalAdapter,
            BigInteger::class to BigIntegerAdapter,
            Date::class to DateAdapter,
            LocalDate::class to LocalDateAdapter,
            LocalDateTime::class to LocalDateTimeAdapter,
            OffsetDateTime::class to OffsetDateTimeAdapter,
            UUID::class to UUIDAdapter,
            AtomicInteger::class to AtomicIntegerAdapter,
            AtomicLong::class to AtomicLongAdapter,
            AtomicBoolean::class to AtomicBooleanAdapter,
            URI::class to UriAdapter,
            URL::class to UrlAdapter,
            StringBuilder::class to StringBuilderAdapter
        )
    )

    @JvmStatic
    val jvmJson: Json by lazy { Json(context = kotlinSerializationAdapters) }
}
