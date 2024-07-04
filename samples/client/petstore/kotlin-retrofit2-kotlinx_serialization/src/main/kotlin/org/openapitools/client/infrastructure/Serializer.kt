package org.openapitools.client.infrastructure

import java.math.BigDecimal
import java.math.BigInteger
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.util.UUID
import kotlinx.serialization.json.Json
import kotlinx.serialization.modules.SerializersModule
import java.net.URI
import java.net.URL
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong

object Serializer {
    @Deprecated("Use Serializer.kotlinxSerializationAdapters instead", replaceWith = ReplaceWith("Serializer.kotlinxSerializationAdapters"))
    @JvmStatic
    val kotlinSerializationAdapters: SerializersModule
        get() { return kotlinxSerializationAdapters }

    @JvmStatic
    val kotlinxSerializationAdapters = SerializersModule {
        contextual(BigDecimal::class, BigDecimalAdapter)
        contextual(BigInteger::class, BigIntegerAdapter)
        contextual(LocalDate::class, LocalDateAdapter)
        contextual(LocalDateTime::class, LocalDateTimeAdapter)
        contextual(OffsetDateTime::class, OffsetDateTimeAdapter)
        contextual(UUID::class, UUIDAdapter)
        contextual(AtomicInteger::class, AtomicIntegerAdapter)
        contextual(AtomicLong::class, AtomicLongAdapter)
        contextual(AtomicBoolean::class, AtomicBooleanAdapter)
        contextual(URI::class, URIAdapter)
        contextual(URL::class, URLAdapter)
        contextual(StringBuilder::class, StringBuilderAdapter)
    }

    @Deprecated("Use Serializer.kotlinxSerializationJson instead", replaceWith = ReplaceWith("Serializer.kotlinxSerializationJson"))
    @JvmStatic
    val jvmJson: Json
        get() { return kotlinxSerializationJson }

    @JvmStatic
    val kotlinxSerializationJson: Json by lazy {
        Json {
            serializersModule = kotlinxSerializationAdapters
            ignoreUnknownKeys = true
            isLenient = true
        }
    }
}
