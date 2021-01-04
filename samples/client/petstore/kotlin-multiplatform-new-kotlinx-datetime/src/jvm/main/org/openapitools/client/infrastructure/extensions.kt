package org.openapitools.client.infrastructure

import kotlinx.datetime.toJavaLocalDateTime
import kotlinx.datetime.toJavaZoneOffset
import kotlinx.datetime.toKotlinLocalDateTime
import kotlinx.datetime.toKotlinZoneOffset

@JvmSynthetic
public fun java.time.OffsetDateTime.toKotlinOffsetDateTime(): OffsetDateTime {
    return OffsetDateTime.of(
        toLocalDateTime().toKotlinLocalDateTime(),
        toOffsetTime().offset.toKotlinZoneOffset()
    )
}

@JvmSynthetic
public fun OffsetDateTime.toJavaOffsetDateTime(): java.time.OffsetDateTime {
    return java.time.OffsetDateTime.of(dateTime.toJavaLocalDateTime(), offset.toJavaZoneOffset())
}

public fun offsetDateTimeToKotlin(offsetDateTime: java.time.OffsetDateTime): OffsetDateTime =
    offsetDateTime.toKotlinOffsetDateTime()

public fun offsetDateTimeToJava(offsetDateTime: OffsetDateTime): java.time.OffsetDateTime =
    offsetDateTime.toJavaOffsetDateTime()
