package org.openapitools.client.infrastructure

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.PrimitiveDescriptor
import kotlinx.serialization.Serializer
import kotlinx.serialization.PrimitiveKind
import kotlinx.serialization.SerialDescriptor

@Serializer(forClass = StringBuilder::class)
object StringBuilderAdapter : KSerializer<StringBuilder> {
    override fun serialize(encoder: Encoder, value: StringBuilder) {
        encoder.encodeString(value.toString())
    }

    override fun deserialize(decoder: Decoder): StringBuilder = StringBuilder(decoder.decodeString())

    override val descriptor: SerialDescriptor = PrimitiveDescriptor("StringBuilder", PrimitiveKind.STRING)
}