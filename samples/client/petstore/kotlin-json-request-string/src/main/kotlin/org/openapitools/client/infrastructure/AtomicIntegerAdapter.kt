package org.openapitools.client.infrastructure

import kotlinx.serialization.KSerializer
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.SerialDescriptor
import java.util.concurrent.atomic.AtomicInteger

object AtomicIntegerAdapter : KSerializer<AtomicInteger> {
    override fun serialize(encoder: Encoder, value: AtomicInteger) {
        encoder.encodeInt(value.get())
    }

    override fun deserialize(decoder: Decoder): AtomicInteger = AtomicInteger(decoder.decodeInt())

    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("AtomicInteger", PrimitiveKind.INT)
}
