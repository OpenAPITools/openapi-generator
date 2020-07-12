package org.openapitools.client.infrastructure

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.PrimitiveDescriptor
import kotlinx.serialization.Serializer
import kotlinx.serialization.PrimitiveKind
import kotlinx.serialization.SerialDescriptor
import java.util.concurrent.atomic.AtomicBoolean

@Serializer(forClass = AtomicBoolean::class)
object AtomicBooleanAdapter : KSerializer<AtomicBoolean> {
    override fun serialize(encoder: Encoder, value: AtomicBoolean) {
        encoder.encodeBoolean(value.get())
    }

    override fun deserialize(decoder: Decoder): AtomicBoolean = AtomicBoolean(decoder.decodeBoolean())

    override val descriptor: SerialDescriptor = PrimitiveDescriptor("AtomicBoolean", PrimitiveKind.BOOLEAN)
}