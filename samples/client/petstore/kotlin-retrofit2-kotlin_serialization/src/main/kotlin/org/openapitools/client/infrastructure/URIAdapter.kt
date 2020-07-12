package org.openapitools.client.infrastructure

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.PrimitiveDescriptor
import kotlinx.serialization.Serializer
import kotlinx.serialization.PrimitiveKind
import kotlinx.serialization.SerialDescriptor
import java.net.URI

@Serializer(forClass = URI::class)
object UriAdapter : KSerializer<URI> {
    override fun serialize(encoder: Encoder, value: URI) {
        encoder.encodeString(value.toASCIIString())
    }

    override fun deserialize(decoder: Decoder): URI = URI(decoder.decodeString())

    override val descriptor: SerialDescriptor = PrimitiveDescriptor("URI", PrimitiveKind.STRING)
}