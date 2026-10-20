package org.openapitools.client.infrastructure

import kotlinx.serialization.KSerializer
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.SerialDescriptor
import java.net.URI

object URIAdapter : KSerializer<URI> {
    override fun serialize(encoder: Encoder, value: URI) {
        encoder.encodeString(value.toASCIIString())
    }

    override fun deserialize(decoder: Decoder): URI = URI(decoder.decodeString())

    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("URI", PrimitiveKind.STRING)
}
