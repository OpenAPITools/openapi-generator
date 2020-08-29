package org.openapitools.client.infrastructure

import kotlinx.serialization.Serializable
import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

@Serializable(with = OctetByteArray.Companion::class)
class OctetByteArray(val value: ByteArray) {
    companion object : KSerializer<OctetByteArray> {
        override val descriptor = PrimitiveSerialDescriptor("OctetByteArray", PrimitiveKind.STRING)
        override fun serialize(encoder: Encoder, value: OctetByteArray) = encoder.encodeString(hex(value.value))
        override fun deserialize(decoder: Decoder) = OctetByteArray(hex(decoder.decodeString()))
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false
        other as OctetByteArray
        return value.contentEquals(other.value)
    }

    override fun hashCode(): Int {
        return value.contentHashCode()
    }

    override fun toString(): String {
        return "OctetByteArray(${hex(value)})"
    }
}
