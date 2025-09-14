package org.openapitools.client.infrastructure

import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*

@Serializable(OctetByteArray.Companion::class)
class OctetByteArray(val value: ByteArray) {
    companion object : KSerializer<OctetByteArray> {
        override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("OctetByteArray", PrimitiveKind.STRING)
        override fun serialize(encoder: Encoder, value: OctetByteArray): Unit = encoder.encodeString(hex(value.value))
        override fun deserialize(decoder: Decoder): OctetByteArray = OctetByteArray(hex(decoder.decodeString()))
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
