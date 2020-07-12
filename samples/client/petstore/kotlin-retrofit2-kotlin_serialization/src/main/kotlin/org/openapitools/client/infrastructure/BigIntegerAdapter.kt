package org.openapitools.client.infrastructure

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.PrimitiveDescriptor
import kotlinx.serialization.Serializer
import kotlinx.serialization.PrimitiveKind
import kotlinx.serialization.SerialDescriptor
import java.math.BigInteger

@Serializer(forClass = BigInteger::class)
object BigIntegerAdapter : KSerializer<BigInteger> {
    override val descriptor: SerialDescriptor = PrimitiveDescriptor("BigInteger", PrimitiveKind.STRING)

    override fun deserialize(decoder: Decoder): BigInteger {
        return BigInteger(decoder.decodeString())
    }

    override fun serialize(encoder: Encoder, value: BigInteger) {
        encoder.encodeString(value.toString())
    }
}