package org.openapitools.client.infrastructure

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializer
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.SerialDescriptor
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

@Serializer(forClass = OffsetDateTime::class)
object OffsetDateTimeAdapter : KSerializer<OffsetDateTime> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("OffsetDateTime", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: OffsetDateTime) {
        encoder.encodeString(DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value))
    }

    override fun deserialize(decoder: Decoder): OffsetDateTime {
        return OffsetDateTime.parse(tryFormatDateTime(decoder.decodeString()), DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    private fun tryFormatDateTime(stringValue: String): String {
        val offset = stringValue.substringAfter("+")
        return when (offset.length) {
            4 -> fixOffsetValue(stringValue, offset)
            else -> stringValue
        }
    }

    private fun fixOffsetValue(stringValue: String = "0000", offset: String): String  {
        val offsetChars = offset.toCharArray().toMutableList()
        offsetChars.add(2, ':')
        return stringValue.replaceAfter("+", offsetChars.joinToString(""))
    }
}
