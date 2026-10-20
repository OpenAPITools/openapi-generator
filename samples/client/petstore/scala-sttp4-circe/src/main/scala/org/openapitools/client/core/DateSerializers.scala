package org.openapitools.client.core

import java.time.{LocalDate, OffsetDateTime}
import java.time.format.DateTimeFormatter

trait DateSerializers {
    import io.circe.{Decoder, Encoder}
    implicit val isoOffsetDateTimeDecoder: Decoder[OffsetDateTime] = Decoder.decodeOffsetDateTimeWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    implicit val isoOffsetDateTimeEncoder: Encoder[OffsetDateTime] = Encoder.encodeOffsetDateTimeWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

    implicit val localDateDecoder: Decoder[LocalDate] = Decoder.decodeLocalDateWithFormatter(DateTimeFormatter.ISO_LOCAL_DATE)
    implicit val localDateEncoder: Encoder[LocalDate] = Encoder.encodeLocalDateWithFormatter(DateTimeFormatter.ISO_LOCAL_DATE)
}
