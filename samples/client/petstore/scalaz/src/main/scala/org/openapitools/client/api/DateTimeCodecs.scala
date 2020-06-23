package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import java.time.OffsetDateTime

object DateTimeCodecs {
  implicit def dateTimeEncodeJson: EncodeJson[OffsetDateTime] =
    EncodeJson[OffsetDateTime](dt => StringEncodeJson(dt.toString))

  implicit def dateTimeDecodeJson: DecodeJson[OffsetDateTime] =
    DecodeJson.of[String].map(OffsetDateTime.parse(_)) setName "java.time.OffsetDateTime"
}
