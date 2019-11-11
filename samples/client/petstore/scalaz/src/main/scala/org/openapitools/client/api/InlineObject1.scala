package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime
import InlineObject1._

case class InlineObject1 (
  /* Additional data to pass to server */
  additionalMetadata: Option[String],
/* file to upload */
  file: Option[File])

object InlineObject1 {
  import DateTimeCodecs._

  implicit val InlineObject1CodecJson: CodecJson[InlineObject1] = CodecJson.derive[InlineObject1]
  implicit val InlineObject1Decoder: EntityDecoder[InlineObject1] = jsonOf[InlineObject1]
  implicit val InlineObject1Encoder: EntityEncoder[InlineObject1] = jsonEncoderOf[InlineObject1]
}
