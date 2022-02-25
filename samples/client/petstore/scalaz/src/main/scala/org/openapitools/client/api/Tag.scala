package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime


import Tag._

case class Tag (
  id: Option[Long],
name: Option[String])

object Tag {
  import DateTimeCodecs._

  implicit val TagCodecJson: CodecJson[Tag] = CodecJson.derive[Tag]
  implicit val TagDecoder: EntityDecoder[Tag] = jsonOf[Tag]
  implicit val TagEncoder: EntityEncoder[Tag] = jsonEncoderOf[Tag]
}
