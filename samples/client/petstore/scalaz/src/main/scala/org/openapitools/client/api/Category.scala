package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime


import Category._

case class Category (
  id: Option[Long],
name: Option[String])

object Category {
  import DateTimeCodecs._

  implicit val CategoryCodecJson: CodecJson[Category] = CodecJson.derive[Category]
  implicit val CategoryDecoder: EntityDecoder[Category] = jsonOf[Category]
  implicit val CategoryEncoder: EntityEncoder[Category] = jsonEncoderOf[Category]
}
