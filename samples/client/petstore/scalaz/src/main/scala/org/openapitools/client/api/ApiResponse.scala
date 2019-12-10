package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime


import ApiResponse._

case class ApiResponse (
  code: Option[Integer],
`type`: Option[String],
message: Option[String])

object ApiResponse {
  import DateTimeCodecs._

  implicit val ApiResponseCodecJson: CodecJson[ApiResponse] = CodecJson.derive[ApiResponse]
  implicit val ApiResponseDecoder: EntityDecoder[ApiResponse] = jsonOf[ApiResponse]
  implicit val ApiResponseEncoder: EntityEncoder[ApiResponse] = jsonEncoderOf[ApiResponse]
}
