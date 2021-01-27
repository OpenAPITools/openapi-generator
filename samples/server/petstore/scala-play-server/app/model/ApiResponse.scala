package model

import play.api.libs.json._

/**
  * Describes the result of uploading an image resource
  */

case class ApiResponse(
  code: Option[Int],
  `type`: Option[String],
  message: Option[String]
)

object ApiResponse {
  implicit lazy val apiResponseJsonFormat: Format[ApiResponse] = Json.format[ApiResponse]
}

