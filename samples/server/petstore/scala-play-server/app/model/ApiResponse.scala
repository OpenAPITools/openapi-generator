package model

import play.api.libs.json._

/**
  * Describes the result of uploading an image resource
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-12-27T11:49:03.383+01:00[Europe/Vienna]")
case class ApiResponse(
  code: Option[Int],
  `type`: Option[String],
  message: Option[String]
)

object ApiResponse {
  implicit lazy val apiResponseJsonFormat: Format[ApiResponse] = Json.format[ApiResponse]
}

