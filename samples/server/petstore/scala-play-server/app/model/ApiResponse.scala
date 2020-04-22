package model

import play.api.libs.json._

/**
  * Describes the result of uploading an image resource
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2020-01-04T23:10:22.106-05:00[America/New_York]")
case class ApiResponse(
  code: Option[Int],
  `type`: Option[String],
  message: Option[String]
)

object ApiResponse {
  implicit lazy val apiResponseJsonFormat: Format[ApiResponse] = Json.format[ApiResponse]
}

