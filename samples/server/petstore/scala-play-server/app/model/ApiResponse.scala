package model

import play.api.libs.json._

/**
  * Describes the result of uploading an image resource
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-09-23T12:38:26.780+02:00[Europe/Prague]")
case class ApiResponse(
  code: Option[Int],
  `type`: Option[String],
  message: Option[String]
)

object ApiResponse {
  implicit lazy val apiResponseJsonFormat: Format[ApiResponse] = Json.format[ApiResponse]
}

