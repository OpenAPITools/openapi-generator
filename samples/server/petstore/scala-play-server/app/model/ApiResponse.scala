package model

import play.api.libs.json._

/**
  * Describes the result of uploading an image resource
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), comments = "Generator version: 7.5.0-SNAPSHOT")
case class ApiResponse(
  code: Option[Int],
  `type`: Option[String],
  message: Option[String]
)

object ApiResponse {
  implicit lazy val apiResponseJsonFormat: Format[ApiResponse] = Json.format[ApiResponse]
}

