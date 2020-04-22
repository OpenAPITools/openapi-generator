package model

import play.api.libs.json._

/**
  * A tag for a pet
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2020-01-04T23:10:22.106-05:00[America/New_York]")
case class Tag(
  id: Option[Long],
  name: Option[String]
)

object Tag {
  implicit lazy val tagJsonFormat: Format[Tag] = Json.format[Tag]
}

