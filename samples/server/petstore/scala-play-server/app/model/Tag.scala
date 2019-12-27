package model

import play.api.libs.json._

/**
  * A tag for a pet
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-12-27T11:49:03.383+01:00[Europe/Vienna]")
case class Tag(
  id: Option[Long],
  name: Option[String]
)

object Tag {
  implicit lazy val tagJsonFormat: Format[Tag] = Json.format[Tag]
}

