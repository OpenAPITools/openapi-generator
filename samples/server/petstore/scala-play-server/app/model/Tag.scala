package model

import play.api.libs.json._

/**
  * A tag for a pet
  */

case class Tag(
  id: Option[Long],
  name: Option[String]
)

object Tag {
  implicit lazy val tagJsonFormat: Format[Tag] = Json.format[Tag]
}

