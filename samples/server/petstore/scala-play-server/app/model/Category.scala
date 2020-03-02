package model

import play.api.libs.json._

/**
  * A category for a pet
  */

case class Category(
  id: Option[Long],
  name: Option[String]
)

object Category {
  implicit lazy val categoryJsonFormat: Format[Category] = Json.format[Category]
}

