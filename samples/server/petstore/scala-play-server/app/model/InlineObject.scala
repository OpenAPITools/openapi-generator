package model

import play.api.libs.json._

/**
  * Represents the Swagger definition for inline_object.
  * @param name Updated name of the pet
  * @param status Updated status of the pet
  */

case class InlineObject(
  name: Option[String],
  status: Option[String]
)

object InlineObject {
  implicit lazy val inlineObjectJsonFormat: Format[InlineObject] = Json.format[InlineObject]
}

