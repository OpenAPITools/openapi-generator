package model

import play.api.libs.json._

/**
  * A category for a pet
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-10-03T13:49:17.716+02:00[Europe/Berlin]")
case class Category(
  id: Option[Long],
  name: Option[String]
)

object Category {
  implicit lazy val categoryJsonFormat: Format[Category] = Json.format[Category]
}

