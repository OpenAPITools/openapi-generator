package model

import play.api.libs.json._

/**
  * A category for a pet
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-09-23T12:38:26.780+02:00[Europe/Prague]")
case class Category(
  id: Option[Long],
  name: Option[String]
)

object Category {
  implicit lazy val categoryJsonFormat: Format[Category] = Json.format[Category]
}

