package model

import play.api.libs.json._

/**
  * A category for a pet
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2019-11-18T19:56:26.062753Z[Europe/London]")
case class Category(
  id: Option[Long],
  name: Option[String]
)

object Category {
  implicit lazy val categoryJsonFormat: Format[Category] = Json.format[Category]
}

