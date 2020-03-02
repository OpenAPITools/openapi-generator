package model

import play.api.libs.json._

/**
  * A User who is purchasing from the pet store
  * @param userStatus User Status
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2020-02-29T14:21:53.710+07:00[Asia/Bangkok]")
case class User(
  id: Option[Long],
  username: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String],
  password: Option[String],
  phone: Option[String],
  userStatus: Option[Int]
)

object User {
  implicit lazy val userJsonFormat: Format[User] = Json.format[User]
}

