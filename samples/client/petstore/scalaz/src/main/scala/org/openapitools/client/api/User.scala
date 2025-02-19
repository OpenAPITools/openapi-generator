package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime


import User._

case class User (
  id: Option[Long],
username: Option[String],
firstName: Option[String],
lastName: Option[String],
email: Option[String],
password: Option[String],
phone: Option[String],
/* User Status */
  userStatus: Option[Integer])

object User {
  import DateTimeCodecs._

  implicit val UserCodecJson: CodecJson[User] = CodecJson.derive[User]
  implicit val UserDecoder: EntityDecoder[User] = jsonOf[User]
  implicit val UserEncoder: EntityEncoder[User] = jsonEncoderOf[User]
}
