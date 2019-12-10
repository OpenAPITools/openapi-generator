package org.openapitools.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime


import Pet._

case class Pet (
  id: Option[Long],
category: Option[Category],
name: String,
photoUrls: List[String],
tags: Option[List[Tag]],
/* pet status in the store */
  status: Option[Status])

object Pet {
  import DateTimeCodecs._
  sealed trait Status
  case object Available extends Status
  case object Pending extends Status
  case object Sold extends Status

  object Status {
    def toStatus(s: String): Option[Status] = s match {
      case "Available" => Some(Available)
      case "Pending" => Some(Pending)
      case "Sold" => Some(Sold)
      case _ => None
    }

    def fromStatus(x: Status): String = x match {
      case Available => "Available"
      case Pending => "Pending"
      case Sold => "Sold"
    }
  }

  implicit val StatusEnumEncoder: EncodeJson[Status] =
    EncodeJson[Status](is => StringEncodeJson(Status.fromStatus(is)))

  implicit val StatusEnumDecoder: DecodeJson[Status] =
    DecodeJson.optionDecoder[Status](n => n.string.flatMap(jStr => Status.toStatus(jStr)), "Status failed to de-serialize")

  implicit val PetCodecJson: CodecJson[Pet] = CodecJson.derive[Pet]
  implicit val PetDecoder: EntityDecoder[Pet] = jsonOf[Pet]
  implicit val PetEncoder: EntityEncoder[Pet] = jsonEncoderOf[Pet]
}
