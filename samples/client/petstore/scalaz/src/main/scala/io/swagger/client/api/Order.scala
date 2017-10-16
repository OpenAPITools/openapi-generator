package io.swagger.client.api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime
import Order._

case class Order (
  id: Option[Long],
petId: Option[Long],
quantity: Option[Integer],
shipDate: Option[DateTime],
/* Order Status */
  status: Option[Status],
complete: Option[Boolean])

object Order {
  import DateTimeCodecs._
  sealed trait Status
  case object Placed extends Status
  case object Approved extends Status
  case object Delivered extends Status

  object Status {
    def toStatus(s: String): Option[Status] = s match {
      case "Placed" => Some(Placed)
      case "Approved" => Some(Approved)
      case "Delivered" => Some(Delivered)
      case _ => None
    }

    def fromStatus(x: Status): String = x match {
      case Placed => "Placed"
      case Approved => "Approved"
      case Delivered => "Delivered"
    }
  }

  implicit val StatusEnumEncoder: EncodeJson[Status] =
    EncodeJson[Status](is => StringEncodeJson(Status.fromStatus(is)))

  implicit val StatusEnumDecoder: DecodeJson[Status] =
    DecodeJson.optionDecoder[Status](n => n.string.flatMap(jStr => Status.toStatus(jStr)), "Status failed to de-serialize")

  implicit val OrderCodecJson: CodecJson[Order] = CodecJson.derive[Order]
  implicit val OrderDecoder: EntityDecoder[Order] = jsonOf[Order]
  implicit val OrderEncoder: EntityEncoder[Order] = jsonEncoderOf[Order]
}
