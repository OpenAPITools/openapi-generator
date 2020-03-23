package model

import play.api.libs.json._
import java.time.OffsetDateTime

/**
  * An order for a pets from the pet store
  * @param status Order Status
  */

case class Order(
  id: Option[Long],
  petId: Option[Long],
  quantity: Option[Int],
  shipDate: Option[OffsetDateTime],
  status: Option[Order.Status.Value],
  complete: Option[Boolean]
)

object Order {
  implicit lazy val orderJsonFormat: Format[Order] = Json.format[Order]

  // noinspection TypeAnnotation
  object Status extends Enumeration {
    val Placed = Value("placed")
    val Approved = Value("approved")
    val Delivered = Value("delivered")

    type Status = Value
    implicit lazy val StatusJsonFormat: Format[Value] = Format(Reads.enumNameReads(this), Writes.enumNameWrites[this.type])
  }
}

