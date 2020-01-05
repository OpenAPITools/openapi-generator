package model

import play.api.libs.json._
import java.time.OffsetDateTime

/**
  * An order for a pets from the pet store
  * @param status Order Status
  */
@javax.annotation.Generated(value = Array("org.openapitools.codegen.languages.ScalaPlayFrameworkServerCodegen"), date = "2020-01-04T23:10:22.106-05:00[America/New_York]")
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

