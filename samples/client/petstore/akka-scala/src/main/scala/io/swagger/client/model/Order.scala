package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Order (
  Id: Option[Long],
  PetId: Option[Long],
  Quantity: Option[Int],
  ShipDate: Option[DateTime],
  /* Order Status */
  Status: Option[OrderEnums.Status],
  Complete: Option[Boolean])
   extends ApiModel

object OrderEnums {

  type Status = Status.Value
  
  object Status extends Enumeration {
    val Placed = Value("placed")
    val Approved = Value("approved")
    val Delivered = Value("delivered")
  }

  
}

