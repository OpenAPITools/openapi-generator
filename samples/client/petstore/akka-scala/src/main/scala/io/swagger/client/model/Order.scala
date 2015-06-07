package io.swagger.client.model


case class Order(
                  id: Option[Long],
                  petId: Option[Long],
                  quantity: Option[Int],
                  shipDate: Option[DateTime],
                  /* Order Status */
                  status: Option[OrderEnums.Status],
                  complete: Option[Boolean])
  extends ApiModel

object OrderEnums {

  type Status = Status.Value

  object Status extends Enumeration {
    val Placed = Value("placed")
    val Approved = Value("approved")
    val Delivered = Value("delivered")
  }


}

