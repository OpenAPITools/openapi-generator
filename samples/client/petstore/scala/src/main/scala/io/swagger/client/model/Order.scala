package io.swagger.client.model


case class Order(
                  id: Long,
                  petId: Long,
                  quantity: Integer,
                  shipDate: DateTime,
                  /* Order Status */
                  status: String,
                  complete: Boolean)
  
