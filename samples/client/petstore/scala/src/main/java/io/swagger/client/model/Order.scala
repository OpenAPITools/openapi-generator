package io.swagger.client.model

import org.joda.time.DateTime



case class Order (
  id: Long,
  petId: Long,
  quantity: Integer,
  shipDate: DateTime,
  /* Order Status */
  status: String,
  complete: Boolean)
  
