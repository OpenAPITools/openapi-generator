package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

case class Order (
  id: Long,
  petId: Long,
  /* Order Status */
  status: String,
  quantity: Int,
  shipDate: Date)

