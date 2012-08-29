package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

case class Order (
  var id: Long,
  var petId: Long,
  /* Order Status */
  var status: String,
  var quantity: Int,
  var shipDate: Date)

