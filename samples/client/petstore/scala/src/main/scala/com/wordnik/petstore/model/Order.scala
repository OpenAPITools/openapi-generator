package com.wordnik.petstore.model

import java.util.Date
case class Order (
  id: Long,
  petId: Long,
  /* Order Status */
  status: String,
  quantity: Int,
  shipDate: Date)

