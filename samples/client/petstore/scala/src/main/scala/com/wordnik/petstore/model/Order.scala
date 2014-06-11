package com.wordnik.petstore.model

import java.util.Date
case class Order (
  id: Long,
  petId: Long,
  quantity: Int,
  /* Order Status */
  status: String,
  shipDate: Date)

