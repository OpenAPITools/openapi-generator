package com.wordnik.petstore.model

import java.util.Date
case class Order (
  id: Long,
  /* Order Status */
  status: String,
  petId: Long,
  quantity: Int,
  shipDate: Date)

