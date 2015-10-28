package com.wordnik.client.model

import java.util.Date


case class Order (
  id: Long,
  petId: Long,
  quantity: Int,
  shipDate: Date,
  status: String,
  complete: Boolean
)
