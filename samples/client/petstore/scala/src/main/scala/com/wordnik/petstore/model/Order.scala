package com.wordnik.petstore.model

import java.util.Date
case class Order (
  /* Unique identifier for the order */
  id: Long,
  /* ID of pet being ordered */
  petId: Long,
  /* Number of pets ordered */
  quantity: Int,
  /* Status of the order */
  status: String,
  /* Date shipped, only if it has been */
  shipDate: Date)

