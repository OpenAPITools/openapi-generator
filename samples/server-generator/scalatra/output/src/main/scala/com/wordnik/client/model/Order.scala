package com.wordnik.client.model

import java.util.Date
case class Order (
  id: Option[Long],

  petId: Option[Long],

  status: Option[String],// Order Status

  quantity: Option[Int],

  shipDate: Option[Date]

  )

