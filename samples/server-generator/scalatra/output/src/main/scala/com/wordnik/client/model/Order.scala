package com.wordnik.client.model

import java.util.Date
case class Order(
  id: Option[Long],

  petId: Option[Long],

  quantity: Option[Int],

  status: Option[String], // Order Status

  shipDate: Option[Date])

