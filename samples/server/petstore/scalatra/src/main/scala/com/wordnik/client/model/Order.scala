package com.wordnik.client.model

import java.util.Date

case class Order(
  id: Option[Long],
  petId: Option[Long],
  quantity: Option[Int],
  shipDate: Option[Date],
  status: Option[String], // Order Status
  complete: Option[Boolean])
