package com.wordnik.client.model



case class ApiResponse (
  code: Option[Int],
type: Option[String],
message: Option[String]
)
