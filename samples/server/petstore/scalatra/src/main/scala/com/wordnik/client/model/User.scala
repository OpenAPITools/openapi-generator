package com.wordnik.client.model

case class User(
  id: Option[Long],
  username: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String],
  password: Option[String],
  phone: Option[String],
  userStatus: Option[Int] // User Status
  )
