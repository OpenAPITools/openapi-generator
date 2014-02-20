package com.wordnik.client.model

case class User(
  id: Option[Long],

  firstName: Option[String],

  username: Option[String],

  lastName: Option[String],

  email: Option[String],

  password: Option[String],

  phone: Option[String],

  userStatus: Option[Int] // User Status
  )

