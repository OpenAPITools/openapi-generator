package com.wordnik.client.model

case class User (
  id: Option[Long],

  lastName: Option[String],

  phone: Option[String],

  username: Option[String],

  email: Option[String],

  userStatus: Option[Int],// User Status

  firstName: Option[String],

  password: Option[String]

  )

