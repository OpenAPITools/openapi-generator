package com.wordnik.petstore.model

case class User (
  id: Long,
  lastName: String,
  username: String,
  phone: String,
  email: String,
  /* User Status */
  userStatus: Int,
  firstName: String,
  password: String)

