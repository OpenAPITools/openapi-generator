package com.wordnik.petstore.model

case class User (
  id: Long,
  lastName: String,
  phone: String,
  username: String,
  email: String,
  /* User Status */
  userStatus: Int,
  firstName: String,
  password: String)

