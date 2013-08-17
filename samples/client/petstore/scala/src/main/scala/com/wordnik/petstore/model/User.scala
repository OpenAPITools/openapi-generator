package com.wordnik.petstore.model

case class User (
  id: Long,
  firstName: String,
  username: String,
  lastName: String,
  email: String,
  password: String,
  phone: String,
  /* User Status */
  userStatus: Int)

