package com.wordnik.client.model



case class User (
  id: Long,
  username: String,
  firstName: String,
  lastName: String,
  email: String,
  password: String,
  phone: String,
  userStatus: Int
)
