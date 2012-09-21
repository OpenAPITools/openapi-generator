package com.wordnik.client.model

import scala.reflect.BeanProperty

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

