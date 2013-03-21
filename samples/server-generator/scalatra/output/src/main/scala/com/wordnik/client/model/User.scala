package com.wordnik.client.model

import scala.reflect.BeanProperty

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

