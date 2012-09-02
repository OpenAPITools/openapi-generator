package com.wordnik.client.model

import scala.reflect.BeanProperty

case class User (
  var id: Long,
  var lastName: String,
  var username: String,
  var phone: String,
  var email: String,
  /* User Status */
  var userStatus: Int,
  var firstName: String,
  var password: String)

