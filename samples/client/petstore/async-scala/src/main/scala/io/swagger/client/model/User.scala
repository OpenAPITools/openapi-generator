package io.swagger.client.model

import org.joda.time.DateTime


case class User (
  id: Long,
  username: String,
  firstName: String,
  lastName: String,
  email: String,
  password: String,
  phone: String,
  userStatus: Integer  // User Status
  
)
