package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class User (
  id: Option[Long],
  username: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String],
  password: Option[String],
  phone: Option[String],
  /* User Status */
  userStatus: Option[Int])
   extends ApiModel


