package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class User (
  Id: Option[Long],
  Username: Option[String],
  FirstName: Option[String],
  LastName: Option[String],
  Email: Option[String],
  Password: Option[String],
  Phone: Option[String],
  /* User Status */
  UserStatus: Option[Int])
   extends ApiModel


