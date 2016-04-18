package io.swagger.client.model

import org.joda.time.DateTime
import java.util.UUID


case class User (
  id: Option[Long],
username: Option[String],
firstName: Option[String],
lastName: Option[String],
email: Option[String],
password: Option[String],
phone: Option[String],
userStatus: Option[Integer]  // User Status
)
