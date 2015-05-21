package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class User (
  id: Option[Long],
  username: Option[String],
  email: Option[String],
  status: Option[Int],
  faceBookId: Option[String],
  userName: Option[String],
  displayName: Option[String],
  password: Option[String])
   extends ApiModel


