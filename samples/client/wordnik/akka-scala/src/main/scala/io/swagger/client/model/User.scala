package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class User (
  Id: Option[Long],
  Username: Option[String],
  Email: Option[String],
  Status: Option[Int],
  FaceBookId: Option[String],
  UserName: Option[String],
  DisplayName: Option[String],
  Password: Option[String])
   extends ApiModel


