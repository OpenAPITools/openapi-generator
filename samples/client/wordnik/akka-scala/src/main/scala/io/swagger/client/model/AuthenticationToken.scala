package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class AuthenticationToken (
  Token: Option[String],
  UserId: Option[Long],
  UserSignature: Option[String])
   extends ApiModel


