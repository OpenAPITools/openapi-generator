package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class AuthenticationToken (
  token: Option[String],
  userId: Option[Long],
  userSignature: Option[String])
   extends ApiModel


