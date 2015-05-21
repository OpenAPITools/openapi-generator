package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class ApiTokenStatus (
  valid: Option[Boolean],
  token: Option[String],
  resetsInMillis: Option[Long],
  remainingCalls: Option[Long],
  expiresInMillis: Option[Long],
  totalRequests: Option[Long])
   extends ApiModel


