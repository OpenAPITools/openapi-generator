package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class ApiTokenStatus (
  Valid: Option[Boolean],
  Token: Option[String],
  ResetsInMillis: Option[Long],
  RemainingCalls: Option[Long],
  ExpiresInMillis: Option[Long],
  TotalRequests: Option[Long])
   extends ApiModel


