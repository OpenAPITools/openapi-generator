package io.swagger.client.model


case class ApiTokenStatus(
                           valid: Option[Boolean],
                           token: Option[String],
                           resetsInMillis: Option[Long],
                           remainingCalls: Option[Long],
                           expiresInMillis: Option[Long],
                           totalRequests: Option[Long])
  extends ApiModel


