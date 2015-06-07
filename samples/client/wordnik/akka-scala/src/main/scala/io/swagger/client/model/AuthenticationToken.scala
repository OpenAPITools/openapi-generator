package io.swagger.client.model


case class AuthenticationToken(
                                token: Option[String],
                                userId: Option[Long],
                                userSignature: Option[String])
  extends ApiModel


