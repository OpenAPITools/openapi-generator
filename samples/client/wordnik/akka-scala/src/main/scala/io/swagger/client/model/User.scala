package io.swagger.client.model


case class User(
                 id: Option[Long],
                 username: Option[String],
                 email: Option[String],
                 status: Option[Int],
                 faceBookId: Option[String],
                 userName: Option[String],
                 displayName: Option[String],
                 password: Option[String])
  extends ApiModel


