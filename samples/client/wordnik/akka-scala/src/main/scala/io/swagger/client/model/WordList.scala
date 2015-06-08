package io.swagger.client.model


case class WordList(
                     id: Option[Long],
                     permalink: Option[String],
                     name: Option[String],
                     createdAt: Option[DateTime],
                     updatedAt: Option[DateTime],
                     lastActivityAt: Option[DateTime],
                     username: Option[String],
                     userId: Option[Long],
                     description: Option[String],
                     numberWordsInList: Option[Long],
                     `type`: Option[String])
  extends ApiModel


