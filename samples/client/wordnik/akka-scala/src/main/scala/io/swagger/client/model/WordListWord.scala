package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordListWord (
  id: Option[Long],
  word: Option[String],
  username: Option[String],
  userId: Option[Long],
  createdAt: Option[DateTime],
  numberCommentsOnWord: Option[Long],
  numberLists: Option[Long])
   extends ApiModel


