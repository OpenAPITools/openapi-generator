package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordListWord (
  Id: Option[Long],
  Word: Option[String],
  Username: Option[String],
  UserId: Option[Long],
  CreatedAt: Option[DateTime],
  NumberCommentsOnWord: Option[Long],
  NumberLists: Option[Long])
   extends ApiModel


