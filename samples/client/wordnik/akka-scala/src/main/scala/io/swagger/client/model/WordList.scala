package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordList (
  Id: Option[Long],
  Permalink: Option[String],
  Name: Option[String],
  CreatedAt: Option[DateTime],
  UpdatedAt: Option[DateTime],
  LastActivityAt: Option[DateTime],
  Username: Option[String],
  UserId: Option[Long],
  Description: Option[String],
  NumberWordsInList: Option[Long],
  Type: Option[String])
   extends ApiModel


