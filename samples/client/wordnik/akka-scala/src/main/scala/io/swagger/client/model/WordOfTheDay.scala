package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordOfTheDay (
  Id: Option[Long],
  ParentId: Option[String],
  Category: Option[String],
  CreatedBy: Option[String],
  CreatedAt: Option[DateTime],
  ContentProvider: Option[ContentProvider],
  HtmlExtra: Option[String],
  Word: Option[String],
  Definitions: Option[Seq[SimpleDefinition]],
  Examples: Option[Seq[SimpleExample]],
  Note: Option[String],
  PublishDate: Option[DateTime])
   extends ApiModel


