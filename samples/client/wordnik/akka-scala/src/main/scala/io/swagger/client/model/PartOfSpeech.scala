package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class PartOfSpeech (
  Roots: Option[Seq[Root]],
  StorageAbbr: Option[Seq[String]],
  AllCategories: Option[Seq[Category]])
   extends ApiModel


