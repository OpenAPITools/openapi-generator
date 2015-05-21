package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class PartOfSpeech (
  roots: Option[Seq[Root]],
  storageAbbr: Option[Seq[String]],
  allCategories: Option[Seq[Category]])
   extends ApiModel


