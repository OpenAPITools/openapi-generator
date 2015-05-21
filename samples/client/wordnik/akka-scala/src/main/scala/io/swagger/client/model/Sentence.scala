package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Sentence (
  hasScoredWords: Option[Boolean],
  id: Option[Long],
  scoredWords: Option[Seq[ScoredWord]],
  display: Option[String],
  rating: Option[Int],
  documentMetadataId: Option[Long])
   extends ApiModel


