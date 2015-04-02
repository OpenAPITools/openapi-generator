package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Sentence (
  HasScoredWords: Option[Boolean],
  Id: Option[Long],
  ScoredWords: Option[Seq[ScoredWord]],
  Display: Option[String],
  Rating: Option[Int],
  DocumentMetadataId: Option[Long])
   extends ApiModel


