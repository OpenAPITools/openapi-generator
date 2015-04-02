package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class ScoredWord (
  Position: Option[Int],
  Id: Option[Long],
  DocTermCount: Option[Int],
  Lemma: Option[String],
  WordType: Option[String],
  Score: Option[Float],
  SentenceId: Option[Long],
  Word: Option[String],
  Stopword: Option[Boolean],
  BaseWordScore: Option[Double],
  PartOfSpeech: Option[String])
   extends ApiModel


