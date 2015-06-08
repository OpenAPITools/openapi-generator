package io.swagger.client.model


case class ScoredWord(
                       position: Option[Int],
                       id: Option[Long],
                       docTermCount: Option[Int],
                       lemma: Option[String],
                       wordType: Option[String],
                       score: Option[Float],
                       sentenceId: Option[Long],
                       word: Option[String],
                       stopword: Option[Boolean],
                       baseWordScore: Option[Double],
                       partOfSpeech: Option[String])
  extends ApiModel


