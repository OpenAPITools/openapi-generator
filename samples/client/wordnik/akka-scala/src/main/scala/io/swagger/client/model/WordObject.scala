package io.swagger.client.model


case class WordObject(
                       id: Option[Long],
                       word: Option[String],
                       originalWord: Option[String],
                       suggestions: Option[Seq[String]],
                       canonicalForm: Option[String],
                       vulgar: Option[String])
  extends ApiModel


