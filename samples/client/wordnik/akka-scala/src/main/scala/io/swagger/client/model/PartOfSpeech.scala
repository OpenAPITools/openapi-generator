package io.swagger.client.model


case class PartOfSpeech(
                         roots: Option[Seq[Root]],
                         storageAbbr: Option[Seq[String]],
                         allCategories: Option[Seq[Category]])
  extends ApiModel


