package io.swagger.client.model


case class Example(
                    id: Option[Long],
                    exampleId: Option[Long],
                    title: Option[String],
                    text: Option[String],
                    score: Option[ScoredWord],
                    sentence: Option[Sentence],
                    word: Option[String],
                    provider: Option[ContentProvider],
                    year: Option[Int],
                    rating: Option[Float],
                    documentId: Option[Long],
                    url: Option[String])
  extends ApiModel


