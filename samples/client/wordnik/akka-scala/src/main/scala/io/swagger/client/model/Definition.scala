package io.swagger.client.model


case class Definition(
                       extendedText: Option[String],
                       text: Option[String],
                       sourceDictionary: Option[String],
                       citations: Option[Seq[Citation]],
                       labels: Option[Seq[Label]],
                       score: Option[Float],
                       exampleUses: Option[Seq[ExampleUsage]],
                       attributionUrl: Option[String],
                       seqString: Option[String],
                       attributionText: Option[String],
                       relatedWords: Option[Seq[Related]],
                       sequence: Option[String],
                       word: Option[String],
                       notes: Option[Seq[Note]],
                       textProns: Option[Seq[TextPron]],
                       partOfSpeech: Option[String])
  extends ApiModel


