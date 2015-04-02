package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Definition (
  ExtendedText: Option[String],
  Text: Option[String],
  SourceDictionary: Option[String],
  Citations: Option[Seq[Citation]],
  Labels: Option[Seq[Label]],
  Score: Option[Float],
  ExampleUses: Option[Seq[ExampleUsage]],
  AttributionUrl: Option[String],
  SeqString: Option[String],
  AttributionText: Option[String],
  RelatedWords: Option[Seq[Related]],
  Sequence: Option[String],
  Word: Option[String],
  Notes: Option[Seq[Note]],
  TextProns: Option[Seq[TextPron]],
  PartOfSpeech: Option[String])
   extends ApiModel


