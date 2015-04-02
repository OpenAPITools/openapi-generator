package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Example (
  Id: Option[Long],
  ExampleId: Option[Long],
  Title: Option[String],
  Text: Option[String],
  Score: Option[ScoredWord],
  Sentence: Option[Sentence],
  Word: Option[String],
  Provider: Option[ContentProvider],
  Year: Option[Int],
  Rating: Option[Float],
  DocumentId: Option[Long],
  Url: Option[String])
   extends ApiModel


