package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class WordObject (
  Id: Option[Long],
  Word: Option[String],
  OriginalWord: Option[String],
  Suggestions: Option[Seq[String]],
  CanonicalForm: Option[String],
  Vulgar: Option[String])
   extends ApiModel


