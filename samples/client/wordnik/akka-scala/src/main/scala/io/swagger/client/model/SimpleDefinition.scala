package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class SimpleDefinition (
  text: Option[String],
  source: Option[String],
  note: Option[String],
  partOfSpeech: Option[String])
   extends ApiModel


