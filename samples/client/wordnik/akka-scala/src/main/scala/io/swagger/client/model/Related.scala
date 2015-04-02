package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Related (
  label1: Option[String],
  relationshipType: Option[String],
  label2: Option[String],
  label3: Option[String],
  words: Option[Seq[String]],
  gram: Option[String],
  label4: Option[String])
   extends ApiModel


