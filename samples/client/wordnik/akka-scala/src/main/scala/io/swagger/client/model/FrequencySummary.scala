package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class FrequencySummary (
  unknownYearCount: Option[Int],
  totalCount: Option[Long],
  frequencyString: Option[String],
  word: Option[String],
  frequency: Option[Seq[Frequency]])
   extends ApiModel


