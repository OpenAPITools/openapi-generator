package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class FrequencySummary (
  UnknownYearCount: Option[Int],
  TotalCount: Option[Long],
  FrequencyString: Option[String],
  Word: Option[String],
  Frequency: Option[Seq[Frequency]])
   extends ApiModel


