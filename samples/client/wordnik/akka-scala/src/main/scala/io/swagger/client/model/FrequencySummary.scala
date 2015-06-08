package io.swagger.client.model


case class FrequencySummary(
                             unknownYearCount: Option[Int],
                             totalCount: Option[Long],
                             frequencyString: Option[String],
                             word: Option[String],
                             frequency: Option[Seq[Frequency]])
  extends ApiModel


