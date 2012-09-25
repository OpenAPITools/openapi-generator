package com.wordnik.client.model

import com.wordnik.client.model.Frequency
case class FrequencySummary (
  unknownYearCount: Int,
  totalCount: Long,
  frequencyString: String,
  word: String,
  frequency: List[Frequency])

