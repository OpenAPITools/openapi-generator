package com.wordnik.client.model

import com.wordnik.client.model.ScoredWord
case class Sentence (
  id: Long,
  hasScoredWords: Boolean,
  scoredWords: List[ScoredWord],
  display: String,
  rating: Int,
  documentMetadataId: Long)

