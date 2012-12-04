package com.wordnik.client.model

import com.wordnik.client.model.ScoredWord
case class Sentence (
  hasScoredWords: Boolean,
  id: Long,
  scoredWords: List[ScoredWord],
  display: String,
  rating: Int,
  documentMetadataId: Long)

