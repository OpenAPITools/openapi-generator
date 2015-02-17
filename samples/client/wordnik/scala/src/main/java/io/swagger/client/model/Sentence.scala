package io.swagger.client.model

import io.swagger.client.model.ScoredWord



case class Sentence (
  hasScoredWords: Boolean,
  id: Long,
  scoredWords: List[ScoredWord],
  display: String,
  rating: Integer,
  documentMetadataId: Long)
  
