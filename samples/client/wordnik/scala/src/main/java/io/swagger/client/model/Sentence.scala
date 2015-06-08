package io.swagger.client.model


case class Sentence(
                     hasScoredWords: Boolean,
                     id: Long,
                     scoredWords: List[ScoredWord],
                     display: String,
                     rating: Integer,
                     documentMetadataId: Long)
  
