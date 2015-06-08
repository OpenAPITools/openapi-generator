package io.swagger.client.model


case class ScoredWord(
                       position: Integer,
                       id: Long,
                       docTermCount: Integer,
                       lemma: String,
                       wordType: String,
                       score: Float,
                       sentenceId: Long,
                       word: String,
                       stopword: Boolean,
                       baseWordScore: Double,
                       partOfSpeech: String)
  
