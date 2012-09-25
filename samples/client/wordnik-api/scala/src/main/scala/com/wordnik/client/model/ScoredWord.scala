package com.wordnik.client.model

case class ScoredWord (
  id: Long,
  position: Int,
  lemma: String,
  docTermCount: Int,
  wordType: String,
  score: Float,
  word: String,
  sentenceId: Long,
  stopword: Boolean,
  baseWordScore: Double,
  partOfSpeech: String)

