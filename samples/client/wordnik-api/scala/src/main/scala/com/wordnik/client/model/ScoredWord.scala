package com.wordnik.client.model

case class ScoredWord (
  position: Int,
  id: Long,
  docTermCount: Int,
  lemma: String,
  wordType: String,
  score: Float,
  sentenceId: Long,
  word: String,
  stopword: Boolean,
  baseWordScore: Double,
  partOfSpeech: String)

