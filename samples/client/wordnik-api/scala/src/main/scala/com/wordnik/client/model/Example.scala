package com.wordnik.client.model

import com.wordnik.client.model.Sentence
import com.wordnik.client.model.ScoredWord
import com.wordnik.client.model.ContentProvider
case class Example (
  id: Long,
  text: String,
  title: String,
  exampleId: Long,
  score: ScoredWord,
  sentence: Sentence,
  year: Int,
  provider: ContentProvider,
  word: String,
  rating: Float,
  url: String,
  documentId: Long)

