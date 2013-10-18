package com.wordnik.client.model

import com.wordnik.client.model.Sentence
import com.wordnik.client.model.ContentProvider
import com.wordnik.client.model.ScoredWord
case class Example (
  id: Long,
  exampleId: Long,
  title: String,
  text: String,
  score: ScoredWord,
  sentence: Sentence,
  word: String,
  provider: ContentProvider,
  year: Int,
  rating: Float,
  documentId: Long,
  url: String)

