package io.swagger.client.model

import io.swagger.client.model.Sentence
import io.swagger.client.model.ContentProvider
import io.swagger.client.model.ScoredWord



case class Example (
  id: Long,
  exampleId: Long,
  title: String,
  text: String,
  score: ScoredWord,
  sentence: Sentence,
  word: String,
  provider: ContentProvider,
  year: Integer,
  rating: Float,
  documentId: Long,
  url: String)
  
