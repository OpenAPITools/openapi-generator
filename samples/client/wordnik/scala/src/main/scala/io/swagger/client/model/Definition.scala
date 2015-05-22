package io.swagger.client.model

import io.swagger.client.model.ExampleUsage
import io.swagger.client.model.Note
import io.swagger.client.model.Label
import io.swagger.client.model.Related
import io.swagger.client.model.Citation
import io.swagger.client.model.TextPron



case class Definition (
  extendedText: String,
  text: String,
  sourceDictionary: String,
  citations: List[Citation],
  labels: List[Label],
  score: Float,
  exampleUses: List[ExampleUsage],
  attributionUrl: String,
  seqString: String,
  attributionText: String,
  relatedWords: List[Related],
  sequence: String,
  word: String,
  notes: List[Note],
  textProns: List[TextPron],
  partOfSpeech: String)
  
