package com.wordnik.client.model

import com.wordnik.client.model.ExampleUsage
import com.wordnik.client.model.Note
import com.wordnik.client.model.Citation
import com.wordnik.client.model.TextPron
import com.wordnik.client.model.Label
import com.wordnik.client.model.Related
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

