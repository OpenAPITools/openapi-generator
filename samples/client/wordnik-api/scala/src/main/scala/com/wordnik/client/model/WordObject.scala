package com.wordnik.client.model

case class WordObject (
  id: Long,
  originalWord: String,
  word: String,
  suggestions: List[String],
  canonicalForm: String,
  vulgar: String)

