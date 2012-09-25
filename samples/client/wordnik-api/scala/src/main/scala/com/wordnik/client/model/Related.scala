package com.wordnik.client.model

case class Related (
  label1: String,
  label2: String,
  relationshipType: String,
  label3: String,
  words: List[String],
  label4: String,
  gram: String)

