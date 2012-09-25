package com.wordnik.client.model

case class Note (
  noteType: String,
  appliesTo: List[String],
  value: String,
  pos: Int)

