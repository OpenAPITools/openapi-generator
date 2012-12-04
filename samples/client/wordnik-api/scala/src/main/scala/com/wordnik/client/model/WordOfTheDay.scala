package com.wordnik.client.model

import java.util.Date
import com.wordnik.client.model.SimpleDefinition
import com.wordnik.client.model.SimpleExample
import com.wordnik.client.model.ContentProvider
case class WordOfTheDay (
  id: Long,
  parentId: String,
  category: String,
  createdBy: String,
  createdAt: Date,
  contentProvider: ContentProvider,
  htmlExtra: String,
  word: String,
  definitions: List[SimpleDefinition],
  examples: List[SimpleExample],
  note: String,
  publishDate: Date)

