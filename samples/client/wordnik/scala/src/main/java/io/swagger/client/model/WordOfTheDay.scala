package io.swagger.client.model

import io.swagger.client.model.SimpleExample
import io.swagger.client.model.ContentProvider
import io.swagger.client.model.SimpleDefinition
import org.joda.time.DateTime



case class WordOfTheDay (
  id: Long,
  parentId: String,
  category: String,
  createdBy: String,
  createdAt: DateTime,
  contentProvider: ContentProvider,
  htmlExtra: String,
  word: String,
  definitions: List[SimpleDefinition],
  examples: List[SimpleExample],
  note: String,
  publishDate: DateTime)
  
