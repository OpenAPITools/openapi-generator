package io.swagger.client.model

import org.joda.time.DateTime



case class WordList (
  id: Long,
  permalink: String,
  name: String,
  createdAt: DateTime,
  updatedAt: DateTime,
  lastActivityAt: DateTime,
  username: String,
  userId: Long,
  description: String,
  numberWordsInList: Long,
  _type: String)
  
