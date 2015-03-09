package io.swagger.client.model

import org.joda.time.DateTime



case class WordListWord (
  id: Long,
  word: String,
  username: String,
  userId: Long,
  createdAt: DateTime,
  numberCommentsOnWord: Long,
  numberLists: Long)
  
