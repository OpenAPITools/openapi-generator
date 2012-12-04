package com.wordnik.client.model

import java.util.Date
case class WordListWord (
  id: Long,
  word: String,
  username: String,
  userId: Long,
  createdAt: Date,
  numberCommentsOnWord: Long,
  numberLists: Long)

