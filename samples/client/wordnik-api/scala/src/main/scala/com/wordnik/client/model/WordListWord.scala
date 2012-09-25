package com.wordnik.client.model

import java.util.Date
case class WordListWord (
  id: Long,
  username: String,
  createdAt: Date,
  userId: Long,
  numberCommentsOnWord: Long,
  word: String,
  numberLists: Long)

