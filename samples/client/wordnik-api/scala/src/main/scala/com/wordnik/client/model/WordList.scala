package com.wordnik.client.model

import java.util.Date
case class WordList (
  id: Long,
  permalink: String,
  name: String,
  createdAt: Date,
  updatedAt: Date,
  lastActivityAt: Date,
  username: String,
  userId: Long,
  description: String,
  numberWordsInList: Long,
  `type`: String)

