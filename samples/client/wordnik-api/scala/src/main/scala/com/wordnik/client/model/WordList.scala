package com.wordnik.client.model

import java.util.Date
case class WordList (
  id: Long,
  updatedAt: Date,
  username: String,
  permalink: String,
  description: String,
  createdAt: Date,
  lastActivityAt: Date,
  name: String,
  userId: Long,
  numberWordsInList: Long,
  `type`: String)

