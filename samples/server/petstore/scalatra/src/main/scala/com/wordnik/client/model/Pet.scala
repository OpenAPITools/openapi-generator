package com.wordnik.client.model

import com.wordnik.client.model.Category
import com.wordnik.client.model.Tag


case class Pet (
  id: Long,
  category: Category,
  name: String,
  photoUrls: List[String],
  tags: List[Tag],
  status: String
)
