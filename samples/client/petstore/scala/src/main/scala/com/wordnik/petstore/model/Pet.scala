package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
case class Pet (
  tags: List[Tag],
  id: Long,
  category: Category,
  /* pet status in the store */
  status: String,
  name: String,
  photoUrls: List[String])

