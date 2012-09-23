package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
case class Pet (
  id: Long,
  tags: List[Tag],
  category: Category,
  /* pet status in the store */
  status: String,
  name: String,
  photoUrls: List[String])

