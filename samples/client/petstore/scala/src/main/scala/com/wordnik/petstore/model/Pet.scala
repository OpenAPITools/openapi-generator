package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
case class Pet (
  name: String,
  id: Long,
  tags: List[Tag],
  /* pet status in the store */
  status: String,
  photoUrls: List[String],
  category: Category)

