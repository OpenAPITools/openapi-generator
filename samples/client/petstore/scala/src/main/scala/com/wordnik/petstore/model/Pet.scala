package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
case class Pet (
  /* unique identifier for the pet */
  id: Long,
  category: Category,
  name: String,
  photoUrls: List[String],
  tags: List[Tag],
  /* pet status in the store */
  status: String)

