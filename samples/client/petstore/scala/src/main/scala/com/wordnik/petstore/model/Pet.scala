package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
case class Pet (
  /* Unique identifier for the Pet */
  id: Long,
  /* Category the pet is in */
  category: Category,
  /* Friendly name of the pet */
  name: String,
  /* Image URLs */
  photoUrls: List[String],
  /* Tags assigned to this pet */
  tags: List[Tag],
  /* pet status in the store */
  status: String)

