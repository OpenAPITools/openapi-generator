package io.swagger.client.model

import io.swagger.client.model.Category
import io.swagger.client.model.Tag



case class Pet (
  id: Long,
  category: Category,
  name: String,
  photoUrls: List[String],
  tags: List[Tag],
  /* pet status in the store */
  status: String)
  
