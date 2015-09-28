package io.swagger.client.model

import org.joda.time.DateTime


case class Pet (
  id: Long,
  category: Category,
  name: String,
  photoUrls: List[String],
  tags: List[Tag],
  status: String  // pet status in the store
  
)
