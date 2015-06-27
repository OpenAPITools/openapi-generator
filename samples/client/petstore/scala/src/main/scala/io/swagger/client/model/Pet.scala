package io.swagger.client.model




case class Pet (
  id: Long,
  category: Category,
  name: String,
  photoUrls: List[String],
  tags: List[Tag],
  /* pet status in the store */
  status: String)
  
