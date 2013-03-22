package com.wordnik.client.model

import com.wordnik.client.model.Category
import com.wordnik.client.model.Tag
case class Pet (
  tags: Option[List[Tag]],
  id: Option[Long],
  category: Option[Category],
  status: Option[String],// pet status in the store
  name: Option[String],
  photoUrls: Option[List[String]]
)

