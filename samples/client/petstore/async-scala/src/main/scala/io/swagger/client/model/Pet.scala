package io.swagger.client.model

import org.joda.time.DateTime
import java.util.UUID


case class Pet (
  id: Option[Long],
category: Option[Category],
name: String,
photoUrls: List[String],
tags: Option[List[Tag]],
status: Option[String]  // pet status in the store
)
