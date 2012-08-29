package com.wordnik.client.model

import com.wordnik.client.model.Category
import com.wordnik.client.model.Tag
import scala.reflect.BeanProperty

case class Pet (
  var id: Long,
  var tags: java.util.List[Tag],
  var category: Category,
  /* pet status in the store */
  var status: String,
  var name: String,
  var photoUrls: java.util.List[String])

