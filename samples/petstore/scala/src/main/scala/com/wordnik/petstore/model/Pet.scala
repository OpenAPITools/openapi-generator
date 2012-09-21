package com.wordnik.petstore.model

import com.wordnik.petstore.model.Category
import com.wordnik.petstore.model.Tag
import scala.reflect.BeanProperty

class Pet {
  @BeanProperty var id: Long = 0L
  @BeanProperty var tags: java.util.List[Tag] = _
  @BeanProperty var category: Category = _
  /* pet status in the store */
  @BeanProperty var status: String = _
  @BeanProperty var name: String = _
  @BeanProperty var photoUrls: java.util.List[String] = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Pet {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  tags: ").append(tags).append("\n")
    sb.append("  category: ").append(category).append("\n")
    sb.append("  status: ").append(status).append("\n")
    sb.append("  name: ").append(name).append("\n")
    sb.append("  photoUrls: ").append(photoUrls).append("\n")
    sb.append("}\n")
    sb.toString
  }
}

