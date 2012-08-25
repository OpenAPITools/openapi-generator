package com.wordnik.petstore.model

import scala.reflect.BeanProperty

class Tag {
  @BeanProperty var id: Long = _
  @BeanProperty var name: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Tag {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  name: ").append(name).append("\n")
    sb.append("}\n")
    sb.toString
  }
}

