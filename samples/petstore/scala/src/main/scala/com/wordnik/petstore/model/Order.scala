package com.wordnik.petstore.model

import java.util.Date
import scala.reflect.BeanProperty

class Order {
  @BeanProperty var id: Long = _
  @BeanProperty var petId: Long = _
  /* Order Status */
  @BeanProperty var status: String = _
  @BeanProperty var quantity: Int = _
  @BeanProperty var shipDate: Date = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Order {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  petId: ").append(petId).append("\n")
    sb.append("  status: ").append(status).append("\n")
    sb.append("  quantity: ").append(quantity).append("\n")
    sb.append("  shipDate: ").append(shipDate).append("\n")
    sb.append("}\n")
    sb.toString
  }
}

