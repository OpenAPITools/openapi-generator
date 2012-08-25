package com.wordnik.petstore.model

import scala.reflect.BeanProperty

class User {
  @BeanProperty var id: Long = _
  @BeanProperty var lastName: String = _
  @BeanProperty var username: String = _
  @BeanProperty var phone: String = _
  @BeanProperty var email: String = _
  /* User Status */
  @BeanProperty var userStatus: Int = _
  @BeanProperty var firstName: String = _
  @BeanProperty var password: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class User {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  lastName: ").append(lastName).append("\n")
    sb.append("  username: ").append(username).append("\n")
    sb.append("  phone: ").append(phone).append("\n")
    sb.append("  email: ").append(email).append("\n")
    sb.append("  userStatus: ").append(userStatus).append("\n")
    sb.append("  firstName: ").append(firstName).append("\n")
    sb.append("  password: ").append(password).append("\n")
    sb.append("}\n")
    sb.toString
  }
}

