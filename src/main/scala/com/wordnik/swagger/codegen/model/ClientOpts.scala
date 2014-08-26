package com.wordnik.swagger.codegen.model

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class ClientOpts(
  @BeanProperty var uri: String,
  @BeanProperty var auth: Option[ApiKeyValue],
  @BeanProperty var properties: java.util.Map[String, String]) {
  def this() = this(null, None, new java.util.HashMap[String, String]())

  @BeanProperty var outputDirectory: String = _

  override def toString() = {
    val sb = new StringBuilder()
    sb.append("ClientOpts: {\n")
    sb.append("  uri: ").append(uri).append(",")
    sb.append("  auth: ").append(auth).append(",")
    sb.append(properties.asScala.mkString("  ", ",", "\n"))
    sb.append("}")
    sb.toString
  }
}
