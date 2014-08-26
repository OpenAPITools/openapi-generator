package com.wordnik.swagger.util

import com.wordnik.swagger.codegen.model._

import scala.collection.JavaConverters._
import scala.beans.BeanProperty

class ValidationException(code:Int, msg:String, errors: List[ValidationError]) extends Exception(msg:String) {
  val messages: java.util.List[ValidationMessage] = (
    for(e <- errors) yield ({
      val m = new ValidationMessage()
      m.path = e.path
      m.message = e.message
      m.severity = e.severity
      m
    })
  ).toList.asJava
}

class ValidationMessage() {
  @BeanProperty var path: String = _
  @BeanProperty var message: String = _
  @BeanProperty var severity: String = _
}