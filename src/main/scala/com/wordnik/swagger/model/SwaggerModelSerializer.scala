package com.wordnik.swagger.model

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.native.Serialization.{read, write}

class ModelPropertySerializer extends CustomSerializer[ModelProperty] (formats => ({
  case json =>
    implicit val fmts: Formats = formats
    ModelProperty(
      `type` = (json \ "type").extractOrElse(""),
      required = ((json \ "required").extractOrElse("false")).toBoolean,
      description = (json \ "description").extractOpt[String],
      allowableValues = (json \ "allowableValues").extract[AllowableValuesFoo],
      items = (json \ "items").extractOpt[ModelRef]
    )
}, {
  case x: ModelProperty =>
  implicit val fmts = formats
  ("type" -> x.`type`) ~
  ("required" -> x.required) ~
  ("description" -> x.description) ~
  ("allowableValues" -> {
    x.allowableValues match {
      case Any => JNothing // don't serialize when not a concrete type
      case e:AllowableValuesFoo => Extraction.decompose(x.allowableValues)
      case _ => JNothing
    }
  }) ~
  ("items" -> Extraction.decompose(x.items))
}))

class ModelRefSerializer extends CustomSerializer[ModelRef](formats => ({
  case json =>
    implicit val fmts: Formats = formats
    ModelRef(
      (json \ "$ref").extract[String],
      (json \ "type").extract[String]
    )
}, {
  case x: ModelRef =>
  implicit val fmts = formats
  ("$ref" -> x.ref) ~
  ("type" -> x.`type`)
}))

class AllowableValuesSerializer extends CustomSerializer[AllowableValuesFoo](formats => ({
  case json =>
    implicit val fmts: Formats = formats
    json \ "valueType" match {
      case JString(x) if x.equalsIgnoreCase("list") =>
        AllowableListValues((json \ "values").extract[List[String]])
      case JString(x) if x.equalsIgnoreCase("range") =>
        AllowableRangeValues((json \ "min").extract[String], (json \ "max").extract[String])
      case _ => Any
    }
}, {
  case AllowableListValues(values, "LIST") => 
    implicit val fmts = formats
    ("valueType" -> "LIST") ~ ("values" -> Extraction.decompose(values))
  case AllowableRangeValues(min, max)  => 
    ("valueType" -> "RANGE") ~ ("min" -> min) ~ ("max" -> max)
}))
