package com.wordnik.swagger.codegen

import com.wordnik.swagger.core._

object BasicScalaGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicScalaGenerator extends BasicGenerator {
  override def defaultIncludes = Set("Int",
    "String", 
    "Long",
    "Float",
    "Double",
    "Boolean",
    "Any")

  override def typeMapping = Map(
    "string" -> "String",
    "int" -> "Int",
    "float" -> "Float",
    "long" -> "Long",
    "double" -> "Double",
    "object" -> "Any")
    
  // template used for models
  modelTemplateFiles += "model.mustache" -> ".scala"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".scala"

  // location of templates
  override def templateDir = "scala"

  // where to write generated code
  override def destinationDir = "generated-code/scala/src/main/scala"

  // reserved words which need special quoting
  override def reservedWords = Set("type", "package", "match", "object")

  // import/require statements for specific datatypes
  override def importMapping = Map("Date" -> "java.util.Date")

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // response classes--if you don't want a response class, override and set to None
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(typeMapping.getOrElse(e, e))
    }
  }

  override def toDeclaration(obj: DocumentationSchema): (String, String) = {
    val datatype = (obj.getType.charAt(0).toUpperCase + obj.getType.substring(1)) match {
      case "Array" => {
        "java.util.List[%s]" format toDeclaredType(
	  if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
	)
      }
      case e: String => e
    } 
    (datatype, toDefaultValue(datatype, obj))
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationDir + "com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", "generated-code/scala", "pom.xml"))
}
