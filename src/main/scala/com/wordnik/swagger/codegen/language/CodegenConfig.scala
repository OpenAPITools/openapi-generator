package com.wordnik.swagger.codegen.language

import com.wordnik.swagger.core._

import scala.collection.mutable.{ HashMap, HashSet }

abstract class CodegenConfig {
  def packageName: String
  def templateDir: String
  def destinationDir: String
  val apiTemplateFiles = new HashMap[String, String]()
  val modelTemplateFiles = new HashMap[String, String]()

  def defaultIncludes = Set[String]()
  def languageSpecificPrimitives = Set[String]()
  def typeMapping = Map[String, String]()

  // optional configs
  def apiPackage: Option[String] = None
  def reservedWords: Set[String] = Set()

  // swagger primitive types
  def importMapping: Map[String, String] = Map()
  def modelPackage: Option[String] = None
  def escapeReservedWord(word: String) = word

  def apiNameFromPath(apiPath: String): String

  // only process these apis (by name)
  val apisToProcess = new HashSet[String]

  // only process these models
  val modelsToProcess = new HashSet[String]

  // method name from operation.nickname
  def toMethodName(name: String): String = name

  // override if you want to do something special on processing
  def processOperation(apiPath: String, op: DocumentationOperation) = {}
  def processResponseClass(responseClass: String): Option[String] = Some(responseClass)

  def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(typeMapping.getOrElse(e, e))
    }
  }

  def supportingFiles = List(): List[(String, String, String)]

  // mapping for datatypes
  def toDeclaration(obj: DocumentationSchema) = {
    var declaredType = toDeclaredType(obj.getType)
    val defaultValue = toDefaultValue(declaredType, obj)
    (declaredType, defaultValue)
  }

  def toDeclaredType(dataType: String): String = {
    typeMapping.getOrElse(dataType, dataType)
  }

  def toGetter(name: String, datatype: String) = {
    val base = datatype match {
      case "boolean" => "is"
      case _ => "get"
    }
    base + name.charAt(0).toUpperCase + name.substring(1)
  }

  def toSetter(name: String, datatype: String) = {
    val base = datatype match {
      case _ => "set"
    }
    base + name.charAt(0).toUpperCase + name.substring(1)
  }

  def toVarName(name: String): String = {
    name match {
      case _ if (reservedWords.contains(name)) => escapeReservedWord(name)
      case _ => typeMapping.getOrElse(name, name)
    }
  }

  def toDefaultValue(datatype: String, defaultValue: String): Option[String] = {

    if (defaultValue != "" && defaultValue != null) {
      toDeclaredType(datatype) match {
        case "int" => Some(defaultValue)
        case "long" => Some(defaultValue)
        case "double" => Some(defaultValue)
        case x if x == "string" || x == "String" => {
          defaultValue match {
            case e: String => Some("\"" + defaultValue + "\"")
            case _ => None
          }
        }
        case _ => None
      }
    } else None
  }

  def toDefaultValue(properCase: String, obj: DocumentationSchema) = {
    properCase match {
      case "int" => "0"
      case "long" => "0L"
      case "double" => "0.0"
      case e: String if (Set("List").contains(e)) => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        "new java.util.ArrayList[" + toDeclaredType(inner) + "]" + "()"
      }
      case _ => "_"
    }
  }
}
