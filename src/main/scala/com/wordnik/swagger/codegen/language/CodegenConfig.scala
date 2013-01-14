/**
 *  Copyright 2012 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen.language

import com.wordnik.swagger.model._

import scala.collection.mutable.{ HashMap, HashSet }

abstract class CodegenConfig {
  /*
   * abstract methods
   */
  def packageName: String
  def templateDir: String
  def destinationDir: String
  def toModelName(name: String): String
  def toApiName(name: String): String

  
  def toModelFilename(name: String) = name
  def toApiFilename(name: String) = toApiName(name)
  def apiNameFromPath(apiPath: String): String
  def processApiMap(m: Map[String, AnyRef]): Map[String, AnyRef] = m
  def processModelMap(m: Map[String, AnyRef]): Map[String, AnyRef] = m

  val apiTemplateFiles = new HashMap[String, String]()
  val modelTemplateFiles = new HashMap[String, String]()

  def defaultIncludes = Set[String]()
  def languageSpecificPrimitives = Set[String]()
  def typeMapping = Map[String, String]()

  // optional configs
  def invokerPackage: Option[String] = None
  def apiPackage: Option[String] = None
  def modelPackage: Option[String] = None

  def reservedWords: Set[String] = Set()

  // swagger primitive types
  def importMapping: Map[String, String] = Map()
  def escapeReservedWord(word: String) = word

  // only process these apis (by name)
  val apisToProcess = new HashSet[String]

  // only process these models
  val modelsToProcess = new HashSet[String]

  // method name from operation.nickname
  def toMethodName(name: String): String = name

  // override if you want to do something special on processing
  // def processOperation(apiPath: String, op: DocumentationOperation) = {}
  def processOperation(apiPath: String, op: Operation) = {}

  def processResponseClass(responseClass: String): Option[String] = Some(responseClass)

  def processApiOperation(apiPath: String, op: Operation) = {}
  def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(typeMapping.getOrElse(e, e))
    }
  }

  def supportingFiles = List(): List[(String, String, String)]

  // mapping for datatypes
  def toDeclaration(property: ModelProperty) = {
    var declaredType = toDeclaredType(property.`type`)
    val defaultValue = toDefaultValue(declaredType, property)
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
    base + name(0).toUpper + name.substring(1)
  }

  def toSetter(name: String, datatype: String) = {
    val base = datatype match {
      case _ => "set"
    }
    base + name(0).toUpper + name.substring(1)
  }

  def toVarName(name: String): String = {
    name match {
      case _ if (reservedWords.contains(name)) => escapeReservedWord(name)
      case _ => typeMapping.getOrElse(name, name)
    }
  }

  def toDefaultValue(datatype: String, v: String): Option[String] = {
    if (v != "" && v != null) {
      datatype match {
        case "int" => Some(v)
        case "long" => Some(v)
        case "double" => Some(v)
        case x if x == "string" || x == "String" => {
          v match {
            case e: String => Some("\"" + v + "\"")
            case _ => None
          }
        }
        case _ => None
      }
    } else None
  }

  def toDefaultValue(dataType: String, obj: ModelProperty) = {
    dataType match {
      case "int" => "0"
      case "long" => "0L"
      case "float" => "0f"
      case "double" => "0.0"
      case e: String if (Set("List").contains(e)) => {
        val inner = {
          obj.items match {
            case Some(items) => {
              if(items.ref != None) 
                items.ref.get
              else
                items.`type`
            }
            case _ => throw new Exception("no inner type defined")
          }
        }
        "new java.util.ArrayList[" + toDeclaredType(inner) + "]" + "()"
      }
      case _ => "_"
    }
  }
}
