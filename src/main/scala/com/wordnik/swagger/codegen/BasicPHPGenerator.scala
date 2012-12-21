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

package com.wordnik.swagger.codegen

import com.wordnik.swagger.model._

import java.io.File

object BasicPHPGenerator extends BasicPHPGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicPHPGenerator extends BasicGenerator {
  // template used for models
  modelTemplateFiles += "model.mustache" -> ".php"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".php"

  // location of templates
  override def templateDir = "php"

  // where to write generated code
  override def destinationDir = "generated-code/php"

  // package for models
  override def modelPackage = Some("models")

  // package for apis
  override def apiPackage = Some("")

  // file suffix
  override def fileSuffix = ".php"

  // reserved words which need special quoting
  // These will all be object properties, in which context we don't need
  // to worry about escaping them for PHP.
  override def reservedWords = Set()

  // import/require statements for specific datatypes
  override def importMapping = Map()


 // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    typeMapping.contains(responseClass) match {
      case true => Some(typeMapping(responseClass))
      case false => {
        responseClass match {
          case "void" => None
          case e: String => {
            responseClass.startsWith("List") match {
              case true => Some("array")
              case false => Some(responseClass)
            }
          }
        }
      }
    }
  }


  override def processResponseDeclaration(responseClass: String): Option[String] = {
    typeMapping.contains(responseClass) match {
      case true => Some(typeMapping(responseClass))
      case false => {
        responseClass match {
          case "void" => None
          case e: String => {
            responseClass.startsWith("List") match {
              case true => {
                val responseSubClass = responseClass.dropRight(1).substring(5)
                typeMapping.contains(responseSubClass) match {
                  case true => Some("array[" + typeMapping(responseSubClass) + "]")
                  case false => Some("array[" + responseSubClass + "]")
                }
              }
              case false => Some(responseClass)
            }
          }
        }
      }
    }
  }
  override def typeMapping = Map(
    "string" -> "string",
    "str" -> "string",
    "int" -> "int",
    "float" -> "float",
    "long" -> "int",
    "double" -> "float",
    "Array" -> "array",
    "boolean" -> "bool",
    "Date" -> "DateTime"
    )

  override def toDeclaredType(dt: String): String = {
    val declaredType = typeMapping.getOrElse(dt, dt)
    declaredType.startsWith("Array") match {
      case true => {
        val innerType = dt.dropRight(1).substring(6)
        typeMapping.contains(innerType) match {
          case true => "array[" + typeMapping(innerType) + "]"
          case false => "array[" + innerType + "]"
        }
      }
      case _ => declaredType
    }
  }

  override def toDeclaration(obj: ModelProperty) = {
    var declaredType = toDeclaredType(obj.`type`)

    declaredType match {
      case "Array" => declaredType = "array"
      case e: String => {
        e
      }
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "array" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => throw new Exception("no inner type defined")
          }
        }
        declaredType += "[" + toDeclaredType(inner) + "]"
        "array"
      }
      case _ =>
    }
    (declaredType, defaultValue)
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

  // supporting classes
  override def supportingFiles = List(
    ("Swagger.mustache", destinationDir + File.separator + apiPackage.get,
     "Swagger.php")
  )
}
