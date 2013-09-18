/**
 *  Copyright 2013 Wordnik, Inc.
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

object BasicScalaGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicScalaGenerator extends BasicGenerator {
  override def defaultIncludes = Set(
    "Int",
    "String",
    "Long",
    "Short",
    "Char",
    "Byte",
    "Float",
    "Double",
    "Boolean",
    "AnyRef",
    "Any")

  override def typeMapping = Map(
    "array" -> "List",
    "set" -> "Set",
    "boolean" -> "Boolean",
    "string" -> "String",
    "int" -> "Int",
    "long" -> "Long",
    "float" -> "Float",
    "byte" -> "Byte",
    "short" -> "Short",
    "char" -> "Char",
    "long" -> "Long",
    "double" -> "Double",
    "object" -> "Any",
    "file" -> "File")

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".scala"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".scala"

  // location of templates
  override def templateDir = "scala"

  // where to write generated code
  override def destinationDir = "generated-code/scala/src/main/scala"

  // reserved words which need special quoting
  override def reservedWords =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield")

  // import/require statements for specific datatypes
  override def importMapping = Map(
    "Date" -> "java.util.Date",
    "File" -> "java.io.File"
  )

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

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => {
        val ComplexTypeMatcher = "(.*)\\[(.*)\\].*".r
        val t = e match {
          case ComplexTypeMatcher(container, inner) => {
            e.replaceAll(container, typeMapping.getOrElse(container.toLowerCase, container))
          }
          case _ => e
        }
        Some(typeMapping.getOrElse(t, t))
      }
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = dt.indexOf("[") match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n) == "Array")
          "List" + dt.substring(n)
        else if (dt.substring(0, n) == "Set")
          "Set" + dt.substring(n)
        else dt
      }
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: ModelProperty): (String, String) = {
    obj.`type` match {
      case "Array" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        val e = "List[%s]".format(toDeclaredType(inner))
        (e, toDefaultValue(inner, obj))
      }
      case "List" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        val e = "List[%s]".format(toDeclaredType(inner))
        (e, toDefaultValue(inner, obj))
      }
      case "Set" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        val e = "Set[%s]".format(toDeclaredType(inner))
        (e, toDefaultValue(inner, obj))
      }
      case e: String => (toDeclaredType(e), toDefaultValue(e, obj))
    }
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationDir + "/com/wordnik/client", "ApiInvoker.scala"),
    ("pom.mustache", "generated-code/scala", "pom.xml"))
}
