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

import com.wordnik.swagger.core._

object BasicJavaGenerator extends BasicJavaGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicJavaGenerator extends BasicGenerator {
  override def defaultIncludes = Set(
    "double",
    "int",
    "long",
    "float",
    "String",
    "boolean")

  override def typeMapping = Map(
    "string" -> "String",
    "int" -> "Integer",
    "float" -> "Float",
    "long" -> "Long",
    "double" -> "Double",
    "object" -> "Object")

  // location of templates
  override def templateDir = "Java"

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".java"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".java"

  // where to write generated code
  override def destinationDir = "src/test/java"

  override def reservedWords = Set("abstract", "continue", "for", "new", "switch", "assert", 
      "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", 
      "this", "break", "double", "implements", "protected", "throw", "byte", "else", 
      "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", 
      "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", 
      "void", "class", "finally", "long", "strictfp", "volatile", "const", "float", 
      "native", "super", "while")

  // import/require statements for specific datatypes
  override def importMapping = Map(
    "Date" -> "java.util.Date",
    "Array" -> "java.util.*",
    "ArrayList" -> "java.util.*",
    "List" -> "java.util.List")

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // file suffix
  override def fileSuffix = ".java"

  // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(typeMapping.getOrElse(e, e.replaceAll("\\[", "<").replaceAll("\\]", ">")))
    }
  }

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(typeMapping.getOrElse(e, e.replaceAll("\\[", "<").replaceAll("\\]", ">")))
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = dt.indexOf("[") match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n) == "Array") {
          "List" + dt.substring(n).replaceAll("\\[", "<").replaceAll("\\]", ">")
        } 
        else dt.replaceAll("\\[", "<").replaceAll("\\]", ">")
      }
      case _ => dt
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: DocumentationSchema) = {
    var declaredType = toDeclaredType(obj.getType)

    declaredType match {
      case "Array" => {
        declaredType = "List"
      }
      case e: String => e
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "List" => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        declaredType += "<" + toDeclaredType(inner) + ">"
      }
      case _ =>
    }
    (declaredType, defaultValue)
  }

  // default values
  override def toDefaultValue(dataType: String, obj: DocumentationSchema) = {
    dataType match {
      case "boolean" => "false"
      case "int" => "0"
      case "long" => "0L"
      case "float" => "0.0f"
      case "double" => "0.0"
      case "List" => {
        val inner = {
          if (obj.items.ref != null) obj.items.ref
          else obj.items.getType
        }
        "new ArrayList<" + toDeclaredType(inner) + ">" + "()"
      }
      case _ => "null"
    }
  }

  override def escapeReservedWord(word: String) = {
    if (reservedWords.contains(word)) 
      throw new Exception("reserved word " + "\"" + word + "\" not allowed")
    else word
  }
}