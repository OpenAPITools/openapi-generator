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

object BasicCSharpGenerator extends BasicCSharpGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicCSharpGenerator extends BasicGenerator {
  override def defaultIncludes = Set(
    "double",
    "int",
    "long",
    "float",
    "String",
    "boolean",
    "Boolean",
    "Double",
    "Integer",
    "Long",
    "Float")

  /**
   * We are using csharp objects instead of primitives to avoid showing default
   * primitive values when the API returns missing data.  For instance, having a
   * {"count":0} != count is unknown.  You can change this to use primitives if you
   * desire, but update the default values as well or they'll be set to null in 
   * variable declarations.
   */
  override def typeMapping = Map(
    "boolean" -> "bool",
    "string" -> "string",
    "int" -> "int",
    "float" -> "float",
    "long" -> "long",
    "double" -> "double",
    "object" -> "object",
    "Date" -> "DateTime",
    "date" -> "DateTime")

  // location of templates
  override def templateDir = "csharp"

  // where to write generated code
  override def destinationDir = "generated-code/csharp/src/main/csharp"

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".cs"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".cs"

  override def reservedWords = Set("abstract", "continue", "for", "new", "switch", "assert", 
      "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", 
      "this", "break", "double", "implements", "protected", "throw", "byte", "else", 
      "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", 
      "catch", "extends", "short", "try", "char", "final", "interface", "static", 
      "void", "class", "finally", "strictfp", "volatile", "const", "float", 
      "native", "super", "while")

  // import/require statements for specific datatypes
  override def importMapping = Map()

  // package for models
  override def modelPackage = Some("Com.Wordnik.Client.Model")

  // package for api classes
  override def apiPackage = Some("Com.Wordnik.Client.Api")

  // file suffix
  override def fileSuffix = ".cs"

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
        if (dt.substring(0, n) == "Array")
          "List" + dt.substring(n).replaceAll("\\[", "<").replaceAll("\\]", ">")
        else dt.replaceAll("\\[", "<").replaceAll("\\]", ">")
      }
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: ModelProperty) = {
    var declaredType = toDeclaredType(obj.`type`)

    declaredType match {
      case "Array" => declaredType = "List"
      case e: String => e
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "List" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => throw new Exception("no inner type defined")
          }
        }
        declaredType += "<" + toDeclaredType(inner) + ">"
      }
      case _ =>
    }
    (declaredType, defaultValue)
  }

  /**
   * we are defaulting to null values since the codegen uses csharp objects instead of primitives
   * If you change to primitives, you can put in the appropriate values (0.0f, etc).
   */
  override def toDefaultValue(dataType: String, obj: ModelProperty) = {
    dataType match {
      case "Boolean" => "null"
      case "Integer" => "null"
      case "Long" => "null"
      case "Float" => "null"
      case "Double" => "null"
      case "List" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => throw new Exception("no inner type defined")
          }
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

  /*override def toVarName(name: String): String = {
    name match {
      case _ if (reservedWords.contains(name)) => escapeReservedWord(name)
      case _ => typeMapping.getOrElse(name, name)
    }
    capitalize(name)
  }

  def capitalize(s: String) = { 
    s(0).toUpper + s.substring(1, s.length).toLowerCase 
  }*/

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "ApiInvoker.java"),
      ("apiException.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "ApiException.java"),
      ("Newtonsoft.Json.dll", "generated-code/csharp/bin", "Newtonsoft.Json.dll"),
      ("compile.mustache", "generated-code/csharp", "compile.bat"))
}