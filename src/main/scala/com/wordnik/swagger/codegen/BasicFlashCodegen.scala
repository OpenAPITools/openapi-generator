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

abstract class BasicFlashCodegen extends BasicGenerator {
  override def defaultIncludes = Set(
    "Date",
    "String",
    "Boolean",
    "Number")

  override def typeMapping = Map(
    "boolean" -> "Boolean",
    "string" -> "String",
    "int" -> "Number",
    "float" -> "Number",
    "long" -> "Number",
    "double" -> "Number")

  override def packageName = "com.wordnik.client"

  // location of templates
  override def templateDir = "flash"

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".as"
  modelTemplateFiles += "modelList.mustache" -> "List.as"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".as"

  // where to write generated code
  override def destinationDir = "src/test/flash"

  // import/require statements for specific datatypes
  override def importMapping = Map()


  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // file suffix
  override def fileSuffix = ".as"

  override def toVarName(name: String): String = {
    name.substring(0, 1).toLowerCase + name.substring(1, name.length)
  }

  // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => Some(e)
    }
  }

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None
      case e: String => {
        responseClass.startsWith("List") match {
          case true => {
            val responseSubClass = responseClass.dropRight(1).substring(5)
            typeMapping.contains(responseSubClass) match {
              case true => Some("Array")
              case false => Some(packageName + ".model." +
                responseSubClass + "List")
            }
          }
          case false => Some(responseClass)
        }
      }
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = dt.indexOf("[") match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n) == "Array") {
          "Array"
        } else if (dt.substring(0, n) == "List") {
          "Array"
        } else dt
      }
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: ModelProperty) = {
    var declaredType = toDeclaredType(obj.`type`)

    declaredType match {
      case "Array" => {
        declaredType = "Array"
      }
      case "List" => {
        declaredType = "Array"
      }
      case e: String => e
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "List" => "Array"
      case _ =>
    }
    (declaredType, defaultValue)
  }

  override def toDefaultValue(dataType: String, obj: ModelProperty) = {
    dataType match {
      case "Boolean" => "false"
      case "Number" => "0.0"
      case "List" => "new Array()"
      case "Array" => "new Array()"
      case _ => "null"
    }
  }

  def destinationRoot: String

  // supporting classes
  def baseSupportingFiles = List(
    ("ApiInvoker.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiInvoker.as"),
    ("ApiUrlHelper.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiUrlHelper.as"),
    ("ApiUserCredentials.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ApiUserCredentials.as"),
    ("ListWrapper.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "ListWrapper.as"),
    ("SwaggerApi.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "SwaggerApi.as"),
    ("XMLWriter.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/common", "XMLWriter.as"),

    ("ApiError.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/exception", "ApiError.as"),
    ("ApiErrorCodes.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/exception", "ApiErrorCodes.as"),

    ("ApiClientEvent.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/event", "ApiClientEvent.as"),
    ("Response.as", destinationRoot + "/src/main/flex/com/wordnik/swagger/event", "Response.as"),

    ("build.properties", destinationRoot, "build.properties"),
    ("build.xml", destinationRoot, "build.xml"),
    ("AirExecutorApp-app.xml", destinationRoot + "/bin", "AirExecutorApp-app.xml"),

    ("ASAXB-0.1.1.swc", destinationRoot + "/lib", "ASAXB-0.1.1.swc"),
    ("as3corelib.swc", destinationRoot + "/lib/ext", "as3corelib.swc"),
    ("flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc", destinationRoot + "/lib/ext", "flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc"),
    ("flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc", destinationRoot + "/lib/ext", "flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc"),
    ("flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc", destinationRoot + "/lib/ext", "flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc"),
    ("flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc", destinationRoot + "/lib/ext", "flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc")
  )
}