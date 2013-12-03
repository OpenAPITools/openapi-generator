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

import com.wordnik.swagger.codegen.BasicJavaGenerator

import com.wordnik.swagger.model._

object BasicAndroidJavaClient extends BasicAndroidJavaGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicAndroidJavaGenerator extends BasicJavaGenerator {
  override def typeMapping = super.typeMapping ++ Map(
    "file" -> "File")

  override def importMapping = super.importMapping ++ Map(
    "Set" -> "java.util.Set")

  override def defaultIncludes = Set(
    "Integer",
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

  // package for api invoker, error files
  override def invokerPackage = Some("com.wordnik.client")

  override def templateDir = "android-java"

  // where to write generated code
  override def destinationDir = "generated-code/android-java/src/main/java"

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles = List(
    ("apiInvoker.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "ApiInvoker.java"),
    ("jsonUtil.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "JsonUtil.java"),
    ("ApiException.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "ApiException.java"),
    ("pom.mustache", "android-java", "pom.xml")
  )
}