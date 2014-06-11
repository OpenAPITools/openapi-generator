/**
 *  Copyright 2014 Wordnik, Inc.
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

import com.wordnik.swagger.codegen.BasicScalaGenerator

import scala.collection.mutable.{ HashMap, ListBuffer }

object NodeServerGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)
  val codeDir = "app"
  val apiDir = "apis"

  override def templateDir = "samples/server-generator/node/templates"

  val outputFolder = "samples/server-generator/node/output"
    
  // where to write generated code
  override def destinationDir = outputFolder + java.io.File.separator + codeDir

  override def apiPackage = Option(apiDir)

  // template used for apis
  apiTemplateFiles ++= Map("api.mustache" -> ".js")
  
  modelTemplateFiles.clear

  additionalParams ++= Map(
    "artifactId" -> "swagger-sample-app",
    "artifactVersion" -> "1.0.0",
    "homepage" -> "http://swagger.wordnik.com",
    "codeDir" -> codeDir,
    "apiFolder" -> apiDir
  )

  // supporting classes
  override def supportingFiles = List(
    ("package.mustache", outputFolder, "package.json"),
    ("README.mustache", outputFolder, "README.md"),
    ("main.mustache", destinationDir, "main.js"),
    ("models.mustache", destinationDir, "models.js"))
}
