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

import com.wordnik.swagger.codegen.BasicScalaGenerator

import scala.collection.mutable.{ HashMap, ListBuffer }

object NodeServerGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def templateDir = "samples/server-generator/node/templates"

  val outputFolder = "samples/server-generator/node/output"
    
  // where to write generated code
  override def destinationDir = outputFolder + "/App"

  // template used for apis
  apiTemplateFiles ++= Map("api.mustache" -> ".js")
  
  modelTemplateFiles.clear

  override def apiPackage = Some("apis")
  
  // supporting classes
  override def supportingFiles = List(
    ("package.json", outputFolder, "package.json"),
    ("README.mustache", outputFolder, "README.md"),
    ("main.mustache", destinationDir, "main.js"),
    ("models.mustache", destinationDir, "models.js"))
}
