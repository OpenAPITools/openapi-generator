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

import com.wordnik.swagger.codegen.BasicGenerator

import scala.collection.mutable.{ HashMap, ListBuffer }

object SwaggerDocGenerator extends BasicGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def templateDir = "src/main/resources/swagger-static"

  val outputFolder = "samples/docs/swagger-static-docs"

  // where to write generated code
  override def destinationDir = outputFolder + "/sd"

  // template used for apis
  apiTemplateFiles += "operation.mustache" -> ".html"
  
  modelTemplateFiles.clear

  // package for models
  override def modelPackage = Some(".")

  // package for api classes
  override def apiPackage = Some("operations")

  override def supportingFiles = List(
    ("assets/css/bootstrap-responsive.css", destinationDir + "/assets/css", "bootstrap-responsive.css"),
    ("assets/css/bootstrap.css", destinationDir + "/assets/css", "bootstrap.css"),
    ("assets/css/style.css", destinationDir + "/assets/css", "style.css"),
    ("assets/images/logo.png", destinationDir + "/assets/images", "logo.png"),
    ("assets/js/bootstrap.js", destinationDir + "/assets/js", "bootstrap.js"),
    ("assets/js/jquery-1.8.3.min.js", destinationDir + "/assets/js", "jquery-1.8.3.min.js"),
    ("index.mustache", destinationDir, "index.html")
  )
}
