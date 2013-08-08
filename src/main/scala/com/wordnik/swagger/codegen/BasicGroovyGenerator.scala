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

import com.wordnik.swagger.codegen.BasicJavaGenerator

object BasicGroovyGenerator extends BasicGroovyGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicGroovyGenerator extends BasicJavaGenerator {

  // location of templates
  override def templateDir = "Groovy"

  // where to write generated code
  override def destinationDir = "generated-code/groovy/src/main/groovy"

  // template used for models
  modelTemplateFiles += "model.mustache" -> ".groovy"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".groovy"


  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // file suffix
  override def fileSuffix = ".groovy"


  override def supportingFiles =
    List(
      ("ApiUtils.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replace(".", java.io.File.separator) + java.io.File.separator, "ApiUtils.groovy"),
      ("build.gradle.mustache", "generated-code/groovy", "build.gradle"))

}
