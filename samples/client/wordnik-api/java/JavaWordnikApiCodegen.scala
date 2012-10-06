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

import com.wordnik.swagger.codegen.BasicJavaGenerator

object JavaWordnikApiCodegen extends BasicJavaGenerator {
  def main(args: Array[String]) = generateClient(args)

  // location of templates
  override def templateDir = "Java"

  def destinationRoot = "samples/client/wordnik-api/java"

  // where to write generated code
  override def destinationDir = destinationRoot + "/src/main/java"

  // package for api invoker, error files
  override def invokerPackage = Some("com.wordnik.client.common")

  // package for models
  override def modelPackage = Some("com.wordnik.client.model")

  // package for api classes
  override def apiPackage = Some("com.wordnik.client.api")

  // supporting classes
  override def supportingFiles =
    List(
      ("apiInvoker.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replaceAll("\\.", java.io.File.separator) + java.io.File.separator, "ApiInvoker.java"),
      ("apiException.mustache", destinationDir + java.io.File.separator + invokerPackage.get.replaceAll("\\.", java.io.File.separator) + java.io.File.separator, "ApiException.java"),
      ("pom.mustache", destinationRoot, "pom.xml"))
}
