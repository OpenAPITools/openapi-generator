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

import com.wordnik.swagger.codegen.BasicRubyGenerator

import java.io.File

import scala.collection.mutable.{ HashMap, ListBuffer }

object SinatraServerGenerator extends BasicRubyGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def toApiName(name: String): String = name + "_api"

  // undo the ruby-ish conversions in the BasicRubyGenerator
  override def toVarName(name: String): String = name
  override def toMethodName(name: String): String = name

  override def templateDir = "samples/server-generator/sinatra/templates"

  val outputFolder = "samples/server-generator/sinatra/output"

  // where to write generated code
  override def destinationDir = outputFolder + ""

  override def modelPackage = Some("com.wordnik.client.model")

  // template used for apis
  apiTemplateFiles ++= Map("api.mustache" -> ".rb")

  modelTemplateFiles.clear

  override def apiPackage = Some("lib")

  // supporting classes
  override def supportingFiles = List(
    ("README.md", outputFolder, "README.md"),
    ("config.ru", outputFolder, "config.ru"),
    ("Gemfile", outputFolder, "Gemfile"),
    ("my_app.mustache", outputFolder, "my_app.rb"),
    ("lib/swaggering.rb", outputFolder + File.separator + "lib", "swaggering.rb"))

  override def processApiMap(m: Map[String, AnyRef]): Map[String, AnyRef] = {
    val mutable = scala.collection.mutable.Map() ++ m

    mutable.map(k => {
      k._1 match {
        // the sinatra templates like lower-case httpMethods
        case e: String if (e == "httpMethod") => mutable += "httpMethod" -> k._2.toString.toLowerCase

        // convert path into ruby-ish syntax without basePart (i.e. /pet.{format}/{petId} => /:petId
        case e: String if (e == "path") => {
          val path = {
            val arr = k._2.toString.split("/")
            if (arr.length >= 2) {
              mutable += "basePart" -> (arr.slice(2, arr.length).mkString("", "/", ""))
              "/" + arr.slice(2, arr.length).mkString("", "/", "")
            } else
              k._2.toString
          }
          mutable += "path" -> path.replaceAll("\\{", ":").replaceAll("\\}", "")
        }
        case _ =>
      }
    })
    mutable.toMap
  }
}
