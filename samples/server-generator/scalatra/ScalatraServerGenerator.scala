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

object ScalatraServerGenerator extends BasicScalaGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def templateDir = "samples/server-generator/scalatra/templates"

  val outputFolder = "samples/server-generator/scalatra/output"

  // where to write generated code
  override def destinationDir = outputFolder + "/src/main/scala"

  override def modelPackage = Some("com.wordnik.client.model")

  // template used for apis
  apiTemplateFiles ++= Map("api.mustache" -> ".scala")

  modelTemplateFiles ++= Map("model.mustache" -> ".scala")

  override def apiPackage = Some("apis")

  // supporting classes
  override def supportingFiles = List(
    ("README.mustache", outputFolder, "README.md"),
    ("build.sbt", outputFolder, "build.sbt"),
    ("web.xml", outputFolder + "/src/main/webapp/WEB-INF", "web.xml"),
    ("Bootstrap.mustache", destinationDir, "ScalatraBootstrap.scala"),
    ("ServletApp.mustache", destinationDir, "ServletApp.scala"),
    ("project/build.properties", outputFolder, "project/build.properties"),
    ("project/plugins.sbt", outputFolder, "project/plugins.sbt"))

  override def processApiMap(m: Map[String, AnyRef]): Map[String, AnyRef] = {
    val mutable = scala.collection.mutable.Map() ++ m

    mutable.map(k => {
      k._1 match {
        // the scalatra templates like lower-case httpMethods
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
          // rip out the root path

          mutable += "path" -> path.replaceAll("\\{", ":").replaceAll("\\}", "")
        }
        case _ =>
      }
    })
    mutable.toMap
  }
}
