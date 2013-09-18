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

import com.wordnik.swagger.codegen.BasicGenerator
import com.wordnik.swagger.codegen.spec.SwaggerSpec
import com.wordnik.swagger.model._

import scala.collection.mutable.{ HashMap, ListBuffer }

object SwaggerDocGenerator extends BasicGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def templateDir = "src/main/resources/swagger-static"

  val outputFolder = "samples/docs/swagger-static-docs"

  // where to write generated code
  override def destinationDir = outputFolder + "/src/main/webapp"

  // template used for apis
  apiTemplateFiles += "operation.mustache" -> ".html"
  
  modelTemplateFiles += "model.mustache" -> ".html"

  override def toDeclaration(obj: ModelProperty): (String, String) = {
    obj.`type` match {
      case "Array" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        val e = "List[%s]" format toDeclaredType(inner)
        (e, toDefaultValue(inner, obj))
      }
      case e: String => (toDeclaredType(e), toDefaultValue(e, obj))
    }
  }

  override def processApiMap(m: Map[String, AnyRef]): Map[String, AnyRef] = {
    val mutable = scala.collection.mutable.Map() ++ m
    mutable.map(k => {
      k._1 match {
        case "allParams" => {
          val paramList = k._2.asInstanceOf[List[_]]
          paramList.foreach(param => {
            val map = param.asInstanceOf[scala.collection.mutable.HashMap[String, AnyRef]]
            if(map.contains("dataType")){
              val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
              val v = map("dataType") match {
                case ComplexTypeMatcher(v) => v
                case _ => map("dataType").asInstanceOf[String]
              }
              if(SwaggerSpec.primitives.contains(v)) {
                map += "simpleType" -> v
              }
              else {
                map += "complexType" -> v
              }
            }
          })
        }
        case _ =>
      }
    })
    mutable.toMap
  }

  // package for models
  override def modelPackage = Some("models")

  // package for api classes
  override def apiPackage = Some("operations")

  override def supportingFiles = List(
    ("pom.xml", outputFolder, "pom.xml"),
    ("assets/css/bootstrap-responsive.css", destinationDir + "/assets/css", "bootstrap-responsive.css"),
    ("assets/css/bootstrap.css", destinationDir + "/assets/css", "bootstrap.css"),
    ("assets/css/style.css", destinationDir + "/assets/css", "style.css"),
    ("assets/images/logo.png", destinationDir + "/assets/images", "logo.png"),
    ("assets/js/bootstrap.js", destinationDir + "/assets/js", "bootstrap.js"),
    ("assets/js/jquery-1.8.3.min.js", destinationDir + "/assets/js", "jquery-1.8.3.min.js"),
    ("assets/js/main.js", destinationDir + "/assets/js", "main.js"),
    ("index.mustache", destinationDir, "index.html")
  )
}
