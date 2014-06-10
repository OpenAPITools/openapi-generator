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

import com.wordnik.swagger.codegen.BasicJavaGenerator

import scala.collection.mutable.{ HashMap, ListBuffer }

object JavaJaxRSServerGenerator extends BasicJavaGenerator {
  def main(args: Array[String]) = generateClient(args)

  override def templateDir = "samples/server-generator/java-jaxrs/templates"

  val outputFolder = "samples/server-generator/java-jaxrs/output"

  // where to write generated code
  override def destinationDir = outputFolder + "/src/main/java"

  override def modelPackage = Some("com.wordnik.client.model")

  // template used for apis
  apiTemplateFiles ++= Map("api.mustache" -> ".java")

  modelTemplateFiles ++= Map("model.mustache" -> ".java")

  override def apiPackage = Some("com.wordnik.api")

  // supporting classes
  override def supportingFiles = List(
    ("README.mustache", outputFolder, "README.md"),
    ("ApiException.mustache", destinationDir + "/" + apiPackage.get.replaceAll("\\.", "/"), "ApiException.java"),
    ("ApiOriginFilter.mustache", destinationDir + "/" + apiPackage.get.replaceAll("\\.", "/"), "ApiOriginFilter.java"),
    ("ApiResponse.mustache", destinationDir + "/" + apiPackage.get.replaceAll("\\.", "/"), "ApiResponse.java"),
    ("JacksonJsonProvider.mustache", destinationDir + "/" + apiPackage.get.replaceAll("\\.", "/"), "JacksonJsonProvider.java"),
    ("NotFoundException.mustache", destinationDir + "/" + apiPackage.get.replaceAll("\\.", "/"), "NotFoundException.java"),
    ("pom.xml", outputFolder, "pom.xml"),
    ("web.mustache", outputFolder + "/src/main/webapp/WEB-INF", "web.xml")
	)

  override def processApiMap(m: Map[String, AnyRef]): Map[String, AnyRef] = {
    val mutable = scala.collection.mutable.Map() ++ m

    mutable.map(k => {
      k._1 match {
        case "allParams" => {
          val paramList = k._2.asInstanceOf[List[_]]
          paramList.foreach(param => {
            val map = param.asInstanceOf[scala.collection.mutable.HashMap[String, AnyRef]]
            if(map.contains("dataType")){
              val dataType = map("dataType")
              map += "dataType" -> dataType.toString.replaceAll("Array\\[","List[")
            }
            if(map.contains("required")) {
              if(map("required") == "false") map += "notRequired" -> "true"
            }
            if(map.contains("defaultValue")) {
            	// unquote default value
            	val defaultValue = {
            		map("defaultValue") match {
            			case Some(d) => {
            				val str = d.toString
		            		if(str.startsWith("\"") && str.endsWith("\""))
		            			Some(str.substring(1, str.length-1))
		            		else Some(d)
		            	}
		            	case None => None
		            }
            	}
            	map += "defaultValue" -> defaultValue
            }
            if(map.contains("allowableValues")) {
              val allowableValues = map("allowableValues")
              val quote = map("swaggerDataType") match {
                case "string" => "\""
                case _ => ""
              }
              val pattern = "([A-Z]*)\\[(.*)\\]".r
              val str = allowableValues match {
                case pattern(valueType, values) => {
                  valueType match {
                    case "LIST" => {
                      val l = values.split(",").toList
                      Some("AllowableValues(" + l.mkString(quote, quote + "," + quote, quote + ")"))
                    }
                    case "RANGE" => {
                      val r = values.split(",")
                      Some("AllowableValues(Range(" + r(0) + "," + r(1) + ", 1))")
                    }
                  }
                }
                case _ => None
              }
              str match {
                case Some(s) => map += "allowableValues" -> s
                case _ =>
              }
            }
          })
        }
        case "path" => {
          val path = {
            val arr = k._2.toString.split("/")
            if (arr.length >= 2) {
              mutable += "basePart" -> (arr.slice(2, arr.length).mkString("", "/", ""))
              "/" + arr.slice(2, arr.length).mkString("", "/", "")
            } else
              k._2.toString
          }
          // rip out the root path
          mutable += "path" -> path
        }
        case "returnType" => {
        	k._2 match {
        		case Some(returnType) =>
        		case None => mutable += "returnType" -> "void"
        	}
        }
        case _ =>
      }
    })
    mutable.toMap
  }
}
