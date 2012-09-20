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

package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.core.util.JsonUtil

import com.wordnik.swagger.core._

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import Source._

object ApiExtractor {
  def m = JsonUtil.getJsonMapper

  def extractApiDocs(basePath: String, apis: List[DocumentationEndPoint], apiKey: Option[String] = None): List[Documentation] = {
    for (api <- apis) yield {
      val json = basePath.startsWith("http") match {
        case true => {
          println("calling: " + ((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")))
          Source.fromURL((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")).mkString
        }
        case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
      }
      val out = m.readValue(json, classOf[Documentation])
      out
    }
  }

  def extractOperations(basePath: String, api: DocumentationEndPoint): List[(String, DocumentationOperation)] = {
    (for(op <- api.getOperations.toList) yield (api.path, op)).toList
  }

  def getOperations(path: String, op: List[DocumentationOperation]): Map[String, DocumentationOperation] = {
    val opMap = new HashMap[String, DocumentationOperation]
    op.foreach(operation => opMap += path -> operation)
    opMap.toMap
  }
}