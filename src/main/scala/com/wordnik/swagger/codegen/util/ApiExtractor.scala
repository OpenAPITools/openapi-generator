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

import com.wordnik.swagger.model._

import java.net.URL
import java.io.InputStream

import scala.io._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }

object ApiExtractor extends RemoteUrl {
  def json = ScalaJsonUtil.getJsonMapper

  def fetchApiListings(basePath: String, apis: List[ApiListingReference], apiKey: Option[String] = None): List[ApiListing] = {
    for (api <- apis) yield {
      val str = basePath.startsWith("http") match {
        case true => {
          println("calling: " + ((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")))
          urlToString((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json"))
        }
        case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
      }
      json.readValue(str, classOf[ApiListing])
    }
  }

  def extractApiOperations(basePath: String, references: List[ApiListingReference], apiKey: Option[String] = None) = {
    for (api <- references) yield {
      val str = basePath.startsWith("http") match {
        case true => {
          println("calling: " + ((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")))
          urlToString((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json"))
        }
        case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
      }
      json.readValue(str, classOf[ApiListing])
    }
  }

  def extractApiOperations(basePath: String, apiDescription: ApiDescription): List[(String, Operation)] = {
    (for(op <- apiDescription.operations) yield (apiDescription.path, op)).toList
  }
}