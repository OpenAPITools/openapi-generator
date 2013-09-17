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

package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.model._

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.read

import java.net.URL
import java.io.InputStream

import scala.io._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }

object ApiExtractor extends RemoteUrl {
  def fetchApiListings(version: String, basePath: String, apis: List[ApiListingReference], authorization: Option[AuthorizationValue] = None): List[ApiListing] = {
    implicit val formats = SwaggerSerializers.formats(version)
    (for (api <- apis) yield {
      try{
        val json = (basePath.startsWith("http")) match {
          case true => {
            val path = if(api.path.startsWith("http")) api.path
            else basePath + api.path
            urlToString(path.replaceAll(".\\{format\\}", ".json"), authorization)
          }
          case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
        }
        Some(parse(json).extract[ApiListing])
      }
      catch {
        case e: java.io.FileNotFoundException => {
          println("WARNING!  Unable to read API " + basePath + api.path)
          None
        }
        case e: Throwable => {
          println("WARNING!  Unable to read API " + basePath + api.path)
          e.printStackTrace()
          None
        }
      }
    }).flatten.toList
  }

  def extractApiOperations(version: String, basePath: String, references: List[ApiListingReference], authorization: Option[AuthorizationValue] = None) = {
    implicit val formats = SwaggerSerializers.formats(version)
    for (api <- references) yield {
      val json = basePath.startsWith("http") match {
        case true => {
          urlToString((basePath + api.path).replaceAll(".\\{format\\}", ".json"), authorization)
        }
        case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
      }
      parse(json).extract[ApiListing]
    }
  }

  def extractApiOperations(basePath: String, apiDescription: ApiDescription): List[(String, Operation)] = {
    (for(op <- apiDescription.operations) yield (apiDescription.path, op)).toList
  }
}
