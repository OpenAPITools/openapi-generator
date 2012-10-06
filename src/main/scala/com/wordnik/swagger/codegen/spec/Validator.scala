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

package com.wordnik.swagger.codegen.spec

import com.wordnik.swagger.codegen.util.{CoreUtils, ApiExtractor, ResourceExtractor}
import com.wordnik.swagger.codegen.PathUtil

import scala.collection.JavaConversions._

object Validator extends PathUtil {
  def main(args: Array[String]) {
    if(args.length == 0) {
      throw new RuntimeException("Need url to resources.json as argument. You can also specify VM Argument -DfileMap=/path/to/folder/containing.resources.json/")
    }
    val host = args(0)
    val apiKey = {
      if (args.length > 1) Some("?api_key=" + args(1))
      else None
    }

    val outputFilename = {
      if (args.length > 2) Some(args(2))
      else None
    }
    val doc = {
      try {
        ResourceExtractor.fetchListing(getResourcePath(host), apiKey)
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host, e)
      }
    }

    val basePath = getBasePath(doc.basePath)
    val apis = ApiExtractor.fetchApiListings(basePath, doc.apis, apiKey)

    val swaggerSpecValidator = new SwaggerSpecValidator(doc, apis, false)
    swaggerSpecValidator.validate()
    swaggerSpecValidator.generateReport(host, outputFilename)

    System.exit(0)

  }
}
