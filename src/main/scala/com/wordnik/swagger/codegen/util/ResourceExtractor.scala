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

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.read

import scala.io._

object ResourceExtractor extends RemoteUrl {
  implicit val formats = SwaggerSerializers.formats

	def fetchListing(path: String, apiKey: Option[String] = None): ResourceListing = {
		val json = path.startsWith("http") match {
			case true => urlToString(path + apiKey.getOrElse(""))
			case false => Source.fromFile(path).mkString
		}
		parse(json).extract[ResourceListing]
	}
}