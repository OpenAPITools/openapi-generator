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

package com.wordnik.swagger.model

import scala.collection.mutable.LinkedHashMap

case class ResourceListing(
  apiVersion: String, 
  swaggerVersion: String, 
  basePath: String,
  apis: List[ApiListingReference] = List())

case class ApiListingReference(path:String, description: String)

trait AllowableValues
case object Any extends AllowableValues
case class AllowableListValues (values: List[String] = List(), valueType: String = "LIST") extends AllowableValues
case class AllowableRangeValues(min: String, max: String) extends AllowableValues

case class Model(
  var id: String,
  var name: String,
  var properties: LinkedHashMap[String, ModelProperty],
  description: Option[String] = None)

case class ModelProperty(
  var `type`: String,
  required: Boolean = false,
  description: Option[String] = None,
  allowableValues: AllowableValues = Any,
  var items: Option[ModelRef] = None)

case class ModelRef(
  `type`: String,
  ref: Option[String] = None)

case class ApiListing (
  apiVersion: String,
  swaggerVersion: String,
  basePath: String,
  var resourcePath: String,
  apis: List[ApiDescription] = List(),
  models: Map[String, Model] = Map())

case class ApiDescription (
  path: String,
  description: String,
  operations: List[Operation] = List())

case class Operation (
  httpMethod: String,
  summary: String,
  notes: String,
  var responseClass: String,
  nickname: String,
  parameters: List[Parameter] = List.empty,
  errorResponses: List[ErrorResponse] = List.empty,
  `deprecated`: Option[String] = None)

case class Parameter (
  name: String,
  description: String,
  defaultValue: String,
  required: Boolean,
  allowMultiple: Boolean,
  var dataType: String,
  allowableValues: AllowableValues = Any,
  paramType: String)

case class ErrorResponse (
  code: Int,
  reason: String)

