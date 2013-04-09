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

import scala.collection.mutable.{ HashSet, ListBuffer, HashMap }
import scala.collection.JavaConversions._
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

object CoreUtils {
  def extractAllModels(apis: List[ApiListing]): Map[String, Model] = {
    val modelObjects = new HashMap[String, Model]
    apis.foreach(api => {
      for ((nm, model) <- extractApiModels(api)) modelObjects += nm -> model
      if(api.models.isDefined) {
	api.models.get.foreach(model => modelObjects += model._1 -> model._2)
      }
    })
    modelObjects.toMap
  }

  def extractModelNames(op: Operation): Set[String] = {
    val modelNames = new HashSet[String]
    modelNames += op.responseClass
    // POST, PUT, DELETE body
    op.parameters.filter(p => p.paramType == "body")
      .foreach(p => modelNames += p.dataType)
    val baseNames = (for (modelName <- (modelNames.toList))
      yield (extractBasePartFromType(modelName))).toSet
    baseNames.toSet
  }

  def extractBasePartFromType(datatype: String): String = {
    val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
    datatype match {
      case ComplexTypeMatcher(basePart) => basePart
      case _ => datatype
    }
  }

  def extractApiModels(sd: ApiListing): Map[String, Model] = {
    val modelNames = new HashSet[String]
    val modelObjects = new HashMap[String, Model]
    // return types
    sd.apis.foreach(api => 
      api.operations.foreach(op => {
        modelNames += op.responseClass
        // POST, PUT, DELETE body
        op.parameters.filter(p => p.paramType == "body")
          .foreach(p => modelNames += p.dataType)
      })
    )
    if(sd.models.isDefined)
      for ((name, m) <- sd.models.get)
        modelObjects += name -> m

    // extract all base model names, strip away Containers like List[] and primitives
    val baseNames = (for (modelName <- (modelNames.toList filterNot primitives.contains))
      yield (extractBasePartFromType(modelName))).toSet

    // get complex models from base
    val requiredModels = modelObjects.filter(obj => baseNames.contains(obj._1))

    val subNames = new HashSet[String]
    // look inside top-level models
    recurseModels(requiredModels.toMap, modelObjects.toMap, subNames)

    val subModels = modelObjects.filter(obj => subNames.contains(obj._1))
    val allModels = requiredModels ++ subModels
    allModels.filter(m => primitives.contains(m._1) == false).toMap
  }

  def recurseModels(requiredModels: Map[String, Model], allModels: Map[String, Model], subNames: HashSet[String]) {
    requiredModels foreach (m => subNames ++ recurseModel(m._2.properties.toList, allModels, subNames.toSet))
  }

  @tailrec def recurseModel(properties: List[(String, ModelProperty)], allModels: Map[String, Model], subNames: Set[String]): Set[String] = {
    properties match {
      case Nil => subNames
      case (_, subObject) :: rest =>
        val propertyName = if (containers.contains(subObject.`type`)) {
          subObject.items flatMap { subItem =>
            Option(subItem.ref.getOrElse(subItem.`type`))
          }
        } else Option((subObject.`type`))

        if (propertyName.isDefined && !subNames.contains(propertyName.get)) {
          val prop = propertyName.get
          if (allModels.contains(prop)) {
            recurseModel(allModels(prop).properties.toList, allModels, subNames + prop)
          } else {
            recurseModel(rest, allModels, subNames + prop)
          }
        } else recurseModel(rest, allModels, subNames)
    }
  }
}
