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

object CoreUtils {
  def extractAllModels2(apis: List[ApiListing]): Map[String, Model] = {
    val modelObjects = new HashMap[String, Model]
    apis.foreach(api => {
      for ((nm, model) <- extractApiModels(api)) modelObjects += nm -> model
      if (api.models != null) api.models.foreach(model => modelObjects += model._1 -> model._2)
    })
    modelObjects.toMap
  }

  def extractModelNames(op: Operation): Set[String] = {
    val modelNames = new HashSet[String]
    modelNames += op.responseClass
    // POST, PUT, DELETE body
    if (op.parameters != null) {
      op.parameters.filter(p => p.paramType == "body")
        .foreach(p => modelNames += p.dataType)
    }
    val baseNames = (for (modelName <- (modelNames.toList))
      yield (extractBasePartFromType(modelName))).toSet
    baseNames.toSet
  }

  def extractModelNames2(modelObjects: Map[String, Model], ep: Operation): Set[String] = {
    val modelNames = new HashSet[String]

    modelNames += ep.responseClass
    // POST, PUT, DELETE body
    if (ep.parameters != null)
      ep.parameters.filter(p => p.paramType == "body")
        .foreach(p => modelNames += p.dataType)

    val baseNames = (for (modelName <- (modelNames.toList))
      yield (extractBasePartFromType(modelName))).toSet

    // get complex models from base
    val requiredModels = modelObjects.filter(obj => baseNames.contains(obj._1))

    val subNames = new HashSet[String]
    // look inside top-level models
    requiredModels.map(model => {
      // add top level model
      subNames += model._1
      model._2.properties.foreach(prop => {
        val subObject = prop._2
        if (containers.contains(subObject.`type`)) {
          subObject.items match {
            case Some(item) => {
              if(item.ref != null)
                subNames += item.ref
              else
                subNames += item.`type`
            }
            case None => 
          }
        }
        else subNames += subObject.`type`
      })
    })
    subNames.toSet
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
    if(sd.apis != null){
      sd.apis.foreach(api => {
        if (api.operations != null)
          api.operations.foreach(op => {
            modelNames += op.responseClass
            // POST, PUT, DELETE body
            if(op.parameters != null)
              op.parameters.filter(p => p.paramType == "body")
                .foreach(p => modelNames += p.dataType)
        })
      })
    }
    if(sd.models != null)
      for ((name, m) <- sd.models) 
        modelObjects += name -> m

    // extract all base model names, strip away Containers like List[] and primitives
    val baseNames = (for (modelName <- (modelNames.toList -- primitives))
      yield (extractBasePartFromType(modelName))).toSet

    // get complex models from base
    val requiredModels = modelObjects.filter(obj => baseNames.contains(obj._1))

    val subNames = new HashSet[String]
    // look inside top-level models
    requiredModels.map(model => {
      model._2.properties.foreach(prop => {
        val subObject = prop._2
        if (containers.contains(subObject.`type`)) {
          subObject.items match {
            case Some(subItem) => {
              if (subItem.ref != null) {
                subNames += subItem.ref
              } else {
                subNames += subItem.`type`
              }
            }
            case _ =>
          }
        } else subNames += subObject.`type`
      })
    })
    
    // look inside submodels
    modelObjects.filter(obj => subNames.contains(obj._1)).foreach(model => {
      model._2.properties.foreach(prop => {
        val subObject = prop._2
        if (containers.contains(subObject.`type`)) {
          subObject.items match {
            case Some(subItem) => {
              if (subItem.ref != null) {
                subNames += subItem.ref
              } else {
                subNames += subItem.`type`
              }
            }
            case _ =>
          }
        } else subNames += subObject.`type`
      })
    })
    val subModels = modelObjects.filter(obj => subNames.contains(obj._1))
    val allModels = requiredModels ++ subModels
    allModels.filter(m => primitives.contains(m._1) == false).toMap
  }
}