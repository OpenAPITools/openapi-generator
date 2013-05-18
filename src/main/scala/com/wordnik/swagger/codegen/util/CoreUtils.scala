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
    apis.foldLeft(Map.empty[String, Model]) { (acc, api) =>
      acc ++ api.models.getOrElse(Map.empty[String, Model]) ++ extractApiModels(api)
    }
  }

  def extractModelNames(op: Operation): Set[String] = {
//    // POST, PUT, DELETE body
    val modelNames = Set(op.responseClass) ++ op.parameters.filter(_.paramType == "body").map(_.dataType)
    modelNames map extractBasePartFromType
  }

  def extractBasePartFromType(datatype: String): String = {
    val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
    datatype match {
      case ComplexTypeMatcher(basePart) => basePart
      case _ => datatype
    }
  }

  def extractApiModels(sd: ApiListing): Map[String, Model] = {
    val modelObjects = sd.models.map(_.foldLeft(Map.empty[String, Model])(_ + _)) getOrElse Map.empty
    // return types
    val modelNames = sd.apis.foldLeft(Set.empty[String]) { (acc, api) =>
      api.operations.foldLeft(acc){ _ ++ extractModelNames(_) }
    }

    // extract all base model names, strip away Containers like List[] and primitives
    val baseNames = (modelNames filterNot primitives.contains) map extractBasePartFromType
    // get complex models from base
    val requiredModels = modelObjects.filter(obj => baseNames.contains(obj._1))


    // look inside top-level models
    val sn = subNames(requiredModels.toMap, modelObjects)
    val subModels = modelObjects.filter(obj => sn.contains(obj._1))
    val allModels = requiredModels ++ subModels
    allModels.filter(m => !primitives.contains(m._1)).toMap
  }

  def subNames(requiredModels: Map[String, Model], allModels: Map[String, Model], acc: Set[String] = Set.empty): Set[String] = {
     requiredModels.foldLeft(acc) { case (subNames, (_, m)) =>
       recurseModel(m.properties.toList, allModels, subNames + m.id)
    }
  }

  @tailrec def recurseModel(properties: List[(String, ModelProperty)], allModels: Map[String, Model], subNames: Set[String]): Set[String] = {
    properties match {
      case Nil => subNames
      case (_, subObject) :: rest =>
        val nm = subObject.`type`
        val propertyName = if (containers.contains(nm)) {
          subObject.items flatMap (si => Option(si.ref.getOrElse(si.`type`))) orElse Option(nm)
        } else Option(nm)

        if (propertyName.isDefined && !subNames.contains(propertyName.get)) {
          val prop = propertyName.get
          if (allModels.contains(prop)) {
            recurseModel(rest, allModels, this.subNames(Map(prop -> allModels(prop)), allModels, subNames + prop))
          } else {
            recurseModel(rest, allModels, subNames + prop)
          }
        } else recurseModel(rest, allModels, subNames)
    }
  }
}
