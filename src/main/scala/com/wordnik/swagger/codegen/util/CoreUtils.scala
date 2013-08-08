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

import scala.collection.mutable.{ HashSet, ListBuffer, HashMap }
import scala.collection.JavaConversions._
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

object CoreUtils {
  def extractAllModels(apis: List[ApiListing], excludes: Set[String] = Set.empty, typeMapping: Map[String, String] = Map.empty): Map[String, Model] = {
    apis.foldLeft(Map.empty[String, Model]) { (acc, api) =>
      acc ++ api.models.getOrElse(Map.empty[String, Model]) ++ extractApiModels(api, excludes, typeMapping)
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

  def extractApiModels(sd: ApiListing, excludes: Set[String] = Set.empty, typeMapping: Map[String, String] = Map.empty): Map[String, Model] = {

    def declNm(nm: String) =
      typeMapping.foldLeft(nm)((n, kv) => ("\\b"+kv._1+"\\b").r.replaceAllIn(n, kv._2))
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
    val sn = subNames(requiredModels.toMap, modelObjects, Set.empty, typeMapping)
    val subModels = modelObjects.filter(obj => sn.contains(obj._1))
    val ex = excludes ++ primitives

    for {
      (k, v) <- requiredModels ++ subModels
      if (!ex.contains(k))
    } yield k -> v.copy(properties = v.properties.map(kv => kv._1 -> kv._2.copy(`type` = declNm(kv._2.`type`))))
  }

  def subNames(requiredModels: Map[String, Model], allModels: Map[String, Model], acc: Set[String] = Set.empty, typeMapping: Map[String, String]): Set[String] = {
     requiredModels.foldLeft(acc) { case (subNames, (_, m)) =>
       recurseModel(m.properties.toList, allModels, subNames + typeMapping.getOrElse(m.id, m.id), typeMapping)
    }
  }

  @tailrec def recurseModel(properties: List[(String, ModelProperty)], allModels: Map[String, Model], subNames: Set[String], typeMapping: Map[String, String]): Set[String] = {
    properties match {
      case Nil =>
        subNames
      case (_, subObject) :: rest =>
        val nm = subObject.`type`
        def declNm(nm: String) =
          typeMapping.foldLeft(nm)((n, kv) => ("\\b"+kv._1+"\\b").r.replaceAllIn(n, kv._2))

        val propertyName = if (containers.contains(nm)) {
          subObject.items flatMap (si => Option(si.ref.getOrElse(si.`type`)).map(declNm)) orElse Option(declNm(nm))
        } else Option(declNm(nm))

        if (propertyName.isDefined && !subNames.contains(propertyName.get)) {
          val prop = propertyName.get
          if (allModels.contains(prop)) {
            val newSubnames = this.subNames(Map(prop -> allModels(prop)), allModels, subNames + prop, typeMapping)
            recurseModel(rest, allModels, newSubnames, typeMapping)
          } else {
            val newSubnames = subNames + prop
            recurseModel(rest, allModels, newSubnames, typeMapping)
          }
        } else recurseModel(rest, allModels, subNames, typeMapping)
    }
  }
}
