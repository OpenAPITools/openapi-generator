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

import com.wordnik.swagger.model._
import com.wordnik.swagger.codegen.PathUtil
import com.wordnik.swagger.codegen.spec.SwaggerSpec._
import com.wordnik.swagger.codegen.util.CoreUtils

import java.util.logging.Logger
import String.format

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

import org.fusesource.scalate.{ TemplateSource, TemplateEngine }
import java.io.{ FileWriter, File }
import scala.io.Source
import scala.collection.mutable.HashSet
import scala.collection.immutable.HashMap

class SwaggerSpecValidator(private val doc: ResourceListing,
  private val apis: List[ApiListing],
  private val fix: Boolean = true) extends PathUtil {

  import ValidationMessage._

  private val validationMessages = ListBuffer.empty[ValidationMessage]

  private val LOGGER = Logger.getLogger(classOf[SwaggerSpecValidator].getName)

  def validate() {
    checkRootProperties()

    apis.foreach(api => {
      fixSubDoc(api)

      if (api.models != null) {
        fixReturnModels(api.models.toMap, apis)
        fixInputDataTypes(api.models.toMap, apis)
        fixModels(api.models.toMap)
      }
    })

    validateResponseModels(apis)
    println("----------")
    println(this)
  }

  def validateResponseModels(subDocs: List[ApiListing]) = {
    val validModelNames = CoreUtils.extractAllModels(subDocs).map(m => m._1).toSet
    val requiredModels = new HashSet[String]
     subDocs.foreach(subDoc => {
       if (subDoc.apis != null) {
	       subDoc.apis.foreach(api => {
          api.operations.foreach(op => {
            requiredModels += {
              val responseClass = op.responseClass
              responseClass.indexOf("[") match {
		            case i: Int if (i > 0) => {
                  CoreUtils.extractBasePartFromType(responseClass)
            		}
            		case _ => responseClass
              }
            }
          })
	      })
      }
    })
    val missingModels = requiredModels.toSet -- (validModelNames ++ primitives)

    if (missingModels.size > 0) println("missing models: " + missingModels)
  }

  def generateReport(host: String, outputFilename: Option[String]) {
    outputFilename match {
      case Some(o) => {
        val rootDir = new java.io.File(".")
        val engine = new TemplateEngine(Some(rootDir))
        val templateLocation = "validator" + File.separator + "index.mustache"
        val template = engine.compile(
          TemplateSource.fromText(templateLocation, Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(templateLocation)).mkString))
        val output = engine.layout(templateLocation, template, HashMap(
          "messages" -> validationMessages,
          "host" -> host,
          "basePath" -> doc.basePath,
          "swaggerVersion" -> doc.swaggerVersion,
          "apiVersion" -> doc.apiVersion))
        val fw = new FileWriter(o, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + o)

      }

      case None =>
        println("Output file location not passed as program argument")
    }

  }

  /**
   * Checks the swagger.version, basePath and api.version which is
   * expected to be present in root resource listing
   *
   */
  private def checkRootProperties() {
    doc.swaggerVersion match {
      case e: String => println("swagger version: " + e)
      case _ => !!(doc, RESOURCE_LISTING, "Properties", "Missing swagger version")
    }
    doc.basePath match {
      case e: String => println("basePath: " + e)
      case _ => !!(doc, RESOURCE_LISTING, "Properties", "Missing base path")
    }
    doc.apiVersion match {
      case e: String => println("api version: " + e)
      case _ => !!(doc, RESOURCE_LISTING, "Properties", "Missing api version", WARNING)
    }
  }

  /**
   * this is here because sub documents don't have the same resourcePath as declared in
   * the main resource listing
   */
  private def fixSubDoc(api: ApiListing) = {
    if (api.resourcePath.indexOf(".{format}") == -1) {
      doc.apis.foreach(op => {
        if (op.path.indexOf(".{format}") > 0 && op.path.replaceAll(".\\{format\\}", "") == api.resourcePath) {
          if (fix) {
            api.resourcePath = api.resourcePath + ".{format}"
          }
        }
      })
    }
  }

  /**
   * this is here because models don't have the proper references to types
   */
  private def fixModels(models: Map[String, Model]) = {
    val validModelNames = models.map(_._1).toSet
    LOGGER.finest("all valid models: " + validModelNames)
    for ((name, model) <- models) {
      // id of model
      getUpdatedType(validModelNames, model.id) match {
        case Some(updatedType) => {
          if (!model.id.equals(updatedType)) {
            !!(model, MODEL, model.id, format("Invalid id. Best guess: %s", updatedType))
            LOGGER.finest("updated " + model.id + " to " + updatedType)
            if (fix) model.id = updatedType
          }
        }
        case None => {
          LOGGER.finest("can't find type for " + model.name + ", type " + model.id)
          !!(model, MODEL, model.name, format("Missing type (%s)", model.id))
        }
      }

      model.properties.foreach(prop => {
        val subObjectName = prop._1
        val subObject = prop._2

        if (containers.contains(subObject.`type`)) {
          // process the sub object
          subObject.items match {
            case Some(item) => {
              getUpdatedType(validModelNames, item.ref.getOrElse(null)) match {
                case Some(updatedType) => {
                  if (!item.ref.get.equals(updatedType)) {
                    !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid ref (%s). Best guess: %s", item.ref, updatedType))
                    LOGGER.finest("updated subObject.items.ref " + item.ref + " to " + updatedType)
                    if (fix) {
                      subObject.items = Some(ModelRef(null, Some(updatedType)))
                    }
                  }
                }
                case None =>
              }
            }
            case _ =>
          }
        } else if (containers.contains(subObject.`type`)) {
          // process the sub object
          if (subObject.items != null && subObject.items != None && subObject.items.get.ref != null){
            subObject.items match {
              case Some(item) => {
                getUpdatedType(validModelNames, item.ref.getOrElse(null)) match {
                  case Some(updatedType) => {
                    if (!item.ref.equals(updatedType)) {
                      !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid ref (%s). Best guess: %s", item.ref, updatedType))
                      LOGGER.finest("updated subObject.items.ref " + item.ref + " to " + updatedType)
                      if (fix) subObject.items = Some(ModelRef(null, Some(updatedType)))
                    }
                  }
                  case None => {
                    !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid ref (%s).", item.ref))
                    LOGGER.finest("didn't know what to do with " + item.ref)
                  }
                }
              }
              case _ =>
            }
          }
          else if (subObject.items != null && subObject.items != None && subObject.items.get.`type` != null) {
            subObject.items match {
              case Some(item) => {
                getUpdatedType(validModelNames, item.`type`) match {
                  case Some(updatedType) => {
                    if (!item.`type`.equals(updatedType)) {
                      !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid type (%s). Best guess: %s", item.`type`, updatedType))
                      LOGGER.finest("updated subObject.items.type" + item.`type` + " to " + updatedType)
                      if (fix) subObject.items = Some(ModelRef(`type` = updatedType))
                    }
                  }
                  case None => {
                    println("nothing found for " + subObject)
                    !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid ref (%s).", item.ref))
                    LOGGER.finest("didn't know what to do with " + item.ref)
                  }
                }
              }
              case _ =>
            }
          }
        } else {
          getUpdatedType(validModelNames, subObject.`type`) match {
            case Some(updatedType) => {
              if (!subObject.`type`.equals(updatedType)) {
                !!(model, MODEL_PROPERTY, format("%s->%s: %s", model.id, subObjectName, subObject.`type`), format("Invalid type (%s). Best guess: %s", subObject.`type`, updatedType))
                LOGGER.finest("updated subObject.getType " + subObject.`type` + " to " + updatedType)
                if (fix) subObject.`type` = updatedType
              }
            }
            case None =>
          }
        }
      })
      // remove params with invalid names (Pos???)
      model.properties = model.properties.filter(prop => {
        if (prop._1.indexOf("$") == -1) true
        else {
          !!(model, MODEL, model.id, format("Invalid property %s. Removing it", prop._1))
          LOGGER.finest("removing invalid property " + prop._1)
          if (fix) false else true
        }
      })
    }
  }

  /**
   * this is here because input params in operations don't match primitives or model names
   */
  private def fixInputDataTypes(models: Map[String, Model], a: List[ApiListing]) = {
    val validModelNames = models.map(m => m._1).toSet

    // List[ApiListing]
    a.foreach(listing => {
      if (listing.apis != null) {
        listing.apis.foreach(api => {
          // List[ApiDescription]
          api.operations.foreach(op => {
            // List[Operation]
            val modelNames = new ListBuffer[String]
            if(op.parameters != null) {
              op.parameters.foreach(p => {
                val dataType = p.dataType

                p.paramType match {
                  case "body" => {
                    getUpdatedType(validModelNames, dataType) match {
                      case Some(updatedName) => {
                        if (!p.dataType.equals(updatedName)) {
                          //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                          !!(p, OPERATION_PARAM, format("%s.%s(body: %s)", apiNameFromPath(api.path), op.nickname, p.dataType), format("Invalid data type %s. Best guess: %s", p.dataType, updatedName))
                          if (fix) p.dataType = updatedName
                        }
                      }
                      case _ => LOGGER.finest("rats!") // leave it alone
                    }
                  }
                  case "path" => {
                    getUpdatedType(validModelNames, dataType) match {
                      case Some(updatedName) => {
                        //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                        !!(p, OPERATION_PARAM, format("%s.%s(path_%s: %s)", apiNameFromPath(api.path), op.nickname, p.name, p.dataType), format("Invalid data type %s. Best guess: %s", p.dataType, updatedName))
                        if (fix) p.dataType = updatedName
                      }
                      case _ => // leave it alone
                    }
                  }
                  case "query" => {
                    getUpdatedType(validModelNames, dataType) match {
                      case Some(updatedName) => {
                        //                      LOGGER.finest("--> updated " + dataType + " to " + updatedName)
                        !!(p, OPERATION_PARAM, format("%s.%s(query_%s: %s)", apiNameFromPath(api.path), op.nickname, p.name, p.dataType), format("Invalid %s. Best guess: %s", p.dataType, updatedName))
                        if (fix) p.dataType = updatedName
                      }
                      case _ => // leave it alone
                    }
                  }
                  case _ =>
                }

              })
            }
          })
        })
      }
    })
  }

  /**
   * this is here because the return types are inconsistent from the swagger-core-1.02-SNAPSHOT
   */
  private def fixReturnModels(models: Map[String, Model], a: List[ApiListing]) = {
    val validModelNames = models.map(m => m._1).toSet

    // List[ApiListing]
    a.foreach(listing => {
      if (listing.apis != null) {
        listing.apis.foreach(api => {
          // List[ApiDescription]
          api.operations.foreach(op => {
            // List[Operation]
            val responseClass = op.responseClass
            if (responseClass != null) {
              getUpdatedType(validModelNames, responseClass) match {
                case Some(updatedName) => {
                  if (!responseClass.equals(updatedName)) {
                    LOGGER.finest("--> updated " + responseClass + " to " + updatedName)
                    !!(op, OPERATION, format("%s.%s(): %s", apiNameFromPath(api.path), op.nickname, op.responseClass), format("Invalid response class. Best guess: %s", updatedName))
                    if (fix) op.responseClass = updatedName
                  }
                }
                case _ => {
                } // leave it alone
              }
            }
          })
        })
      }
    })
  }

  private def getUpdatedType(validModelNames: Set[String], name: String): Option[String] = {
    if(name == null) return None

    if (validModelNames.contains(name)) {
      Some(name)
    } else if (name.indexOf("[") > 0) {
      // it's a complex value
      val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
      val ComplexTypeMatcher(basePart) = name

      getUpdatedType(validModelNames, basePart) match {
        case Some(updatedPart) => {
          Some(name.replaceAll(java.util.regex.Pattern.quote(basePart), updatedPart))
        }
        case _ => None
      }
    } else if (name.indexOf(".") > 0) {
      val basePart = name.split("\\.").last
      getUpdatedType(validModelNames, basePart) match {
        case Some(updatedPart) => {
          Some(updatedPart)
        }
        case _ => {
          None
        }
      }
    } else if (!primitives.contains(name)) {
      val pc = name
      if (validModelNames.contains(pc)) {
        Some(pc)
      } else if (pc == "Ok") {
        Some("void")
      } else if (pc == "Long") {
        Some("long")
      } else if (pc == "Double") {
        Some("double")
      } else if (pc == "Float") {
        Some("float")
      } else if (pc == "Boolean") {
        Some("boolean")
      } else if (pc == "Integer") {
        Some("int")
      } else if (pc == "Byte") {
        Some("byte")
      } else {
        None
      }
    } else {
      None
    }
  }

  def !!(element: AnyRef, elementType: String, elementId: String, message: String, level: String = ERROR) {
    validationMessages += new ValidationMessage(element, elementType, elementId, message, level)
  }

  override def toString = {
    val out = new StringBuilder
    for (v <- validationMessages) {
      out.append(v)
      out.append('\n')
    }

    out.toString()
  }
}

class ValidationMessage(val element: AnyRef, val elementType: String, val elementId: String, val message: String, val level: String) {
  override def toString = level + ": " + elementType + " - " + elementId + " | " + message
}

object ValidationMessage {
  val WARNING = "Warning"
  val ERROR = "Error"

  val RESOURCE_LISTING = "Root Resources Listing"
  val RESOURCE = "Resource"
  val OPERATION = "Operation"
  val OPERATION_PARAM = "Operation Parameter"
  val MODEL = "Model"
  val MODEL_PROPERTY = "Model Property"
}
