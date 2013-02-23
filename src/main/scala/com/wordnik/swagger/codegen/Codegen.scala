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

package com.wordnik.swagger.codegen

import com.wordnik.swagger.model._
import com.wordnik.swagger.codegen.util.CoreUtils
import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

import org.fusesource.scalate._
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.mustache._
import org.fusesource.scalate.support.ScalaCompiler

import java.io.{ File, FileWriter, InputStream }

import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.collection.mutable.{ HashMap, ListBuffer, HashSet }
import scala.collection.JavaConversions._

object Codegen {
  val templates = new HashMap[String, (TemplateEngine, Template)]
}

class Codegen(config: CodegenConfig) {
  implicit val formats = SwaggerSerializers.formats

  def generateSource(bundle: Map[String, AnyRef], templateFile: String): String = {
    val allImports = new HashSet[String]
    val includedModels = new HashSet[String]
    val modelList = new ListBuffer[Map[String, AnyRef]]
    val models = bundle("models")

    models match {
      case e: List[Tuple2[String, Model]] => {
        e.foreach(m => {
          includedModels += m._1
          val modelMap = modelToMap(m._1, m._2)
          modelMap.getOrElse("imports", None) match {
            case im: Set[Map[String, String]] => im.foreach(m => m.map(e => allImports += e._2))
            case None =>
          }
          modelList += modelMap
        })
      }
      case None =>
    }

    val modelData = Map[String, AnyRef]("model" -> modelList.toList)
    val operationList = new ListBuffer[Map[String, AnyRef]]
    val classNameToOperationList = new HashMap[String, ListBuffer[AnyRef]]
    val apis = bundle("apis")
    apis match {
      case a: Map[String, List[(String, Operation)]] => {
        a.map(op => {
          val classname = op._1
          val ops = op._2
          for ((apiPath, operation) <- ops) {
            val opList = classNameToOperationList.getOrElse(classname, {
              val lb = new ListBuffer[AnyRef]
              classNameToOperationList += classname -> lb
              lb
            })
            opList += apiToMap(apiPath, operation)
            CoreUtils.extractModelNames(operation).foreach(i => allImports += i)
          }
        })
      }
      case None =>
    }

    val f = new ListBuffer[AnyRef]
    classNameToOperationList.map(m => f += Map("classname" -> m._1, "operation" -> m._2))

    val imports = new ListBuffer[Map[String, String]]
    val importScope = config.modelPackage match {
      case Some(s) => s + "."
      case None => ""
    }
    // do the mapping before removing primitives!
    allImports.foreach(i => {
      val model = config.toModelName(i)
      includedModels.contains(model) match {
        case false => {
          config.importMapping.containsKey(model) match {
            case true => {
              if(!imports.flatten.map(m => m._2).toSet.contains(config.importMapping(model))) {
                imports += Map("import" -> config.importMapping(model))
              }
            }
            case false =>
          }
        }
        case true =>
      }
    })
    allImports --= config.defaultIncludes
    allImports --= primitives
    allImports --= containers
    allImports.foreach(i => {
      val model = config.toModelName(i)
      includedModels.contains(model) match {
        case false => {
          config.importMapping.containsKey(model) match {
            case true =>
            case false => {
              if(!imports.flatten.map(m => m._2).toSet.contains(importScope + model)){
                imports += Map("import" -> (importScope + model))
              }
            }
          }
        }
        case true => // no need to add the model
      }
    })

    val rootDir = new java.io.File(".")
    val engineData = Codegen.templates.getOrElse(templateFile, {
      val engine = new TemplateEngine(Some(rootDir))
      val srcName = config.templateDir + File.separator + templateFile
      val srcStream = {
        getClass.getClassLoader.getResourceAsStream(srcName) match {
          case is: java.io.InputStream => is
          case _ => {
            val f = new java.io.File(srcName)
            if (!f.exists) throw new Exception("Missing template: " + srcName)
            else new java.io.FileInputStream(f)
          }
        }
      }
      val template = engine.compile(
        TemplateSource.fromText(config.templateDir + File.separator + templateFile,
          Source.fromInputStream(srcStream).mkString))
      val t = Tuple2(engine, template)
      Codegen.templates += templateFile -> t
      t
    })

    val engine = engineData._1
    val template = engineData._2

    val requiredModels = {
      for(i <- allImports) yield {
        HashMap("name" -> i, "hasMore" -> "true")
      }
    }.toList

    requiredModels.size match {
      case i if (i > 0) => requiredModels.last += "hasMore" -> "false"
      case _ =>
    }

    var data = Map[String, AnyRef](
      "name" -> bundle("name"),
      "package" -> bundle("package"),
      "baseName" -> bundle.getOrElse("baseName", None),
      "className" -> bundle("className"),
      "invokerPackage" -> bundle("invokerPackage"),
      "imports" -> imports,
      "requiredModels" -> requiredModels,
      "operations" -> f,
      "models" -> modelData,
      "basePath" -> bundle.getOrElse("basePath", ""))
    var output = engine.layout(config.templateDir + File.separator + templateFile, template, data.toMap)

    //  a shutdown method will be added to scalate in an upcoming release
    engine.compiler.shutdown
    output
  }

  def allowableValuesToString(v: AllowableValues) = {
    v match {
      case av: AllowableListValues => {
        Some(av.values.mkString("LIST[", ",", "]"))
      }
      case av: AllowableRangeValues => {
        Some("RANGE[" + av.min + "," + av.max + "]")
      }
      case _ => None
    }
  }

  def apiToMap(path: String, operation: Operation): Map[String, AnyRef] = {
    var bodyParam: Option[String] = None
    var queryParams = new ListBuffer[AnyRef]
    val pathParams = new ListBuffer[AnyRef]
    val headerParams = new ListBuffer[AnyRef]
    val bodyParams = new ListBuffer[AnyRef]
    var paramList = new ListBuffer[HashMap[String, AnyRef]]

    if (operation.parameters != null) {
      operation.parameters.foreach(param => {
        val params = new HashMap[String, AnyRef]
        params += (param.paramType + "Parameter") -> "true"
        params += "type" -> param.paramType
        params += "defaultValue" -> config.toDefaultValue(param.dataType, param.defaultValue)
        params += "dataType" -> config.toDeclaredType(param.dataType)
        params += "swaggerDataType" -> param.dataType
        params += "description" -> param.description
        params += "hasMore" -> "true"
        params += "allowMultiple" -> param.allowMultiple.toString

        param.allowableValues match {
          case a: AllowableValues => params += "allowableValues" -> allowableValuesToString(a)
          case _ =>
        }

        if (!param.required) {
          params += "optional" -> "true"
        }
        param.paramType match {
          case "body" => {
            params += "paramName" -> "body"
            params += "baseName" -> "body"
            param.required match {
              case true => params += "required" -> "true"
              case _ =>
            }
            bodyParam = Some("body")
            bodyParams += params.clone
          }
          case "path" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> "true"
            pathParams += params.clone
          }
          case "query" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> param.required.toString
            queryParams += params.clone
          }
          case "header" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> param.required.toString
            headerParams += params.clone
          }
          case x @ _ => throw new Exception("Unknown parameter type: " + x)
        }
        paramList += params
      })
    }

    val requiredParams = new ListBuffer[HashMap[String, AnyRef]]
    paramList.filter(p => p.contains("required") && p("required") == "true").foreach(param => {
      requiredParams += HashMap(
        "paramName" -> param("paramName"),
        "defaultValue" -> param("defaultValue"),
        "baseName" -> param("baseName"),
        "hasMore" -> "true")
    })
    requiredParams.size match {
      case 0 =>
      case _ => requiredParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    queryParams.size match {
      case 0 =>
      case _ => queryParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    pathParams.size match {
      case 0 =>
      case _ => pathParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    val sp = {
      val lb = new ListBuffer[AnyRef]
      paramList.foreach(i => {
        i += "secondaryParam" -> "true"
        i("defaultValue") match {
          case Some(e) =>
          case None => lb += i
        }
      })
      paramList.foreach(i => {
        i("defaultValue") match {
          case Some(e) => lb += i
          case None =>
        }
      })
      lb.toList
    }

    paramList.size match {
      case 0 =>
      case _ => {
        sp.head.asInstanceOf[HashMap[String, String]] -= "secondaryParam"
        sp.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }
    }

    val properties =
      HashMap[String, AnyRef](
        "path" -> path,
        "nickname" -> config.toMethodName(operation.nickname),
        "summary" -> operation.summary,
        "notes" -> operation.notes,
        "deprecated" -> operation.`deprecated`,
        "bodyParam" -> bodyParam,
        "allParams" -> sp,
        "bodyParams" -> bodyParams.toList,
        "pathParams" -> pathParams.toList,
        "queryParams" -> queryParams.toList,
        "headerParams" -> headerParams.toList,
        "requiredParams" -> requiredParams.toList,
        "httpMethod" -> operation.httpMethod.toUpperCase,
        operation.httpMethod.toLowerCase -> "true")
    if (requiredParams.size > 0) properties += "requiredParamCount" -> requiredParams.size.toString
    operation.responseClass.indexOf("[") match {
      case -1 => {
        val baseType = operation.responseClass
        properties += "returnType" -> config.processResponseDeclaration(baseType)
        properties += "returnBaseType" -> config.processResponseClass(baseType)
        properties += "returnSimpleType" -> "true"
        properties += "returnTypeIsPrimitive" -> {
          (config.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)) match {
            case true => Some("true")
            case _ => None
          }
        }
      }
      case n: Int => {
        val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
        val ComplexTypeMatcher(basePart) = operation.responseClass

        properties += "returnType" -> config.processResponseDeclaration(operation.responseClass.replaceAll(basePart, config.processResponseClass(basePart).get))
        properties += "returnContainer" -> (operation.responseClass.substring(0, n))
        properties += "returnBaseType" -> config.processResponseClass(basePart)
        properties += "returnTypeIsPrimitive" -> {
          (config.languageSpecificPrimitives.contains(basePart) || primitives.contains(basePart)) match {
            case true => Some("true")
            case _ => None
          }
        }
      }
    }
    config.processApiMap(properties.toMap)
  }

  def modelToMap(className: String, model: Model): Map[String, AnyRef] = {
    val data: HashMap[String, AnyRef] =
      HashMap(
        "classname" -> config.toModelName(className),
        "classVarName" -> config.toVarName(className), // suggested name of object created from this class
        "modelPackage" -> config.modelPackage,
        "newline" -> "\n")

    val l = new ListBuffer[AnyRef]

    val imports = new HashSet[AnyRef]
    model.properties.map(prop => {
      val propertyDocSchema = prop._2
      val dt = propertyDocSchema.`type`

      var baseType = dt
      // import the object inside the container
      if (propertyDocSchema.items != null) {
        // import the container
        imports += Map("import" -> dt)
        propertyDocSchema.items match {
          case Some(items) => baseType = items.ref.getOrElse(items.`type`)
          case _ =>
        }
      }
      baseType = config.typeMapping.contains(baseType) match {
        case true => config.typeMapping(baseType)
        case false => {
          imports += Map("import" -> config.typeMapping.getOrElse(baseType, baseType))
          baseType
        }
      }
      (config.defaultIncludes ++ config.languageSpecificPrimitives).toSet.contains(baseType) match {
        case true =>
        case _ => imports += Map("import" -> baseType)
      }

      val isList = (if (isListType(propertyDocSchema.`type`)) true else None)
      val isMap = (if (isMapType(propertyDocSchema.`type`)) true else None)
      val isNotContainer = if (!isListType(propertyDocSchema.`type`) && !isMapType(propertyDocSchema.`type`)) true else None
      val isContainer = if (isListType(propertyDocSchema.`type`) || isMapType(propertyDocSchema.`type`)) true else None

      val properties =
        HashMap(
          "name" -> config.toVarName(prop._1),
          "nameSingular" -> {
            val name = config.toVarName(prop._1)
            if (name.endsWith("s") && name.length > 1) name.substring(0, name.length - 1) else name
          },
          "baseType" -> {
            if (primitives.contains(baseType))
              baseType
            else
              config.modelPackage match {
                case Some(p) => p + "." + baseType
                case _ => baseType
              }
          },
          "baseTypeVarName" -> config.toVarName(baseType),
          "baseName" -> prop._1,
          "datatype" -> config.toDeclaration(propertyDocSchema)._1,
          "defaultValue" -> config.toDeclaration(propertyDocSchema)._2,
          "description" -> propertyDocSchema.description,
          "notes" -> propertyDocSchema.description,
          "required" -> propertyDocSchema.required.toString,
          "getter" -> config.toGetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "setter" -> config.toSetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "isList" -> isList,
          "isMap" -> isMap,
          "isContainer" -> isContainer,
          "isNotContainer" -> isNotContainer,
          "hasMore" -> "true")
      (config.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)) match {
        case true => properties += "isPrimitiveType" -> "true"
        case _ => properties += "complexType" -> config.toModelName(baseType)
      }

      l += properties
    })
    l.size match {
      case 0 =>
      case _ => l.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }
    data += "vars" -> l
    data += "imports" -> imports.toSet
    config.processModelMap(data.toMap)
  }

  /**
   * gets an input stream from resource or file
   */
  def getInputStream(path: String): InputStream = {
    getClass.getClassLoader.getResourceAsStream(path) match {
      case is: InputStream => is
      case _ => new java.io.FileInputStream(path)
    }
  }

  def writeSupportingClasses(apis: Map[(String, String), List[(String, Operation)]], models: Map[String, Model]) = {
    val rootDir = new java.io.File(".")
    val engine = new TemplateEngine(Some(rootDir))

    val apiList = new ListBuffer[Map[String, AnyRef]]

    apis.foreach(a => {
      apiList += Map(
        "name" -> a._1._2,
        "filename" -> config.toApiFilename(a._1._2),
        "className" -> config.toApiName(a._1._2),
        "basePath" -> a._1._1,
        "operations" -> {
          (for (t <- a._2) yield { Map("operation" -> t._2, "path" -> t._1) }).toList
        })
    })

    val modelList = new ListBuffer[HashMap[String, AnyRef]]

    models.foreach(m => {
      val json = write(m._2)

      modelList += HashMap(
        "modelName" -> m._1,
        "model" -> modelToMap(m._1, m._2),
        "filename" -> config.toModelFilename(m._1),
        "modelJson" -> json,
        "hasMore" -> "true")
    })
    modelList.size match {
      case 0 =>
      case _ => modelList.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    val data: HashMap[String, AnyRef] =
      HashMap(
        "invokerPackage" -> config.invokerPackage,
        "package" -> config.packageName,
        "modelPackage" -> config.modelPackage,
        "apiPackage" -> config.apiPackage,
        "apis" -> apiList,
        "models" -> modelList)

    config.supportingFiles.map(file => {
      val supportingFile = file._1
      val outputDir = file._2
      val destFile = file._3

      val outputFilename = outputDir + File.separator + destFile
      val outputFolder = new File(outputFilename).getParent
      new File(outputFolder).mkdirs

      if (supportingFile.endsWith(".mustache")) {
        val output = {
          val resourceName = config.templateDir + File.separator + supportingFile
          val is = getInputStream(resourceName)
          if (is == null)
            throw new Exception("Resource not found: " + resourceName)
          val template = engine.compile(
            TemplateSource.fromText(resourceName,
              Source.fromInputStream(is).mkString))
          engine.layout(resourceName, template, data.toMap)
        }
        val fw = new FileWriter(outputFilename, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + outputFilename)
      } else {
        val file = new File(config.templateDir + File.separator + supportingFile)
        if(file.isDirectory()) {
          // copy the whole directory
          FileUtils.copyDirectory(file, new File(outputDir))
          println("copied directory " + supportingFile)
        } else {
          val is = getInputStream(config.templateDir + File.separator + supportingFile)
          val outputFile = new File(outputFilename)
          val parentDir = new File(outputFile.getParent)
          if (parentDir != null && !parentDir.exists) {
            println("making directory: " + parentDir.toString + ": " + parentDir.mkdirs)
          }
          FileUtils.copyInputStreamToFile(is, new File(outputFilename))
          println("copied " + outputFilename)
          is.close
        }
      }
    })
    //a shutdown method will be added to scalate in an upcoming release
    engine.compiler.shutdown()
  }

  protected def isListType(dt: String) = isCollectionType(dt, "List") || isCollectionType(dt, "Array")

  protected def isMapType(dt: String) = isCollectionType(dt, "Map")

  protected def isCollectionType(dt: String, str: String) = {
    if (dt.equals(str))
      true
    else
      dt.indexOf("[") match {
        case -1 => false
        case n: Int => {
          if (dt.substring(0, n) == str) {
            true
          } else false
        }
      }
  }
}
