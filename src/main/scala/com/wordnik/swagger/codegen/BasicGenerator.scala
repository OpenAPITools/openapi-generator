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

import com.wordnik.swagger.codegen._
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen.spec.SwaggerSpecValidator
import com.wordnik.swagger.model._
import com.wordnik.swagger.model.SwaggerSerializers
import com.wordnik.swagger.codegen.spec.ValidationMessage

import java.io.{ File, FileWriter }

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import scala.io.Source

abstract class BasicGenerator extends CodegenConfig with PathUtil {
  def packageName = "com.wordnik.client"
  def templateDir = "src/main/resources/scala"
  def destinationDir = "generated-code/src/main/scala"
  def fileSuffix = ".scala"

  override def invokerPackage: Option[String] = Some("com.wordnik.client.common")
  override def modelPackage: Option[String] = Some("com.wordnik.client.model")
  override def apiPackage: Option[String] = Some("com.wordnik.client.api")

  var codegen = new Codegen(this)

  def generateClient(args: Array[String]) = {
    if (args.length == 0) {
      throw new RuntimeException("Need url to resources.json as argument. You can also specify VM Argument -DfileMap=/path/to/folder/containing.resources.json/")
    }
    val host = args(0)
    val apiKey = {
      if (args.length > 1) Some("?api_key=" + args(1))
      else None
    }
    val doc = {
      try {
        ResourceExtractor.fetchListing(getResourcePath(host), apiKey)
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host, e)
      }
    }

    implicit val basePath = getBasePath(doc.basePath)

    val apiReferences = doc.apis
    if (apiReferences == null)
      throw new Exception("No APIs specified by resource")
    val apis = ApiExtractor.fetchApiListings(basePath, apiReferences, apiKey)

    SwaggerSerializers.validationMessages.filter(_.level == ValidationMessage.ERROR).size match {
      case i: Int if i > 0 => {
        println("********* Failed to read swagger json!")
        SwaggerSerializers.validationMessages.foreach(msg => {
          println(msg)
        })
        Option(System.getProperty("skipErrors")) match {
          case Some(str) => println("**** ignoring errors and continuing")
          case None => sys.exit(0)
        }
      }
      case 0 =>
    }

    new SwaggerSpecValidator(doc, apis).validate()

    val allModels = new HashMap[String, Model]
    val operations = extractApiOperations(apis, allModels)
    val operationMap = groupOperationsToFiles(operations)
    val modelBundle = prepareModelMap(allModels.toMap)
    val modelFiles = bundleToSource(modelBundle, modelTemplateFiles.toMap)

    modelFiles.map(m => {
      val filename = m._1

      val file = new java.io.File(filename)
      file.getParentFile().mkdirs

      val fw = new FileWriter(filename, false)
      fw.write(m._2 + "\n")
      fw.close()
      println("wrote model " + filename)
    })

    val apiBundle = prepareApiBundle(operationMap.toMap)
    val apiFiles = bundleToSource(apiBundle, apiTemplateFiles.toMap)

    apiFiles.map(m => {
      val filename = m._1

      val file = new java.io.File(filename)
      file.getParentFile().mkdirs

      val fw = new FileWriter(filename, false)
      fw.write(m._2 + "\n")
      fw.close()
      println("wrote api " + filename)
    })

    codegen.writeSupportingClasses(operationMap, allModels.toMap)
  }

  def extractApiOperations(apiListings: List[ApiListing], allModels: HashMap[String, Model] )(implicit basePath:String) = {
    val output = new ListBuffer[(String, String, Operation)]
    apiListings.foreach(apiDescription => {
      val basePath = apiDescription.basePath
      val resourcePath = apiDescription.resourcePath
      if(apiDescription.apis != null) {
        apiDescription.apis.foreach(api => {
          for ((apiPath, operation) <- ApiExtractor.extractApiOperations(basePath, api)) {
            output += Tuple3(basePath, apiPath, operation)
          }
        })
      }
      output.map(op => processApiOperation(op._2, op._3))
      allModels ++= CoreUtils.extractApiModels(apiDescription)
    })
    output.toList
  }

  /**
   * creates a map of models and properties needed to write source
   */
  def prepareModelMap(models: Map[String, Model]): List[Map[String, AnyRef]] = {
    (for ((name, schema) <- models) yield {
      if (!defaultIncludes.contains(name)) {
        val m = new HashMap[String, AnyRef]
        m += "name" -> toModelName(name)
        m += "className" -> name
        m += "filename" -> toModelFilename(name)
        m += "apis" -> None
        m += "models" -> List((name, schema))
        m += "package" -> modelPackage
        m += "invokerPackage" -> invokerPackage
        m += "outputDirectory" -> (destinationDir + File.separator + modelPackage.getOrElse("").replaceAll("\\.", File.separator))
        m += "newline" -> "\n"

        Some(m.toMap)
      }
      else None
    }).flatten.toList
  }

  def prepareApiBundle(apiMap: Map[(String, String), List[(String, Operation)]] ): List[Map[String, AnyRef]] = {
    (for ((identifier, operationList) <- apiMap) yield {
      val basePath = identifier._1
      val name = identifier._2
      val className = toApiName(name)

      val m = new HashMap[String, AnyRef]

      m += "baseName" -> name
      m += "filename" -> toApiFilename(name)
      m += "name" -> toApiName(name)
      m += "className" -> className
      m += "basePath" -> basePath
      m += "package" -> apiPackage
      m += "invokerPackage" -> invokerPackage
      m += "apis" -> Map(className -> operationList.toList)
      m += "models" -> None
      m += "outputDirectory" -> (destinationDir + File.separator + apiPackage.getOrElse("").replaceAll("\\.", File.separator))
      m += "newline" -> "\n"

      Some(m.toMap)
    }).flatten.toList
  }

  def bundleToSource(bundle:List[Map[String, AnyRef]], templates: Map[String, String]): List[(String, String)] = {
    val output = new ListBuffer[(String, String)]
    bundle.foreach(m => {
      for ((file, suffix) <- templates) {
        output += Tuple2(m("outputDirectory").toString + File.separator + m("filename").toString + suffix, codegen.generateSource(m, file))
      }
    })
    output.toList
  }

  def generateAndWrite(bundle: Map[String, AnyRef], templateFile: String) = {
    val output = codegen.generateSource(bundle, templateFile)
    val outputDir = new File(bundle("outputDirectory").asInstanceOf[String])
    outputDir.mkdirs

    val filename = outputDir + File.separator + bundle("filename")
    val fw = new FileWriter(filename, false)
    fw.write(output + "\n")
    fw.close()
    println("wrote " + filename)
  }

  def groupOperationsToFiles(operations: List[(String, String, Operation)]): Map[(String, String), List[(String, Operation)]] = {
    val opMap = new HashMap[(String, String), ListBuffer[(String, Operation)]]
    for ((basePath, apiPath, operation) <- operations) {
      val className = resourceNameFromFullPath(apiPath)
      val listToAddTo = opMap.getOrElse((basePath, className), {
        val l = new ListBuffer[(String, Operation)]
        opMap += (basePath, className) -> l
        l
      })
      listToAddTo += Tuple2(apiPath, operation)
    }
    opMap.map(m => (m._1, m._2.toList)).toMap
  }
}
