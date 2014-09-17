/**
 *  Copyright 2014 Wordnik, Inc.
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
import com.wordnik.swagger.codegen.model._
import com.wordnik.swagger.codegen.model.SwaggerSerializers
import com.wordnik.swagger.codegen.spec.ValidationMessage
import com.wordnik.swagger.codegen.spec.SwaggerSpec._
import com.wordnik.swagger.util.ValidationException

import java.io.{ File, FileWriter }

import net.iharder.Base64
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import scala.io.Source

abstract class BasicGenerator extends CodegenConfig with PathUtil {
  implicit val formats = SwaggerSerializers.formats("1.2")

  def packageName = "com.wordnik.client"
  def templateDir = "src/main/resources/scala"
  def destinationDir = "generated-code/src/main/scala"
  def fileSuffix = ".scala"

  override def invokerPackage: Option[String] = Some("com.wordnik.client.common")
  override def modelPackage: Option[String] = Some("com.wordnik.client.model")
  override def apiPackage: Option[String] = Some("com.wordnik.client.api")

  var codegen = new Codegen(this)
  var fileMap: Option[String] = None

  @deprecated(message = "please use the generate function", since = "2.0.16")
  def generateClient(args: Array[String]): Unit = {
    generateClientWithoutExit(args)
    System.exit(0)
  }

  @deprecated(message = "please use the generate function", since = "2.0.16")
  def generateClientWithoutExit(args: Array[String]): Seq[File] = {
    if (args.length == 0) {
      throw new RuntimeException("Need url to resource listing as argument. You can also specify VM Argument -DfileMap=/path/to/folder/containing.resources.json/")
    }
    val host = args(0)
    val apiKey = if(args.length > 1) Some(args(1)) else None
    val authorization = authenticate(apiKey)

    val opts = new ClientOpts()
    opts.uri = host
    opts.auth = authorization
    opts.properties = Map("fileMap" -> sys.props("fileMap"))

    generate(opts)
  }

  def generate(opts: ClientOpts) = {
    if (opts == null) {
      throw new RuntimeException("Need url to resource listing as argument. You can also specify VM Argument -DfileMap=/path/to/folder/containing.resources.json/")
    }
    val host = opts.uri
    val authorization = opts.auth

    fileMap = Option(opts.properties.getOrElse("fileMap", null))
    val doc = ResourceExtractor.fetchListing(getResourcePath(host, fileMap), authorization)

    additionalParams ++= opts.properties
    val apis: List[ApiListing] = getApis(host, doc, authorization)

    val errors = new ListBuffer[ValidationError] ++ SwaggerValidator.validate(doc)
    for(api <- apis)
      SwaggerValidator.validate(api, errors)

    errors.filter(_.severity == SwaggerValidator.ERROR).size match {
      case i: Int if i > 0 => {
        println("********* Failed to read swagger json!")
        errors.foreach(msg => {
          println(msg)
        })
        Option(System.getProperty("skipErrors")) match {
          case Some(str) => println("**** ignoring errors and continuing")
          case None => {
            val out = new StringBuilder
            errors.foreach(m => out.append(m).append("\n"))
            println(errors)
            throw new ValidationException(400, "Failed validation", errors.toList)
          }
        }
      }
      case 0 =>
    }

    implicit val basePath = getBasePath(host, doc.basePath, fileMap)

    new SwaggerSpecValidator(doc, apis).validate()

    val allModels = new HashMap[String, Model]
    val operations = extractApiOperations(apis, allModels)
    val operationMap: Map[(String, String), List[(String, Operation)]] =
      groupOperationsToFiles(operations)

    val modelMap = prepareModelMap(allModels.toMap)
    val modelFileContents = writeFiles(modelMap, modelTemplateFiles.toMap)
    val modelFiles = new ListBuffer[File]()

    for((filename, contents) <- modelFileContents) {
      val file = new java.io.File(filename)
      modelFiles += file
      file.getParentFile().mkdirs
      val fw = new FileWriter(filename, false)
      fw.write(contents + "\n")
      fw.close()
    }

    val apiBundle = prepareApiBundle(operationMap.toMap)
    val apiInfo = writeFiles(apiBundle, apiTemplateFiles.toMap)
    val apiFiles = new ListBuffer[File]()

    apiInfo.map(m => {
      val filename = m._1
      val file = new java.io.File(filename)
      apiFiles += file
      file.getParentFile().mkdirs

      val fw = new FileWriter(filename, false)
      fw.write(m._2 + "\n")
      fw.close()
      println("wrote api " + filename)
    })

    codegen.writeSupportingClasses2(apiBundle, modelMap, doc.apiVersion) ++
      modelFiles ++ apiFiles
  }

  /**
   * applies a template to each of the models
   */
  def writeFiles(models: List[Map[String, AnyRef]], templates: Map[String, String]): List[(String, String)] = {
    val output = new ListBuffer[Tuple2[String, String]]
    models.foreach(m => {
      for ((templateFile, suffix) <- templates) {
        val imports = m.getOrElse("imports", None)
        val filename = m("outputDirectory").toString + File.separator + m("filename").toString + suffix
        output += Tuple2(filename, generateSource(m, templateFile))
      }
    })
    output.toList
  }

  def generateSource(bundle: Map[String, AnyRef], templateFile: String): String = {
    val rootDir = new java.io.File(".")
    val (resourcePath, (engine, template)) = Codegen.templates.getOrElseUpdate(templateFile, codegen.compileTemplate(templateFile, Some(rootDir)))
    var output = engine.layout(resourcePath, template, bundle)

    engine.compiler.shutdown
    output
  }

  def getApis(host: String, doc: ResourceListing, authorization: Option[ApiKeyValue]): List[ApiListing] = {
    implicit val basePath = getBasePath(host, doc.basePath, fileMap)
    println("base path is " + basePath)

    val apiReferences = doc.apis
    if (apiReferences == null)
      throw new Exception("No APIs specified by resource")
    ApiExtractor.fetchApiListings(doc.swaggerVersion, basePath, apiReferences, authorization)
  }

  def authenticate(apiKey: Option[String]): Option[ApiKeyValue] = {
    val headerAuth = sys.props.get("header") map { e =>
      // this is ugly and will be replaced with proper arg parsing like in ScalaAsyncClientGenerator soon
      val authInfo = e.split(":")
      ApiKeyValue(authInfo(0), "header", authInfo(1))
    }
    val basicAuth = sys.props.get("auth.basic") map { e =>
      val creds = if (e.contains(":")) Base64.encodeBytes(e.getBytes) else e
      ApiKeyValue("Authorization", "header", s"Basic $creds")
    }
    val apiKeyAuth = apiKey  map { key =>
      ApiKeyValue("api_key", "query", key)
    }

    headerAuth orElse basicAuth orElse apiKeyAuth
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
    val allImports = new HashSet[String]
    val outputDirectory = (destinationDir + File.separator + modelPackage.getOrElse("").replace(".", File.separator))
    (for ((name, schema) <- models) yield {
      if (!defaultIncludes.contains(name)) {
        val modelMap: Map[String, AnyRef] = codegen.modelToMap(name, schema)

        val imports = modelMap("imports").asInstanceOf[Set[Map[String, AnyRef]]]

        val models: List[Map[String, Map[String, AnyRef]]] = List(Map("model" -> modelMap))
        val m = new HashMap[String, AnyRef]
        m += "imports" -> processImports(imports)
        m += "name" -> toModelName(name)
        m += "className" -> name
        m += "filename" -> toModelFilename(name)
        m += "apis" -> None
        m += "models" -> models
        m += "package" -> modelPackage
        m += "invokerPackage" -> invokerPackage
        m += "outputDirectory" -> outputDirectory
        m += "newline" -> "\n"
        m += "modelPackage" -> modelPackage
        m += "modelJson" -> codegen.writeJson(schema)
        m ++= additionalParams
        Some(m.toMap)
      }
      else None
    }).flatten.toList
  }

  def processImports(ii: Set[Map[String, AnyRef]]) = {
    val allImports = new HashSet[String]()

    ii.foreach(_.map(m => allImports += m._2.asInstanceOf[String]))

    val imports = new ListBuffer[Map[String, String]]
    val includedModels = new HashSet[String]
    val importScope = modelPackage match {
      case Some(s) => s + "."
      case _ => ""
    }
    // do the mapping before removing primitives!
    allImports.foreach(value => {
      val model = toModelName(value.asInstanceOf[String])
      includedModels.contains(model) match {
        case false => {
          importMapping.containsKey(model) match {
            case true => {
              if(!imports.flatten.map(m => m._2).toSet.contains(importMapping(model))) {
                imports += Map("import" -> importMapping(model))
              }
            }
            case false =>
          }
        }
        case true =>
      }
    })

    allImports --= defaultIncludes
    allImports --= primitives
    allImports --= containers
    allImports.foreach(i => {
      val model = toModelName(i)
      includedModels.contains(model) match {
        case false => {
          importMapping.containsKey(model) match {
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
    imports
  }

  def prepareApiBundle(apiMap: Map[(String, String), List[(String, Operation)]] ): List[Map[String, AnyRef]] = {
    (for ((identifier, operationList) <- apiMap) yield {
      val basePath = identifier._1
      val name = identifier._2
      val className = toApiName(name)
      val allImports = new HashSet[String]
      val operations = new ListBuffer[AnyRef]
      val o = new ListBuffer[AnyRef]

      val classNameToOperationList = new HashMap[String, ListBuffer[AnyRef]]
      for ((apiPath, operation) <- operationList) {
        CoreUtils.extractModelNames(operation).foreach(i => allImports += i)
      }
      val imports = new ListBuffer[Map[String, String]]
      val includedModels = new HashSet[String]
      val modelList = new ListBuffer[Map[String, AnyRef]]
      val importScope = modelPackage match {
        case Some(s) => s + "."
        case None => ""
      }

      allImports --= defaultIncludes
      allImports --= primitives
      allImports --= containers
      allImports.foreach(i => {
        val model = toModelName(i)
        if(!includedModels.contains(model) && !importMapping.containsKey(model)) {
          if(!imports.flatten.map(m => m._2).toSet.contains(importScope + model)){
            imports += Map("import" -> (importScope + model))
          }
        }
      })

      val names = new HashSet[String]
      for((path, operation) <- operationList) {
        val op = codegen.apiToMap(path, operation)
        val nickname = op.getOrElse("nickname", op("httpMethod")).asInstanceOf[String]
        var updatedNickname = nickname
        if(names.contains(nickname)) {
          var counter = 0
          var done = false
          while(!done) {
            updatedNickname = nickname + "_" + className + "_" + counter
            if(!names.contains(updatedNickname)) done = true
            counter += 1
          }
        }
        names += updatedNickname
        o += (Map("path" -> path) ++ op ++ Map("nickname" -> updatedNickname))
      }
      operations += Map("operation" -> o)

      val m = new HashMap[String, AnyRef]
      m += "imports" -> imports
      m += "baseName" -> name
      m += "filename" -> toApiFilename(name)
      m += "name" -> toApiName(name)
      m += "classname" -> className
      m += "className" -> className
      m += "basePath" -> basePath
      m += "package" -> apiPackage
      m += "invokerPackage" -> invokerPackage
      m += "operations" -> operations
      m += "models" -> None
      m += "outputDirectory" -> (destinationDir + File.separator + apiPackage.getOrElse("").replace(".", File.separator))
      m += "newline" -> "\n"
      m += "modelPackage" -> modelPackage

      m ++= additionalParams
      
      Some(m.toMap)
    }).flatten.toList
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
