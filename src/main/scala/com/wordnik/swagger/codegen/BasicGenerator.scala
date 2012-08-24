package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen._
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util.JsonUtil
import java.io.{ File, FileWriter }

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import scala.io.Source
import spec.SwaggerSpecValidator

abstract class BasicGenerator extends CodegenConfig with PathUtil {
  def packageName = "com.wordnik.client"
  def templateDir = "src/main/resources/scala"
  def destinationDir = "generated-code/src/main/scala"
  def fileSuffix = ".scala"

  override def modelPackage: Option[String] = Some("com.wordnik.client.model")
  override def apiPackage: Option[String] = Some("com.wordnik.client.api")

  var codegen = new Codegen(this)
  def m = JsonUtil.getJsonMapper

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
        val json = ResourceExtractor.extractListing(getResourcePath(host), apiKey)
        m.readValue(json, classOf[Documentation])
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host, e)
      }
    }

    val basePath = getBasePath(doc.basePath)

    val apis = doc.getApis
    if (apis == null)
      throw new Exception("No APIs specified by resource")
    val subDocs = ApiExtractor.extractApiDocs(basePath, apis.toList, apiKey)

    val models = CoreUtils.extractAllModels(subDocs)

    new SwaggerSpecValidator(doc, subDocs).validate()

    val allModels = new HashMap[String, DocumentationSchema]
    val operations = new ListBuffer[(String, String, DocumentationOperation)]

    subDocs.foreach(subDoc => {
      val basePath = subDoc.basePath
      val resourcePath = subDoc.resourcePath
      if (subDoc.getApis != null) {
        subDoc.getApis.foreach(api => {
          for ((apiPath, operation) <- ApiExtractor.extractOperations(doc.basePath, api)) {
            operations += Tuple3(basePath, apiPath, operation)
          }
        })

        operations.map(op => processOperation(op._2, op._3))
        allModels ++= CoreUtils.extractModels(subDoc)
      }
    })

    val apiMap = groupApisToFiles(operations.toList)

    for ((identifier, operationList) <- apiMap) {
      val basePath = identifier._1
      val className = identifier._2
      val map = new HashMap[String, AnyRef]
      map += "basePath" -> basePath
      map += "package" -> apiPackage
      map += "invokerPackage" -> Some(packageName)
      map += "apis" -> Map(className -> operationList.toList)
      map += "models" -> None
      map += "outputDirectory" -> (destinationDir + File.separator + apiPackage.getOrElse("").replaceAll("\\.", File.separator))
      map += "newline" -> "\n"
      for ((file, suffix) <- apiTemplateFiles) {
        map += "filename" -> (className + suffix)
        generateAndWrite(map.toMap, file)
      }
    }

    val modelBundleList = new ListBuffer[Map[String, AnyRef]]
    for ((name, schema) <- allModels) {
      val map = new HashMap[String, AnyRef]
      map += "apis" -> None
      map += "models" -> List((name, schema))
      map += "package" -> modelPackage
      map += "invokerPackage" -> Some(packageName)
      map += "outputDirectory" -> (destinationDir + File.separator + modelPackage.getOrElse("").replaceAll("\\.", File.separator))
      map += "newline" -> "\n"
      modelBundleList += map.toMap
      for ((file, suffix) <- modelTemplateFiles) {
        map += "filename" -> (name + suffix)
        generateAndWrite(map.toMap, file)
      }
    }
    codegen.writeSupportingClasses
  }

  def apiNameFromPath(apiPath: String) = makeApiNameFromPath(apiPath)

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

  def groupApisToFiles(operations: List[(String /*basePath*/ , String /*apiPath*/ , DocumentationOperation /* operation*/ )]): Map[(String, String), ListBuffer[(String, DocumentationOperation)]] = {
    val opMap = new HashMap[(String, String), ListBuffer[(String, DocumentationOperation)]]
    for ((basePath, apiPath, operation) <- operations) {
      val className = apiNameFromPath(apiPath)
      val listToAddTo = opMap.getOrElse((basePath, className), {
        val l = new ListBuffer[(String, DocumentationOperation)]
        opMap += (basePath, className) -> l
        l
      })
      listToAddTo += Tuple2(apiPath, operation)
    }
    opMap.toMap
  }
}
