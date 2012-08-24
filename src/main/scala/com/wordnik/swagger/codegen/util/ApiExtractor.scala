package com.wordnik.swagger.codegen.util

import com.wordnik.swagger.core.util.JsonUtil

import com.wordnik.swagger.core._

import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import Source._

object ApiExtractor {
  def m = JsonUtil.getJsonMapper

  def extractApiDocs(basePath: String, apis: List[DocumentationEndPoint], apiKey: Option[String]): List[Documentation] = {
    for (api <- apis) yield {
      val json = basePath.startsWith("http") match {
        case true => {
          println("api.path: " + api.path)
          println("calling: " + ((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")))
          Source.fromURL((basePath + api.path + apiKey.getOrElse("")).replaceAll(".\\{format\\}", ".json")).mkString
        }
        case false => Source.fromFile((basePath + api.path).replaceAll(".\\{format\\}", ".json")).mkString
      }
      val out = m.readValue(json, classOf[Documentation])
      out
    }
  }

  def extractOperations(basePath: String, api: DocumentationEndPoint): List[(String, DocumentationOperation)] = {
    (for(op <- api.getOperations.toList) yield (api.path, op)).toList
  }

  def getOperations(path: String, op: List[DocumentationOperation]): Map[String, DocumentationOperation] = {
    val opMap = new HashMap[String, DocumentationOperation]
    op.foreach(operation => opMap += path -> operation)
    opMap.toMap
  }
}