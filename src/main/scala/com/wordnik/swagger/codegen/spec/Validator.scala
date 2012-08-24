package com.wordnik.swagger.codegen.spec

import com.wordnik.swagger.codegen.util.{CoreUtils, ApiExtractor, ResourceExtractor}
import com.wordnik.swagger.core.Documentation
import com.wordnik.swagger.codegen.PathUtil
import com.wordnik.swagger.core.util.JsonUtil
import scala.collection.JavaConversions._

/**
 * @author ayush
 * @since 7/6/12 9:03 AM
 *
 */

object Validator extends PathUtil {
  def main(args: Array[String]) {
    if(args.length == 0) {
      throw new RuntimeException("Need url to resources.json as argument. You can also specify VM Argument -DfileMap=/path/to/folder/containing.resources.json/")
    }
    val host = args(0)
    val apiKey = {
      if (args.length > 1) Some("?api_key=" + args(1))
      else None
    }

    val outputFilename = {
      if (args.length > 2) Some(args(2))
      else None
    }
    val doc = {
      try {
        val json = ResourceExtractor.extractListing(getResourcePath(host), apiKey)

        JsonUtil.getJsonMapper.readValue(json, classOf[Documentation])
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host, e)
      }
    }

    val basePath = getBasePath(doc.basePath)
    val subDocs = ApiExtractor.extractApiDocs(basePath, doc.getApis().toList, apiKey)

    val swaggerSpecValidator = new SwaggerSpecValidator(doc, subDocs, false)
    swaggerSpecValidator.validate()
    swaggerSpecValidator.generateReport(host, outputFilename)

    System.exit(0)

  }
}
