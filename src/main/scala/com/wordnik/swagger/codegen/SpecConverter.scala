package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.util.{ ResourceExtractor, ApiExtractor }
import com.wordnik.swagger.model._

import java.io.File

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.{ read, write }

object SpecConverter {
  def main(args: Array[String]) = {
    implicit val formats = SwaggerSerializers.formats("1.2")

    if(args == null || args.length < 2) {
      println("Usage: SpecConverter {host} {outputDir}\nIf no API key is required, use an empty string")
      sys.exit(0)
    }

    val url = args(0)
    val key = None
    val outputDir = new File(args(1))

    if(!outputDir.exists) outputDir.mkdir

    val resourcePath = url.split("/").last
    val resourceListing = ResourceExtractor.fetchListing(url, key)

    resourceListing.swaggerVersion match {
      case "1.1" =>
      case e: String => {
        println("unsupported swagger version %s".format(e))
        sys.exit(0)
      }
    }

    val updatedListing = {
      val apis = (for(api <- resourceListing.apis) yield {
        val path = if(api.path.startsWith("/" + resourcePath)) {
          api.path.substring(resourcePath.length + 1)
        }
        else api.path
        api.copy(path = path.replace(".{format}",""))
      }).toList
      resourceListing.copy(apis = apis, swaggerVersion = "1.2")
    }
    writeToFile(outputDir + File.separator + "api-docs", write(updatedListing))

    val listings = ApiExtractor.fetchApiListings(resourceListing.swaggerVersion, resourceListing.basePath, resourceListing.apis, key)

    listings.foreach(listing => {
      val apis = (for(api <- listing.apis) yield {
        api.copy(path = api.path.replace(".{format}", ""))
      })
      val filename = listing.resourcePath.replace("/","")
      val updatedApi = listing.copy(swaggerVersion = "1.2", apis = apis)
      writeToFile(outputDir + File.separator + filename, write(updatedApi))
    })
  }

  def writeToFile(p: String, s: String) {
    val pw = new java.io.PrintWriter(new File(p))
    try {
      println("writing file %s".format(p))
      pw.write(s)
    } finally {
      pw.close()
    }
  }
}