package com.wordnik.swagger.codegen.util

import scala.io.Source

object ResourceExtractor {
  def extractListing(path: String, apiKey: Option[String]) = {
	path.startsWith("http") match {
	  case true => Source.fromURL(path + apiKey.getOrElse("")).mkString
	  case false => Source.fromFile(path).mkString
	}
  }
}