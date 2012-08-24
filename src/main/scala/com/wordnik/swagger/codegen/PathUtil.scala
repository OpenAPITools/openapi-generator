package com.wordnik.swagger.codegen

/**
 * @author ayush
 * @since 7/6/12 9:06 AM
 *
 */

trait PathUtil {
  def getResourcePath(host: String) = {
    System.getProperty("fileMap") match {
      case s: String => {
        s + "/resources.json"
      }
      case _ => host
    }
  }

  def getBasePath(basePath: String) = {
    System.getProperty("fileMap") match {
      case s: String => s
      case _ => basePath
    }
  }

  def makeApiNameFromPath(apiPath: String) = {
    val name = apiPath.split("/")(1).split("\\.")(0).replaceAll("/", "")
    name.charAt(0).toUpperCase + name.substring(1) + "Api"
  }
}
