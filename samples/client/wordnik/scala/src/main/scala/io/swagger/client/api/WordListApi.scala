package io.swagger.client.api

import java.io.File

import scala.collection.mutable.HashMap

class WordListApi(val defBasePath: String = "https://api.wordnik.com/v4",
                  defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value


  def getWordListByPermalink(permalink: String, auth_token: String): Option[WordList] = {
    // create path and map variables
    val path = "/wordList.json/{permalink}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordList]).asInstanceOf[WordList])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def updateWordList(permalink: String, body: WordList, auth_token: String) = {
    // create path and map variables
    val path = "/wordList.json/{permalink}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {
      if (body != null && body.isInstanceOf[File])
        "multipart/form-data"
      else "application/json"


    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "PUT", queryParams.toMap, body, headerParams.toMap, contentType) match {
        case s: String =>

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def deleteWordList(permalink: String, auth_token: String) = {
    // create path and map variables
    val path = "/wordList.json/{permalink}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "DELETE", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def deleteWordsFromWordList(permalink: String, body: List[StringValue], auth_token: String) = {
    // create path and map variables
    val path = "/wordList.json/{permalink}/deleteWords".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {
      if (body != null && body.isInstanceOf[File])
        "multipart/form-data"
      else "application/json"


    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap, contentType) match {
        case s: String =>

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def getWordListWords(permalink: String, sortBy: String, sortOrder: String, skip: Integer, limit: Integer, auth_token: String) = {
    // create path and map variables
    val path = "/wordList.json/{permalink}/words".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(sortBy) != "null") queryParams += "sortBy" -> sortBy.toString
    if (String.valueOf(sortOrder) != "null") queryParams += "sortOrder" -> sortOrder.toString
    if (String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if (String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString


    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def addWordsToWordList(permalink: String, body: List[StringValue], auth_token: String) = {
    // create path and map variables
    val path = "/wordList.json/{permalink}/words".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "permalink" + "\\}", apiInvoker.escape(permalink))




    val contentType = {
      if (body != null && body.isInstanceOf[File])
        "multipart/form-data"
      else "application/json"


    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap, contentType) match {
        case s: String =>

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

}
