package io.swagger.client.api

import java.io.File

import scala.collection.mutable.HashMap

class WordListsApi(val defBasePath: String = "https://api.wordnik.com/v4",
                   defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value


  def createWordList(body: WordList, auth_token: String): Option[WordList] = {
    // create path and map variables
    val path = "/wordLists.json".replaceAll("\\{format\\}", "json")


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
          Some(ApiInvoker.deserialize(s, "", classOf[WordList]).asInstanceOf[WordList])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

}
