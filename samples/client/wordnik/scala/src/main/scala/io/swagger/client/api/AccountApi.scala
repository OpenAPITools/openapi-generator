package io.swagger.client.api

import java.io.File

import scala.collection.mutable.HashMap

class AccountApi(val defBasePath: String = "https://api.wordnik.com/v4",
                 defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value


  def getApiTokenStatus(api_key: String): Option[ApiTokenStatus] = {
    // create path and map variables
    val path = "/account.json/apiTokenStatus".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]





    headerParams += "api_key" -> api_key


    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[ApiTokenStatus]).asInstanceOf[ApiTokenStatus])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def authenticate(username: String, password: String): Option[AuthenticationToken] = {
    // create path and map variables
    val path = "/account.json/authenticate/{username}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escape(username))




    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(password) != "null") queryParams += "password" -> password.toString




    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[AuthenticationToken]).asInstanceOf[AuthenticationToken])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def authenticatePost(username: String, body: String): Option[AuthenticationToken] = {
    // create path and map variables
    val path = "/account.json/authenticate/{username}".replaceAll("\\{format\\}", "json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escape(username))




    val contentType = {
      if (body != null && body.isInstanceOf[File])
        "multipart/form-data"
      else "application/json"


    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]







    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[AuthenticationToken]).asInstanceOf[AuthenticationToken])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def getLoggedInUser(auth_token: String): Option[User] = {
    // create path and map variables
    val path = "/account.json/user".replaceAll("\\{format\\}", "json")


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
          Some(ApiInvoker.deserialize(s, "", classOf[User]).asInstanceOf[User])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

  def getWordListsForLoggedInUser(auth_token: String, skip: Integer, limit: Integer): Option[List[WordList]] = {
    // create path and map variables
    val path = "/account.json/wordLists".replaceAll("\\{format\\}", "json")


    val contentType = {

      "application/json"
    }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]



    if (String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if (String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString


    headerParams += "auth_token" -> auth_token


    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "array", classOf[WordList]).asInstanceOf[List[WordList]])

        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }

}
