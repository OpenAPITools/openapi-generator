package com.wordnik.petstore.api

import com.wordnik.petstore.model.User
import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException
import scala.collection.mutable.HashMap

class UserApi {
  var basePath: String = "http://petstore.swagger.wordnik.com/api"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def createUsersWithArrayInput (body: Array[User]) = {
    // create path and map variables
    val path = "/user.{format}/createWithArray".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(body) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def createUser (body: User) = {
    // create path and map variables
    val path = "/user.{format}".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(body) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def createUsersWithListInput (body: List[User]) = {
    // create path and map variables
    val path = "/user.{format}/createWithList".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(body) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def updateUser (username: String, body: User) = {
    // create path and map variables
    val path = "/user.{format}/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}",apiInvoker.escapeString(username))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username, body) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "PUT", queryParams.toMap, body, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def deleteUser (username: String) = {
    // create path and map variables
    val path = "/user.{format}/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}",apiInvoker.escapeString(username))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "DELETE", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def getUserByName (username: String) : Option[User]= {
    // create path and map variables
    val path = "/user.{format}/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}",apiInvoker.escapeString(username))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[User]).asInstanceOf[User])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def loginUser (username: String, password: String) : Option[String]= {
    // create path and map variables
    val path = "/user.{format}/login".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(username, password) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(username) != "null") queryParams += "username" -> username.toString
    if(String.valueOf(password) != "null") queryParams += "password" -> password.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[String]).asInstanceOf[String])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def logoutUser () = {
    // create path and map variables
    val path = "/user.{format}/logout".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }

