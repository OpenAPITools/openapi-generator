package com.wordnik.client.api

import com.wordnik.client.model.WordList
import com.wordnik.client.model.StringValue
import com.wordnik.client.model.WordListWord
import com.wordnik.client.common.ApiInvoker
import com.wordnik.client.common.ApiException
import scala.collection.mutable.HashMap

class WordListApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def updateWordList (permalink: String, body: WordList, auth_token: String) = {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
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
  def deleteWordList (permalink: String, auth_token: String) = {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
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
  def getWordListByPermalink (permalink: String, auth_token: String) : Option[WordList]= {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[WordList]).asInstanceOf[WordList])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def addWordsToWordList (permalink: String, body: Array[StringValue], auth_token: String) = {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}/words".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
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
  def getWordListWords (permalink: String, auth_token: String, sortBy: String= "createDate", sortOrder: String= "desc", skip: Int= 0, limit: Int= 100) : Option[List[WordListWord]]= {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}/words".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(sortBy) != "null") queryParams += "sortBy" -> sortBy.toString
    if(String.valueOf(sortOrder) != "null") queryParams += "sortOrder" -> sortOrder.toString
    if(String.valueOf(skip) != "null") queryParams += "skip" -> skip.toString
    if(String.valueOf(limit) != "null") queryParams += "limit" -> limit.toString
    headerParams += "auth_token" -> auth_token
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[WordListWord]).asInstanceOf[List[WordListWord]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def deleteWordsFromWordList (permalink: String, body: Array[StringValue], auth_token: String) = {
    // create path and map variables
    val path = "/wordList.{format}/{permalink}/deleteWords".replaceAll("\\{format\\}","json").replaceAll("\\{" + "permalink" + "\\}",apiInvoker.escapeString(permalink))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(permalink, auth_token) - null).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
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
  }

