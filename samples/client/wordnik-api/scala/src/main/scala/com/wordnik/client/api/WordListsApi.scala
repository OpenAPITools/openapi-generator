package com.wordnik.client.api

import com.wordnik.client.model.WordList
import com.wordnik.client.common.ApiInvoker
import com.wordnik.client.common.ApiException
import scala.collection.mutable.HashMap

class WordListsApi {
  var basePath: String = "http://api.wordnik.com/v4"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def createWordList (body: WordList, auth_token: String) : Option[WordList]= {
    // create path and map variables
    val path = "/wordLists.{format}".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(auth_token) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    headerParams += "auth_token" -> auth_token
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, body, headerParams.toMap) match {
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

