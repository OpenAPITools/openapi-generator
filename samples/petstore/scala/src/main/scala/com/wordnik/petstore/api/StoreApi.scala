package com.wordnik.petstore.api

import com.wordnik.petstore.model.Order
import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException
import scala.collection.mutable.HashMap

class StoreApi {
  var basePath: String = "http://petstore.swagger.wordnik.com/api"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def getOrderById (orderId: String) : Option[Order]= {
    // create path and map variables
    val path = "/store.{format}/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escapeString(orderId))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(orderId) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[Order]).asInstanceOf[Order])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def deleteOrder (orderId: String) = {
    // create path and map variables
    val path = "/store.{format}/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escapeString(orderId))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(orderId) - null).size match {
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
  def placeOrder (body: Order) = {
    // create path and map variables
    val path = "/store.{format}/order".replaceAll("\\{format\\}","json")// query params
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
  }

