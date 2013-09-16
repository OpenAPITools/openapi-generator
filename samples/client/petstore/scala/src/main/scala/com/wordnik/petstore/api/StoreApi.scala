package com.wordnik.petstore.api

import com.wordnik.petstore.model.Order
import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException

import java.io.File
import java.util.Date

import scala.collection.mutable.HashMap

class StoreApi {
  var basePath: String = "http://petstore.swagger.wordnik.com/api"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def getOrderById (orderId: String) : Option[Order]= {
    // create path and map variables
    val path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escape(orderId))

    
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(orderId).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
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
    val path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}",apiInvoker.escape(orderId))

    
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(orderId).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
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
  def placeOrder (body: Order) = {
    // create path and map variables
    val path = "/store/order".replaceAll("\\{format\\}","json")
    val contentType = {
      if(body != null && body.isInstanceOf[File] )
        "multipart/form-data"
      else "application/json"
      }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(body).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
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

