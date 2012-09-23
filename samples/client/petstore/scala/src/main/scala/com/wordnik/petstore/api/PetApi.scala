package com.wordnik.petstore.api

import com.wordnik.petstore.model.Pet
import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException
import scala.collection.mutable.HashMap

class PetApi {
  var basePath: String = "http://petstore.swagger.wordnik.com/api"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def getPetById (petId: String) : Option[Pet]= {
    // create path and map variables
    val path = "/pet.{format}/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escapeString(petId))

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(petId) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[Pet]).asInstanceOf[Pet])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def addPet (body: Pet) = {
    // create path and map variables
    val path = "/pet.{format}".replaceAll("\\{format\\}","json")// query params
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
  def updatePet (body: Pet) = {
    // create path and map variables
    val path = "/pet.{format}".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(body) - null).size match {
       case 1 => // all required values set
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
  def findPetsByStatus (status: String= "available") : Option[List[Pet]]= {
    // create path and map variables
    val path = "/pet.{format}/findByStatus".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(status) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(status) != "null") queryParams += "status" -> status.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Pet]).asInstanceOf[List[Pet]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def findPetsByTags (tags: String) : Option[List[Pet]]= {
    // create path and map variables
    val path = "/pet.{format}/findByTags".replaceAll("\\{format\\}","json")// query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (Set(tags) - null).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(tags) != "null") queryParams += "tags" -> tags.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "List", classOf[Pet]).asInstanceOf[List[Pet]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }

