package com.wordnik.petstore.api

import java.io.File
import com.wordnik.petstore.model.Pet
import com.wordnik.client.ApiInvoker
import com.wordnik.client.ApiException

import java.io.File
import java.util.Date

import scala.collection.mutable.HashMap

class PetApi {
  var basePath: String = "http://petstore.swagger.wordnik.com/api"
  var apiInvoker = ApiInvoker
  
  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  def getPetById (petId: Long) : Option[Pet]= {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(petId).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "", classOf[Pet]).asInstanceOf[Pet])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def deletePet (petId: String) = {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(petId).filter(_ != null)).size match {
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
  def partialUpdate (petId: String, body: Pet) : Option[List[Pet]]= {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    
    val contentType = {
      if(body != null && body.isInstanceOf[File] )
        "multipart/form-data"
      else "application/json"
      }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(petId, body).filter(_ != null)).size match {
       case 2 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "PATCH", queryParams.toMap, body, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "Array", classOf[Pet]).asInstanceOf[List[Pet]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def updatePetWithForm (petId: String, name: String, status: String) = {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(petId).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def uploadFile (additionalMetadata: String, body: File) = {
    // create path and map variables
    val path = "/pet/uploadImage".replaceAll("\\{format\\}","json")
    val contentType = {
      if(body != null && body.isInstanceOf[File] )
        "multipart/form-data"
      else "application/json"
      }

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

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
  def addPet (body: Pet) = {
    // create path and map variables
    val path = "/pet".replaceAll("\\{format\\}","json")
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
  def updatePet (body: Pet) = {
    // create path and map variables
    val path = "/pet".replaceAll("\\{format\\}","json")
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
      apiInvoker.invokeApi(basePath, path, "PUT", queryParams.toMap, body, headerParams.toMap, contentType) match {
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
    val path = "/pet/findByStatus".replaceAll("\\{format\\}","json")
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(status).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(status) != "null") queryParams += "status" -> status.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "Array", classOf[Pet]).asInstanceOf[List[Pet]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  def findPetsByTags (tags: String) : Option[List[Pet]]= {
    // create path and map variables
    val path = "/pet/findByTags".replaceAll("\\{format\\}","json")
    val contentType = {
      "application/json"}

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]

    // verify required params are set
    (List(tags).filter(_ != null)).size match {
       case 1 => // all required values set
       case _ => throw new Exception("missing required params")
    }
    if(String.valueOf(tags) != "null") queryParams += "tags" -> tags.toString
    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, None, headerParams.toMap, contentType) match {
        case s: String =>
          Some(ApiInvoker.deserialize(s, "Array", classOf[Pet]).asInstanceOf[List[Pet]])
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  }

