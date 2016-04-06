package io.swagger.client.api

import io.swagger.client.model.Pet
import io.swagger.client.model.InlineResponse200
import java.io.File
import io.swagger.client.ApiInvoker
import io.swagger.client.ApiException

import com.sun.jersey.multipart.FormDataMultiPart
import com.sun.jersey.multipart.file.FileDataBodyPart

import javax.ws.rs.core.MediaType

import java.io.File
import java.util.Date

import scala.collection.mutable.HashMap

class PetApi(val defBasePath: String = "http://petstore.swagger.io/v2",
                        defApiInvoker: ApiInvoker = ApiInvoker) {
  var basePath = defBasePath
  var apiInvoker = defApiInvoker

  def addHeader(key: String, value: String) = apiInvoker.defaultHeaders += key -> value 

  
  /**
   * Add a new pet to the store
   * 
   * @param body Pet object that needs to be added to the store (optional)
   * @return void
   */
  def addPet (body: Pet)  = {
    // create path and map variables
    val path = "/pet".replaceAll("\\{format\\}","json")

    val contentTypes = List("application/json", "application/xml", "application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = body

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Fake endpoint to test byte array in body parameter for adding a new pet to the store
   * 
   * @param body Pet object in the form of byte array (optional)
   * @return void
   */
  def addPetUsingByteArray (body: String)  = {
    // create path and map variables
    val path = "/pet?testing_byte_array=true".replaceAll("\\{format\\}","json")

    val contentTypes = List("application/json", "application/xml", "application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = body

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Deletes a pet
   * 
   * @param petId Pet id to delete 
   * @param apiKey  (optional)
   * @return void
   */
  def deletePet (petId: Long, apiKey: String)  = {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    headerParams += "api_key" -> apiKey
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "DELETE", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma separated strings
   * @param status Status values that need to be considered for query (optional, default to available)
   * @return List[Pet]
   */
  def findPetsByStatus (status: List[String] /* = available */) : Option[List[Pet]] = {
    // create path and map variables
    val path = "/pet/findByStatus".replaceAll("\\{format\\}","json")

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    if(String.valueOf(status) != "null") queryParams += "status" -> status.toString
    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "array", classOf[Pet]).asInstanceOf[List[Pet]])
         
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param tags Tags to filter by (optional)
   * @return List[Pet]
   */
  def findPetsByTags (tags: List[String]) : Option[List[Pet]] = {
    // create path and map variables
    val path = "/pet/findByTags".replaceAll("\\{format\\}","json")

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    if(String.valueOf(tags) != "null") queryParams += "tags" -> tags.toString
    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "array", classOf[Pet]).asInstanceOf[List[Pet]])
         
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched 
   * @return Pet
   */
  def getPetById (petId: Long) : Option[Pet] = {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "", classOf[Pet]).asInstanceOf[Pet])
         
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched 
   * @return InlineResponse200
   */
  def getPetByIdInObject (petId: Long) : Option[InlineResponse200] = {
    // create path and map variables
    val path = "/pet/{petId}?response=inline_arbitrary_object".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "", classOf[InlineResponse200]).asInstanceOf[InlineResponse200])
         
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param petId ID of pet that needs to be fetched 
   * @return String
   */
  def petPetIdtestingByteArraytrueGet (petId: Long) : Option[String] = {
    // create path and map variables
    val path = "/pet/{petId}?testing_byte_array=true".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "GET", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           Some(ApiInvoker.deserialize(s, "", classOf[String]).asInstanceOf[String])
         
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Update an existing pet
   * 
   * @param body Pet object that needs to be added to the store (optional)
   * @return void
   */
  def updatePet (body: Pet)  = {
    // create path and map variables
    val path = "/pet".replaceAll("\\{format\\}","json")

    val contentTypes = List("application/json", "application/xml", "application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = body

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      postBody = mp
    }
    else {
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "PUT", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param petId ID of pet that needs to be updated 
   * @param name Updated name of the pet (optional)
   * @param status Updated status of the pet (optional)
   * @return void
   */
  def updatePetWithForm (petId: String, name: String, status: String)  = {
    // create path and map variables
    val path = "/pet/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("application/x-www-form-urlencoded", "application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      mp.field("name", name.toString(), MediaType.MULTIPART_FORM_DATA_TYPE)
      
      mp.field("status", status.toString(), MediaType.MULTIPART_FORM_DATA_TYPE)
      
      postBody = mp
    }
    else {
      formParams += "name" -> name.toString()
      formParams += "status" -> status.toString()
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
  /**
   * uploads an image
   * 
   * @param petId ID of pet to update 
   * @param additionalMetadata Additional data to pass to server (optional)
   * @param file file to upload (optional)
   * @return void
   */
  def uploadFile (petId: Long, additionalMetadata: String, file: File)  = {
    // create path and map variables
    val path = "/pet/{petId}/uploadImage".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}",apiInvoker.escape(petId))

    

    val contentTypes = List("multipart/form-data", "application/json")
    val contentType = contentTypes(0)

    // query params
    val queryParams = new HashMap[String, String]
    val headerParams = new HashMap[String, String]
    val formParams = new HashMap[String, String]

    

    
    
    

    var postBody: AnyRef = null

    if(contentType.startsWith("multipart/form-data")) {
      val mp = new FormDataMultiPart()
      
      mp.field("additionalMetadata", additionalMetadata.toString(), MediaType.MULTIPART_FORM_DATA_TYPE)
      
      mp.field("file", file.getName)
      mp.bodyPart(new FileDataBodyPart("file", file, MediaType.MULTIPART_FORM_DATA_TYPE))
      
      postBody = mp
    }
    else {
      formParams += "additionalMetadata" -> additionalMetadata.toString()
      
      
    }

    try {
      apiInvoker.invokeApi(basePath, path, "POST", queryParams.toMap, formParams.toMap, postBody, headerParams.toMap, contentType) match {
        case s: String =>
           
        case _ => None
      }
    } catch {
      case ex: ApiException if ex.code == 404 => None
      case ex: ApiException => throw ex
    }
  }
  
}
