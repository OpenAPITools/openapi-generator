package com.wordnik.client.api

import com.wordnik.client.model.Pet
import java.io.File

import java.io.File

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig, SizeConstraintExceededException}

import scala.collection.JavaConverters._

class PetApi (implicit val swagger: Swagger) extends ScalatraServlet 
    with FileUploadSupport
    with JacksonJsonSupport
    with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "PetApi"
  override protected val applicationName: Option[String] = Some("Pet")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }
  

  val updatePetOperation = (apiOperation[Unit]("updatePet")
      summary "Update an existing pet"
      parameters(bodyParam[Pet]("body").description("").optional)
  )

  put("/pet",operation(updatePetOperation)) {
    
    
    
                
bodyParam[Pet]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val addPetOperation = (apiOperation[Unit]("addPet")
      summary "Add a new pet to the store"
      parameters(bodyParam[Pet]("body").description("").optional)
  )

  post("/pet",operation(addPetOperation)) {
    
    
    
                
bodyParam[Pet]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val findPetsByStatusOperation = (apiOperation[List[Pet]]("findPetsByStatus")
      summary "Finds Pets by status"
      parameters(queryParam[List[String]]("status").description("").optional.defaultValue(available))
  )

  get("/pet/findByStatus",operation(findPetsByStatusOperation)) {
    
    
    
        
      val statusString = params.getAs[String]("status")
      val status = if("multi".equals("default")) {
        statusString match {
          case Some(str) => str.split(",")
          case None => List()
        }
      }
      else
        List()
      
      
            

    
    println("status: " + status)
  
  }

  

  val findPetsByTagsOperation = (apiOperation[List[Pet]]("findPetsByTags")
      summary "Finds Pets by tags"
      parameters(queryParam[List[String]]("tags").description("").optional)
  )

  get("/pet/findByTags",operation(findPetsByTagsOperation)) {
    
    
    
        
      val tagsString = params.getAs[String]("tags")
      val tags = if("multi".equals("default")) {
        tagsString match {
          case Some(str) => str.split(",")
          case None => List()
        }
      }
      else
        List()
      
      
            

    
    println("tags: " + tags)
  
  }

  

  val getPetByIdOperation = (apiOperation[Pet]("getPetById")
      summary "Find pet by ID"
      parameters(pathParam[Long]("petId").description(""))
  )

  get("/pet/{petId}",operation(getPetByIdOperation)) {
    
    
    
      val petId = params.getOrElse("petId", halt(400))
                

    
    println("petId: " + petId)
  
  }

  

  val updatePetWithFormOperation = (apiOperation[Unit]("updatePetWithForm")
      summary "Updates a pet in the store with form data"
      parameters(pathParam[String]("petId").description(""),
        formParam[String]("name").description("").optional,
        formParam[String]("status").description("").optional)
  )

  post("/pet/{petId}",operation(updatePetWithFormOperation)) {
    
    
    
      val petId = params.getOrElse("petId", halt(400))
                

    
    println("petId: " + petId)
  
    
    
                
      val name = params.getAs[String]("name")
    

    
    println("name: " + name)
  
    
    
                
      val status = params.getAs[String]("status")
    

    
    println("status: " + status)
  
  }

  

  val deletePetOperation = (apiOperation[Unit]("deletePet")
      summary "Deletes a pet"
      parameters(pathParam[Long]("petId").description(""),
        )
  )

  delete("/pet/{petId}",operation(deletePetOperation)) {
    
    
    
      val petId = params.getOrElse("petId", halt(400))
                

    
    println("petId: " + petId)
  
    
    
            
      val apiKey = request.getHeader("apiKey")
        

    
    println("apiKey: " + apiKey)
  
  }

  

  val uploadFileOperation = (apiOperation[Unit]("uploadFile")
      summary "uploads an image"
      parameters(pathParam[Long]("petId").description(""),
        formParam[String]("additionalMetadata").description("").optional,
        formParam[File]("file").description("").optional)
  )

  post("/pet/{petId}/uploadImage",operation(uploadFileOperation)) {
    
    
    
      val petId = params.getOrElse("petId", halt(400))
                

    
    println("petId: " + petId)
  
    
    
                
      val additionalMetadata = params.getAs[String]("additionalMetadata")
    

    
    println("additionalMetadata: " + additionalMetadata)
  
    val file = fileParams("file")
    
    println("file: " + file)
  
  }

}