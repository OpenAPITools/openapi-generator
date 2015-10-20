package com.wordnik.client.api

import com.wordnik.client.model.User

import java.io.File

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }
import org.scalatra.servlet.{FileUploadSupport, MultipartConfig, SizeConstraintExceededException}

import scala.collection.JavaConverters._

class UserApi (implicit val swagger: Swagger) extends ScalatraServlet 
    with FileUploadSupport
    with JacksonJsonSupport
    with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "UserApi"
  override protected val applicationName: Option[String] = Some("User")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }
  

  val createUserOperation = (apiOperation[Unit]("createUser")
      summary "Create user"
      parameters(bodyParam[User]("body").description("").optional)
  )

  post("/user",operation(createUserOperation)) {
    
    
    
                
bodyParam[User]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val createUsersWithArrayInputOperation = (apiOperation[Unit]("createUsersWithArrayInput")
      summary "Creates list of users with given input array"
      parameters(bodyParam[List[User]]("body").description("").optional)
  )

  post("/user/createWithArray",operation(createUsersWithArrayInputOperation)) {
    
    
    
                
bodyParam[List[User]]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val createUsersWithListInputOperation = (apiOperation[Unit]("createUsersWithListInput")
      summary "Creates list of users with given input array"
      parameters(bodyParam[List[User]]("body").description("").optional)
  )

  post("/user/createWithList",operation(createUsersWithListInputOperation)) {
    
    
    
                
bodyParam[List[User]]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val loginUserOperation = (apiOperation[String]("loginUser")
      summary "Logs user into the system"
      parameters(queryParam[String]("username").description("").optional,
        queryParam[String]("password").description("").optional)
  )

  get("/user/login",operation(loginUserOperation)) {
    
    
    
        
      
      val username = params.getAs[String]("username")
            

    
    println("username: " + username)
  
    
    
        
      
      val password = params.getAs[String]("password")
            

    
    println("password: " + password)
  
  }

  

  val logoutUserOperation = (apiOperation[Unit]("logoutUser")
      summary "Logs out current logged in user session"
      parameters()
  )

  get("/user/logout",operation(logoutUserOperation)) {
    
  }

  

  val getUserByNameOperation = (apiOperation[User]("getUserByName")
      summary "Get user by user name"
      parameters(pathParam[String]("username").description(""))
  )

  get("/user/{username}",operation(getUserByNameOperation)) {
    
    
    
      val username = params.getOrElse("username", halt(400))
                

    
    println("username: " + username)
  
  }

  

  val updateUserOperation = (apiOperation[Unit]("updateUser")
      summary "Updated user"
      parameters(pathParam[String]("username").description(""),
        bodyParam[User]("body").description("").optional)
  )

  put("/user/{username}",operation(updateUserOperation)) {
    
    
    
      val username = params.getOrElse("username", halt(400))
                

    
    println("username: " + username)
  
    
    
                
bodyParam[User]("body").description("").optional
    
    println("body: " + body)
  
  }

  

  val deleteUserOperation = (apiOperation[Unit]("deleteUser")
      summary "Delete user"
      parameters(pathParam[String]("username").description(""))
  )

  delete("/user/{username}",operation(deleteUserOperation)) {
    
    
    
      val username = params.getOrElse("username", halt(400))
                

    
    println("username: " + username)
  
  }

}