package apis

import com.wordnik.client.model.User
import com.wordnik.swagger.core.ApiPropertiesReader

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{ JValueResult, JacksonJsonSupport }

import scala.collection.JavaConverters._

class UserApi (implicit val swagger: Swagger) extends ScalatraServlet 
    with JacksonJsonSupport
    with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "UserApi"
  override protected val applicationName: Option[String] = Some("user")

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }


  val createUsersWithArrayInputOperation = (apiOperation[Unit]("createUsersWithArrayInput")
      summary "Creates list of users with given input array"
      parameters(
        bodyParam[List[User]]("body").description(""))
  )
  

  post("/createWithArray",operation(createUsersWithArrayInputOperation)) {
    val body = parsedBody.extract[List[User]]
    println(body)
  }



  val createUserOperation = (apiOperation[Unit]("createUser")
      summary "Create user"
      parameters(
        bodyParam[User]("body").description(""))
  )
  

  post("/",operation(createUserOperation)) {
    val body = parsedBody.extract[User]
    println(body)
  }



  val createUsersWithListInputOperation = (apiOperation[Unit]("createUsersWithListInput")
      summary "Creates list of users with given list input"
      parameters(
        bodyParam[List[User]]("body").description(""))
  )
  

  post("/createWithList",operation(createUsersWithListInputOperation)) {
    val body = parsedBody.extract[List[User]]
    println(body)
  }



  val updateUserOperation = (apiOperation[Unit]("updateUser")
      summary "Updated user"
      parameters(
        pathParam[String]("username").description(""),bodyParam[User]("body").description(""))
  )
  

  put("/:username",operation(updateUserOperation)) {
    val username = params.getOrElse("username", halt(400))
    println(username)
  val body = parsedBody.extract[User]
    println(body)
  }



  val deleteUserOperation = (apiOperation[Unit]("deleteUser")
      summary "Delete user"
      parameters(
        pathParam[String]("username").description(""))
  )
  

  delete("/:username",operation(deleteUserOperation)) {
    val username = params.getOrElse("username", halt(400))
    println(username)
  }



  val getUserByNameOperation = (apiOperation[User]("getUserByName")
      summary "Get user by user name"
      parameters(
        pathParam[String]("username").description(""))
  )
  

  get("/:username",operation(getUserByNameOperation)) {
    val username = params.getOrElse("username", halt(400))
    println(username)
  }



  val loginUserOperation = (apiOperation[String]("loginUser")
      summary "Logs user into the system"
      parameters(
        queryParam[String]("username").description(""),queryParam[String]("password").description(""))
  )
  

  get("/login",operation(loginUserOperation)) {
    val username = params.getAs[String]("username")
    println(username)
  val password = params.getAs[String]("password")
    println(password)
  }



  val logoutUserOperation = (apiOperation[Unit]("logoutUser")
      summary "Logs out current logged in user session"
      parameters(
        )
  )
  

  get("/logout",operation(logoutUserOperation)) {
    }

}
