package apis

import json.JsonUtil
import com.wordnik.swagger.core.ApiPropertiesReader
import com.wordnik.client.model.User
import org.scalatra.ScalatraServlet
import org.scalatra.swagger._

import scala.collection.JavaConverters._

class UserApi (implicit val swagger: Swagger) extends ScalatraServlet with SwaggerBase with SwaggerSupport {
  protected def buildFullUrl(path: String) = "http://petstore.swagger.wordnik.com/api/%s" format path

//  val data = new PetData
  val m = JsonUtil.mapper

//  models = Map(swaggerToModel(classOf[Pet]))

  post("/createWithArray",
    summary("Creates list of users with given input array"),
    nickname("createUsersWithArrayInput"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes(""),
    parameters(
      // it's a body param
      Parameter("body", "List of user object",
        dataType = DataType("Array[User]"),
        paramType = ParamType.Body)
      
      )) {
  }

  post("/",
    summary("Create user"),
    nickname("createUser"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes("This can only be done by the logged in user."),
    parameters(
      // it's a body param
      Parameter("body", "Created user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      
      )) {
  }

  post("/createWithList",
    summary("Creates list of users with given list input"),
    nickname("createUsersWithListInput"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes(""),
    parameters(
      // it's a body param
      Parameter("body", "List of user object",
        dataType = DataType("List[User]"),
        paramType = ParamType.Body)
      
      )) {
  }

  put("/:username",
    summary("Updated user"),
    nickname("updateUser"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes("This can only be done by the logged in user."),
    parameters(
      // it's a path param
      Parameter("username", "name that need to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path)
      // it's a body param
      Parameter("username", "name that need to be deleted",
        dataType = DataType("String"),
        paramType = ParamType.Body)
      ,
      // it's a body param
      Parameter("body", "Updated user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      
      )) {
  }

  delete("/:username",
    summary("Delete user"),
    nickname("deleteUser"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes("This can only be done by the logged in user."),
    parameters(
      // it's a path param
      Parameter("username", "The name that needs to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  get("/:username",
    summary("Get user by user name"),
    nickname("getUserByName"),
    responseClass("User"),
    endpoint("{NOT SURE}"),
    notes(""),
    parameters(
      // it's a path param
      Parameter("username", "The name that needs to be fetched. Use user1 for testing.",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  get("/login",
    summary("Logs user into the system"),
    nickname("loginUser"),
    responseClass("String"),
    endpoint("{NOT SURE}"),
    notes(""),
    parameters(
      // it's a query param
      Parameter("username", "The user name for login",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = false,
        defaultValue = None,
        dataType = DataType("String"))
      ,
      // it's a query param
      Parameter("password", "The password for login in clear text",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = false,
        defaultValue = None,
        dataType = DataType("String"))
      
      )) {
  }

  get("/logout",
    summary("Logs out current logged in user session"),
    nickname("logoutUser"),
    responseClass("void"),
    endpoint("{NOT SURE}"),
    notes(""),
    parameters(
      )) {
  }

  }
