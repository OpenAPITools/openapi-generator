package apis

import com.wordnik.client.model.User
import com.wordnik.swagger.core.ApiPropertiesReader
import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.scalatra.json._

import scala.collection.JavaConverters._
import org.json4s.{ DefaultFormats, Formats }

import scala.collection.JavaConverters._

class UserApi (implicit val swagger: Swagger) extends ScalatraServlet with TypedParamSupport with JacksonJsonSupport with JValueResult with SwaggerSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "UserApi"

  def swaggerToModel(cls: Class[_]) = {
    val docObj = ApiPropertiesReader.read(cls)
    val name = docObj.getName
    val fields = for (field <- docObj.getFields.asScala.filter(d => d.paramType != null))
      yield (field.name -> ModelField(field.name, field.notes, DataType(field.paramType)))

    Model(name, name, fields.toMap)
  }

  before() {
    contentType = formats("json")
    response.headers += ("Access-Control-Allow-Origin" -> "*")
  }

  post("/createWithArray",
    summary("Creates list of users with given input array"),
    nickname("createUsersWithArrayInput"),
    responseClass("void"),
    endpoint("createWithArray"),
    notes(""),
    parameters(
      Parameter("body", "List of user object",
        dataType = DataType("Array[User]"),
        paramType = ParamType.Body)
      
      )) {
  }

  post("/",
    summary("Create user"),
    nickname("createUser"),
    responseClass("void"),
    endpoint(""),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter("body", "Created user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      
      )) {
  }

  post("/createWithList",
    summary("Creates list of users with given list input"),
    nickname("createUsersWithListInput"),
    responseClass("void"),
    endpoint("createWithList"),
    notes(""),
    parameters(
      Parameter("body", "List of user object",
        dataType = DataType("List[User]"),
        paramType = ParamType.Body)
      
      )) {
  }

  put("/:username",
    summary("Updated user"),
    nickname("updateUser"),
    responseClass("void"),
    endpoint("{username}"),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter("username", "name that need to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path)
      ,
      Parameter("body", "Updated user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      
      )) {
  }

  delete("/:username",
    summary("Delete user"),
    nickname("deleteUser"),
    responseClass("void"),
    endpoint("{username}"),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter("username", "The name that needs to be deleted",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  get("/:username",
    summary("Get user by user name"),
    nickname("getUserByName"),
    responseClass("User"),
    endpoint("{username}"),
    notes(""),
    parameters(
      Parameter("username", "The name that needs to be fetched. Use user1 for testing.",
        dataType = DataType.String,
        paramType = ParamType.Path)
      
      )) {
  }

  get("/login",
    summary("Logs user into the system"),
    nickname("loginUser"),
    responseClass("String"),
    endpoint("login"),
    notes(""),
    parameters(
      Parameter("username", "The user name for login",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = false,
        defaultValue = None,
        dataType = DataType("String"))
      ,
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
    endpoint("logout"),
    notes(""),
    parameters(
      )) {
  }

  }
