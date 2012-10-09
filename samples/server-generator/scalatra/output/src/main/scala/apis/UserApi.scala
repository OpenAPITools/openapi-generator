package apis

import com.wordnik.client.model.User
import com.wordnik.swagger.core.ApiPropertiesReader

import org.scalatra.{ TypedParamSupport, ScalatraServlet }
import org.scalatra.swagger._
import org.json4s._
import org.json4s.JsonDSL._
import org.scalatra.json.{JValueResult, NativeJsonSupport}

import scala.collection.JavaConverters._

class UserApi (implicit val swagger: Swagger) extends ScalatraServlet 
      with TypedParamSupport 
      with NativeJsonSupport 
      with JValueResult 
      with SwaggerSupport  {
  protected implicit val jsonFormats: Formats = DefaultFormats

  protected val applicationDescription: String = "UserApi"
  override protected val applicationName: Option[String] = Some("user")

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
      Parameter(name = "body",
        description = "List of user object",
        dataType = DataType("Array[User]"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  post("/",
    summary("Create user"),
    nickname("createUser"),
    responseClass("void"),
    endpoint(""),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter(name = "body",
        description = "Created user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  post("/createWithList",
    summary("Creates list of users with given list input"),
    nickname("createUsersWithListInput"),
    responseClass("void"),
    endpoint("createWithList"),
    notes(""),
    parameters(
      Parameter(name = "body",
        description = "List of user object",
        dataType = DataType("List[User]"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  put("/:username",
    summary("Updated user"),
    nickname("updateUser"),
    responseClass("void"),
    endpoint("{username}"),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter(name = "username", 
        description = "name that need to be deleted",
        dataType = DataType.String,
        defaultValue = None,
        paramType = ParamType.Path)
      ,Parameter(name = "body",
        description = "Updated user object",
        dataType = DataType("User"),
        paramType = ParamType.Body)
      )) {

    // do something
  }

  delete("/:username",
    summary("Delete user"),
    nickname("deleteUser"),
    responseClass("void"),
    endpoint("{username}"),
    notes("This can only be done by the logged in user."),
    parameters(
      Parameter(name = "username", 
        description = "The name that needs to be deleted",
        dataType = DataType.String,
        defaultValue = None,
        paramType = ParamType.Path)
      )) {

    // do something
  }

  get("/:username",
    summary("Get user by user name"),
    nickname("getUserByName"),
    responseClass("User"),
    endpoint("{username}"),
    notes(""),
    parameters(
      Parameter(name = "username", 
        description = "The name that needs to be fetched. Use user1 for testing.",
        dataType = DataType.String,
        defaultValue = None,
        paramType = ParamType.Path)
      )) {

    // do something
  }

  get("/login",
    summary("Logs user into the system"),
    nickname("loginUser"),
    responseClass("String"),
    endpoint("login"),
    notes(""),
    parameters(
      Parameter(name = "username", 
        description = "The user name for login",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = false,
        defaultValue = None,
        dataType = DataType("String"))
      ,Parameter(name = "password", 
        description = "The password for login in clear text",
        paramType = ParamType.Query,
        required = true,
        allowMultiple = false,
        defaultValue = None,
        dataType = DataType("String"))
      )) {

    // do something
  }

  get("/logout",
    summary("Logs out current logged in user session"),
    nickname("logoutUser"),
    responseClass("void"),
    endpoint("logout"),
    notes(""),
    parameters(
      )) {

    // do something
  }
}
