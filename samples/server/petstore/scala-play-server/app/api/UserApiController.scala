package api

import org.openapitools.OpenApiExceptions
import javax.inject.{Inject, Singleton}
import play.api.libs.json._
import play.api.mvc._
import model.User


@Singleton
class UserApiController @Inject()(cc: ControllerComponents, api: UserApi) extends AbstractController(cc) {
  /**
    * POST /v2/user
    */
  def createUser(): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val user = request.body.asJson.map(_.as[User]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "user")
      }
      api.createUser(user)
    }

    executeApi()
    Ok
  }

  /**
    * POST /v2/user/createWithArray
    */
  def createUsersWithArrayInput(): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val user = request.body.asJson.map(_.as[List[User]]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "user")
      }
      api.createUsersWithArrayInput(user)
    }

    executeApi()
    Ok
  }

  /**
    * POST /v2/user/createWithList
    */
  def createUsersWithListInput(): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val user = request.body.asJson.map(_.as[List[User]]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "user")
      }
      api.createUsersWithListInput(user)
    }

    executeApi()
    Ok
  }

  /**
    * DELETE /v2/user/:username
    * @param username The name that needs to be deleted
    */
  def deleteUser(username: String): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      api.deleteUser(username)
    }

    executeApi()
    Ok
  }

  /**
    * GET /v2/user/:username
    * @param username The name that needs to be fetched. Use user1 for testing.
    */
  def getUserByName(username: String): Action[AnyContent] = Action { request =>
    def executeApi(): User = {
      api.getUserByName(username)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * GET /v2/user/login?username=[value]&password=[value]
    */
  def loginUser(): Action[AnyContent] = Action { request =>
    def executeApi(): String = {
      val username = request.getQueryString("username")
        .getOrElse {
          throw new OpenApiExceptions.MissingRequiredParameterException("username", "query string")
        }
      val password = request.getQueryString("password")
        .getOrElse {
          throw new OpenApiExceptions.MissingRequiredParameterException("password", "query string")
        }
      api.loginUser(username, password)
    }

    val result = executeApi()
    val json = Json.toJson(result)
    Ok(json)
  }

  /**
    * GET /v2/user/logout
    */
  def logoutUser(): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      api.logoutUser()
    }

    executeApi()
    Ok
  }

  /**
    * PUT /v2/user/:username
    * @param username name that need to be deleted
    */
  def updateUser(username: String): Action[AnyContent] = Action { request =>
    def executeApi(): Unit = {
      val user = request.body.asJson.map(_.as[User]).getOrElse {
        throw new OpenApiExceptions.MissingRequiredParameterException("body", "user")
      }
      api.updateUser(username, user)
    }

    executeApi()
    Ok
  }

  private def splitCollectionParam(paramValues: String, collectionFormat: String): List[String] = {
    val splitBy =
      collectionFormat match {
        case "csv" => ",+"
        case "tsv" => "\t+"
        case "ssv" => " +"
        case "pipes" => "|+"
      }

    paramValues.split(splitBy).toList
  }
}
