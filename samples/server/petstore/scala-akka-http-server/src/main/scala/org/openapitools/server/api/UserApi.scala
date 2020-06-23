package org.openapitools.server.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.http.scaladsl.unmarshalling.FromStringUnmarshaller
import org.openapitools.server.AkkaHttpHelper._
import org.openapitools.server.model.User


class UserApi(
    userService: UserApiService,
    userMarshaller: UserApiMarshaller
) {

  
  import userMarshaller._

  lazy val route: Route =
    path("user") { 
      post {  
            entity(as[User]){ user =>
              userService.createUser(user = user)
            }
      }
    } ~
    path("user" / "createWithArray") { 
      post {  
            entity(as[Seq[User]]){ user =>
              userService.createUsersWithArrayInput(user = user)
            }
      }
    } ~
    path("user" / "createWithList") { 
      post {  
            entity(as[Seq[User]]){ user =>
              userService.createUsersWithListInput(user = user)
            }
      }
    } ~
    path("user" / Segment) { (username) => 
      delete {  
            userService.deleteUser(username = username)
      }
    } ~
    path("user" / Segment) { (username) => 
      get {  
            userService.getUserByName(username = username)
      }
    } ~
    path("user" / "login") { 
      get { 
        parameters("username".as[String], "password".as[String]) { (username, password) => 
            userService.loginUser(username = username, password = password)
        }
      }
    } ~
    path("user" / "logout") { 
      get {  
            userService.logoutUser()
      }
    } ~
    path("user" / Segment) { (username) => 
      put {  
            entity(as[User]){ user =>
              userService.updateUser(username = username, user = user)
            }
      }
    }
}


trait UserApiService {

  def createUserDefault(statusCode: Int): Route =
    complete((statusCode, "successful operation"))
  /**
   * Code: 0, Message: successful operation
   */
  def createUser(user: User): Route

  def createUsersWithArrayInputDefault(statusCode: Int): Route =
    complete((statusCode, "successful operation"))
  /**
   * Code: 0, Message: successful operation
   */
  def createUsersWithArrayInput(user: Seq[User]): Route

  def createUsersWithListInputDefault(statusCode: Int): Route =
    complete((statusCode, "successful operation"))
  /**
   * Code: 0, Message: successful operation
   */
  def createUsersWithListInput(user: Seq[User]): Route

  def deleteUser400: Route =
    complete((400, "Invalid username supplied"))
  def deleteUser404: Route =
    complete((404, "User not found"))
  /**
   * Code: 400, Message: Invalid username supplied
   * Code: 404, Message: User not found
   */
  def deleteUser(username: String): Route

  def getUserByName200(responseUser: User)(implicit toEntityMarshallerUser: ToEntityMarshaller[User]): Route =
    complete((200, responseUser))
  def getUserByName400: Route =
    complete((400, "Invalid username supplied"))
  def getUserByName404: Route =
    complete((404, "User not found"))
  /**
   * Code: 200, Message: successful operation, DataType: User
   * Code: 400, Message: Invalid username supplied
   * Code: 404, Message: User not found
   */
  def getUserByName(username: String)
      (implicit toEntityMarshallerUser: ToEntityMarshaller[User]): Route

  def loginUser200(responseString: String)(implicit toEntityMarshallerString: ToEntityMarshaller[String]): Route =
    complete((200, responseString))
  def loginUser400: Route =
    complete((400, "Invalid username/password supplied"))
  /**
   * Code: 200, Message: successful operation, DataType: String
   * Code: 400, Message: Invalid username/password supplied
   */
  def loginUser(username: String, password: String): Route

  def logoutUserDefault(statusCode: Int): Route =
    complete((statusCode, "successful operation"))
  /**
   * Code: 0, Message: successful operation
   */
  def logoutUser(): Route

  def updateUser400: Route =
    complete((400, "Invalid user supplied"))
  def updateUser404: Route =
    complete((404, "User not found"))
  /**
   * Code: 400, Message: Invalid user supplied
   * Code: 404, Message: User not found
   */
  def updateUser(username: String, user: User): Route

}

trait UserApiMarshaller {
  implicit def fromEntityUnmarshallerUser: FromEntityUnmarshaller[User]

  implicit def fromEntityUnmarshallerUserList: FromEntityUnmarshaller[Seq[User]]



  implicit def toEntityMarshallerUser: ToEntityMarshaller[User]

}

