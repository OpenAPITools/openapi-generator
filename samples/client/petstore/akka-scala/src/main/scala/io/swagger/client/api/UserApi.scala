package io.swagger.client.api

import io.swagger.client.model.User
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object UserApi {

  /**
   * This can only be done by the logged in user.
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param Body Created user object
   */
  def createUser(Body: Option[User] = None): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user", "application/json")
      .withBody(Body)
      .withSuccessResponse[Unit](0)
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param Body List of user object
   */
  def createUsersWithArrayInput(Body: Seq[User]): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user/createWithArray", "application/json")
      .withBody(Body)
      .withSuccessResponse[Unit](0)
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param Body List of user object
   */
  def createUsersWithListInput(Body: Seq[User]): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user/createWithList", "application/json")
      .withBody(Body)
      .withSuccessResponse[Unit](0)
      
  /**
   * 
   * Expected answers:
   *   code 200 : String (successful operation)
   *   code 400 :  (Invalid username/password supplied)
   * 
   * @param Username The user name for login
   * @param Password The password for login in clear text
   */
  def loginUser(Username: Option[String] = None, Password: Option[String] = None): ApiRequest[String] =
    ApiRequest[String](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/login", "application/json")
      .withQueryParam("username", Username)
      .withQueryParam("password", Password)
      .withSuccessResponse[String](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   */
  def logoutUser(): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/logout", "application/json")
      .withSuccessResponse[Unit](0)
      
  /**
   * 
   * Expected answers:
   *   code 404 :  (User not found)
   *   code 200 : User (successful operation)
   *   code 400 :  (Invalid username supplied)
   * 
   * @param Username The name that needs to be fetched. Use user1 for testing. 
   */
  def getUserByName(Username: String): ApiRequest[User] =
    ApiRequest[User](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withPathParam("username", Username)
      .withErrorResponse[Unit](404)
      .withSuccessResponse[User](200)
      .withErrorResponse[Unit](400)
      
  /**
   * This can only be done by the logged in user.
   * 
   * Expected answers:
   *   code 404 :  (User not found)
   *   code 400 :  (Invalid user supplied)
   * 
   * @param Username name that need to be deleted
   * @param Body Updated user object
   */
  def updateUser(Username: String, Body: Option[User] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.PUT, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withBody(Body)
      .withPathParam("username", Username)
      .withErrorResponse[Unit](404)
      .withErrorResponse[Unit](400)
      
  /**
   * This can only be done by the logged in user.
   * 
   * Expected answers:
   *   code 404 :  (User not found)
   *   code 400 :  (Invalid username supplied)
   * 
   * @param Username The name that needs to be deleted
   */
  def deleteUser(Username: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.DELETE, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withPathParam("username", Username)
      .withErrorResponse[Unit](404)
      .withErrorResponse[Unit](400)
      


}

