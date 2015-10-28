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
   * @param body Created user object
   */
  def createUser(body: Option[User] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user", "application/json")
      .withBody(body)
      .withDefaultSuccessResponse[Unit]
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param body List of user object
   */
  def createUsersWithArrayInput(body: Seq[User]): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user/createWithArray", "application/json")
      .withBody(body)
      .withDefaultSuccessResponse[Unit]
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   * 
   * @param body List of user object
   */
  def createUsersWithListInput(body: Seq[User]): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "http://petstore.swagger.io/v2", "/user/createWithList", "application/json")
      .withBody(body)
      .withDefaultSuccessResponse[Unit]
      
  /**
   * 
   * Expected answers:
   *   code 200 : String (successful operation)
   *   code 400 :  (Invalid username/password supplied)
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   */
  def loginUser(username: Option[String] = None, password: Option[String] = None): ApiRequest[String] =
    ApiRequest[String](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/login", "application/json")
      .withQueryParam("username", username)
      .withQueryParam("password", password)
      .withSuccessResponse[String](200)
      .withErrorResponse[Unit](400)
      
  /**
   * 
   * Expected answers:
   *   code 0 :  (successful operation)
   */
  def logoutUser(): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/logout", "application/json")
      .withDefaultSuccessResponse[Unit]
      
  /**
   * 
   * Expected answers:
   *   code 200 : User (successful operation)
   *   code 400 :  (Invalid username supplied)
   *   code 404 :  (User not found)
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   */
  def getUserByName(username: String): ApiRequest[User] =
    ApiRequest[User](ApiMethods.GET, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withPathParam("username", username)
      .withSuccessResponse[User](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * This can only be done by the logged in user.
   * 
   * Expected answers:
   *   code 400 :  (Invalid user supplied)
   *   code 404 :  (User not found)
   * 
   * @param username name that need to be deleted
   * @param body Updated user object
   */
  def updateUser(username: String, body: Option[User] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.PUT, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withBody(body)
      .withPathParam("username", username)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * This can only be done by the logged in user.
   * 
   * Expected answers:
   *   code 400 :  (Invalid username supplied)
   *   code 404 :  (User not found)
   * 
   * @param username The name that needs to be deleted
   */
  def deleteUser(username: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.DELETE, "http://petstore.swagger.io/v2", "/user/{username}", "application/json")
      .withPathParam("username", username)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      


}

