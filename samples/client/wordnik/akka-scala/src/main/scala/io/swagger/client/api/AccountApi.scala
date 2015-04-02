package io.swagger.client.api

import io.swagger.client.model.ApiTokenStatus
import io.swagger.client.model.AuthenticationToken
import io.swagger.client.model.User
import io.swagger.client.model.WordList
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object AccountApi {

  /**
   * 
   * 
   * Expected answers:
   *   code 200 : ApiTokenStatus (Usage statistics for the supplied API key)
   *   code 400 :  (No token supplied.)
   *   code 404 :  (No API account with supplied token.)
   * 
   * @param ApiKey Wordnik authentication token
   */
  def getApiTokenStatus(ApiKey: Option[String] = None): ApiRequest[ApiTokenStatus] =
    ApiRequest[ApiTokenStatus](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/apiTokenStatus", "application/json")
      .withHeaderParam("api_key", ApiKey)
      .withSuccessResponse[ApiTokenStatus](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : AuthenticationToken (A valid authentication token)
   *   code 403 :  (Account not available.)
   *   code 404 :  (User not found.)
   * 
   * @param Username A confirmed Wordnik username
   * @param Password The user&#39;s password
   */
  def authenticate(Username: String, Password: String): ApiRequest[AuthenticationToken] =
    ApiRequest[AuthenticationToken](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/authenticate/{username}", "application/json")
      .withQueryParam("password", Password)
      .withPathParam("username", Username)
      .withSuccessResponse[AuthenticationToken](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : AuthenticationToken (A valid authentication token)
   *   code 403 :  (Account not available.)
   *   code 404 :  (User not found.)
   * 
   * @param Username A confirmed Wordnik username
   * @param Body The user&#39;s password
   */
  def authenticatePost(Username: String, Body: String): ApiRequest[AuthenticationToken] =
    ApiRequest[AuthenticationToken](ApiMethods.POST, "https://api.wordnik.com/v4", "/account.json/authenticate/{username}", "application/json")
      .withBody(Body)
      .withPathParam("username", Username)
      .withSuccessResponse[AuthenticationToken](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * Requires a valid auth_token to be set.
   * 
   * Expected answers:
   *   code 200 : User (The logged-in user)
   *   code 403 :  (Not logged in.)
   *   code 404 :  (User not found.)
   * 
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getLoggedInUser(AuthToken: String): ApiRequest[User] =
    ApiRequest[User](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/user", "application/json")
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[User](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 : Seq[WordList] (success)
   *   code 403 :  (Not authenticated.)
   *   code 404 :  (User account not found.)
   * 
   * @param AuthToken auth_token of logged-in user
   * @param Skip Results to skip
   * @param Limit Maximum number of results to return
   */
  def getWordListsForLoggedInUser(AuthToken: String, Skip: Option[Int] = None, Limit: Option[Int] = None): ApiRequest[Seq[WordList]] =
    ApiRequest[Seq[WordList]](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/wordLists", "application/json")
      .withQueryParam("skip", Skip)
      .withQueryParam("limit", Limit)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Seq[WordList]](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      


}

