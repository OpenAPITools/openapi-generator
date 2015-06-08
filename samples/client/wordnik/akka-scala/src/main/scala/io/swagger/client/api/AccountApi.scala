package io.swagger.client.api

object AccountApi {

  /**
   *
   *
   * Expected answers:
   * code 200 : ApiTokenStatus (Usage statistics for the supplied API key)
   * code 400 :  (No token supplied.)
   * code 404 :  (No API account with supplied token.)
   *
   * @param apiKey Wordnik authentication token
   */
  def getApiTokenStatus(apiKey: Option[String] = None): ApiRequest[ApiTokenStatus] =
    ApiRequest[ApiTokenStatus](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/apiTokenStatus", "application/json")
      .withHeaderParam("api_key", apiKey)
      .withSuccessResponse[ApiTokenStatus](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 : AuthenticationToken (A valid authentication token)
   * code 403 :  (Account not available.)
   * code 404 :  (User not found.)
   *
   * @param username A confirmed Wordnik username
   * @param password The user&#39;s password
   */
  def authenticate(username: String, password: String): ApiRequest[AuthenticationToken] =
    ApiRequest[AuthenticationToken](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/authenticate/{username}", "application/json")
      .withQueryParam("password", password)
      .withPathParam("username", username)
      .withSuccessResponse[AuthenticationToken](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 : AuthenticationToken (A valid authentication token)
   * code 403 :  (Account not available.)
   * code 404 :  (User not found.)
   *
   * @param username A confirmed Wordnik username
   * @param body The user&#39;s password
   */
  def authenticatePost(username: String, body: String): ApiRequest[AuthenticationToken] =
    ApiRequest[AuthenticationToken](ApiMethods.POST, "https://api.wordnik.com/v4", "/account.json/authenticate/{username}", "application/json")
      .withBody(body)
      .withPathParam("username", username)
      .withSuccessResponse[AuthenticationToken](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   * Requires a valid auth_token to be set.
   *
   * Expected answers:
   * code 200 : User (The logged-in user)
   * code 403 :  (Not logged in.)
   * code 404 :  (User not found.)
   *
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getLoggedInUser(authToken: String): ApiRequest[User] =
    ApiRequest[User](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/user", "application/json")
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[User](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 : Seq[WordList] (success)
   * code 403 :  (Not authenticated.)
   * code 404 :  (User account not found.)
   *
   * @param authToken auth_token of logged-in user
   * @param skip Results to skip
   * @param limit Maximum number of results to return
   */
  def getWordListsForLoggedInUser(authToken: String, skip: Option[Int] = None, limit: Option[Int] = None): ApiRequest[Seq[WordList]] =
    ApiRequest[Seq[WordList]](ApiMethods.GET, "https://api.wordnik.com/v4", "/account.json/wordLists", "application/json")
      .withQueryParam("skip", skip)
      .withQueryParam("limit", limit)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Seq[WordList]](200)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)


}

