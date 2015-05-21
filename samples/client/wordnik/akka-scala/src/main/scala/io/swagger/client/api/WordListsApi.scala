package io.swagger.client.api

import io.swagger.client.model.WordList
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object WordListsApi {

  /**
   * 
   * 
   * Expected answers:
   *   code 200 : WordList (success)
   *   code 400 :  (Invalid WordList supplied or mandatory fields are missing)
   *   code 403 :  (Not authenticated)
   *   code 404 :  (WordList owner not found)
   * 
   * @param body WordList to create
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def createWordList(body: Option[WordList] = None, authToken: String): ApiRequest[WordList] =
    ApiRequest[WordList](ApiMethods.POST, "https://api.wordnik.com/v4", "/wordLists.json", "application/json")
      .withBody(body)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[WordList](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      


}

