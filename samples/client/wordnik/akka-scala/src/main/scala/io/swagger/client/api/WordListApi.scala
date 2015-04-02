package io.swagger.client.api

import io.swagger.client.model.WordList
import io.swagger.client.model.StringValue
import io.swagger.client.core._
import io.swagger.client.core.CollectionFormats._
import io.swagger.client.core.ApiKeyLocations._

object WordListApi {

  /**
   * 
   * 
   * Expected answers:
   *   code 200 : WordList (success)
   *   code 400 :  (Invalid ID supplied)
   *   code 403 :  (Not Authorized to access WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink permalink of WordList to fetch
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getWordListByPermalink(Permalink: String, AuthToken: String): ApiRequest[WordList] =
    ApiRequest[WordList](ApiMethods.GET, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[WordList](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid ID supplied)
   *   code 403 :  (Not Authorized to update WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink permalink of WordList to update
   * @param Body Updated WordList
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def updateWordList(Permalink: String, Body: Option[WordList] = None, AuthToken: String): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.PUT, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withBody(Body)
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid ID supplied)
   *   code 403 :  (Not Authorized to delete WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink ID of WordList to delete
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def deleteWordList(Permalink: String, AuthToken: String): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.DELETE, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid permalink supplied)
   *   code 403 :  (Not Authorized to modify WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink permalink of WordList to use
   * @param Body Words to remove from WordList
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def deleteWordsFromWordList(Permalink: String, Body: Seq[StringValue], AuthToken: String): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/deleteWords", "application/json")
      .withBody(Body)
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid ID supplied)
   *   code 403 :  (Not Authorized to access WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink ID of WordList to use
   * @param SortBy Field to sort by
   * @param SortOrder Direction to sort
   * @param Skip Results to skip
   * @param Limit Maximum number of results to return
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getWordListWords(Permalink: String, SortBy: Option[String] = None, SortOrder: Option[String] = None, Skip: Option[Int] = None, Limit: Option[Int] = None, AuthToken: String): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.GET, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/words", "application/json")
      .withQueryParam("sortBy", SortBy)
      .withQueryParam("sortOrder", SortOrder)
      .withQueryParam("skip", Skip)
      .withQueryParam("limit", Limit)
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      
  /**
   * 
   * 
   * Expected answers:
   *   code 200 :  (success)
   *   code 400 :  (Invalid permalink supplied)
   *   code 403 :  (Not Authorized to access WordList)
   *   code 404 :  (WordList not found)
   * 
   * @param Permalink permalink of WordList to user
   * @param Body Array of words to add to WordList
   * @param AuthToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def addWordsToWordList(Permalink: String, Body: Seq[StringValue], AuthToken: String): ApiRequest[UnitUnit] =
    ApiRequest[UnitUnit](ApiMethods.POST, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/words", "application/json")
      .withBody(Body)
      .withPathParam("permalink", Permalink)
      .withHeaderParam("auth_token", AuthToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)
      


}

