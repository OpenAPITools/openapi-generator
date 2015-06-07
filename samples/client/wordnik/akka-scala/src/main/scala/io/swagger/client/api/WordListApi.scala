package io.swagger.client.api

object WordListApi {

  /**
   *
   *
   * Expected answers:
   * code 200 : WordList (success)
   * code 400 :  (Invalid ID supplied)
   * code 403 :  (Not Authorized to access WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink permalink of WordList to fetch
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getWordListByPermalink(permalink: String, authToken: String): ApiRequest[WordList] =
    ApiRequest[WordList](ApiMethods.GET, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[WordList](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid ID supplied)
   * code 403 :  (Not Authorized to update WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink permalink of WordList to update
   * @param body Updated WordList
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def updateWordList(permalink: String, body: Option[WordList] = None, authToken: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.PUT, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withBody(body)
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid ID supplied)
   * code 403 :  (Not Authorized to delete WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink ID of WordList to delete
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def deleteWordList(permalink: String, authToken: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.DELETE, "https://api.wordnik.com/v4", "/wordList.json/{permalink}", "application/json")
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid permalink supplied)
   * code 403 :  (Not Authorized to modify WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink permalink of WordList to use
   * @param body Words to remove from WordList
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def deleteWordsFromWordList(permalink: String, body: Seq[StringValue], authToken: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/deleteWords", "application/json")
      .withBody(body)
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid ID supplied)
   * code 403 :  (Not Authorized to access WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink ID of WordList to use
   * @param sortBy Field to sort by
   * @param sortOrder Direction to sort
   * @param skip Results to skip
   * @param limit Maximum number of results to return
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def getWordListWords(permalink: String, sortBy: Option[String] = None, sortOrder: Option[String] = None, skip: Option[Int] = None, limit: Option[Int] = None, authToken: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/words", "application/json")
      .withQueryParam("sortBy", sortBy)
      .withQueryParam("sortOrder", sortOrder)
      .withQueryParam("skip", skip)
      .withQueryParam("limit", limit)
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid permalink supplied)
   * code 403 :  (Not Authorized to access WordList)
   * code 404 :  (WordList not found)
   *
   * @param permalink permalink of WordList to user
   * @param body Array of words to add to WordList
   * @param authToken The auth token of the logged-in user, obtained by calling /account.json/authenticate/{username} (described above)
   */
  def addWordsToWordList(permalink: String, body: Seq[StringValue], authToken: String): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.POST, "https://api.wordnik.com/v4", "/wordList.json/{permalink}/words", "application/json")
      .withBody(body)
      .withPathParam("permalink", permalink)
      .withHeaderParam("auth_token", authToken)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](403)
      .withErrorResponse[Unit](404)


}

