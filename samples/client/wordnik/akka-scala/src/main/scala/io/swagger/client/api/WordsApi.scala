package io.swagger.client.api

object WordsApi {

  /**
   *
   *
   * Expected answers:
   * code 200 : WordObject (success)
   * code 404 :  (No word found.)
   *
   * @param hasDictionaryDef Only return words with dictionary definitions
   * @param includePartOfSpeech CSV part-of-speech values to include
   * @param excludePartOfSpeech CSV part-of-speech values to exclude
   * @param minCorpusCount Minimum corpus frequency for terms
   * @param maxCorpusCount Maximum corpus frequency for terms
   * @param minDictionaryCount Minimum dictionary count
   * @param maxDictionaryCount Maximum dictionary count
   * @param minLength Minimum word length
   * @param maxLength Maximum word length
   */
  def getRandomWord(hasDictionaryDef: Option[String] = None, includePartOfSpeech: Option[String] = None, excludePartOfSpeech: Option[String] = None, minCorpusCount: Option[Int] = None, maxCorpusCount: Option[Int] = None, minDictionaryCount: Option[Int] = None, maxDictionaryCount: Option[Int] = None, minLength: Option[Int] = None, maxLength: Option[Int] = None): ApiRequest[WordObject] =
    ApiRequest[WordObject](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/randomWord", "application/json")
      .withQueryParam("hasDictionaryDef", hasDictionaryDef)
      .withQueryParam("includePartOfSpeech", includePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", excludePartOfSpeech)
      .withQueryParam("minCorpusCount", minCorpusCount)
      .withQueryParam("maxCorpusCount", maxCorpusCount)
      .withQueryParam("minDictionaryCount", minDictionaryCount)
      .withQueryParam("maxDictionaryCount", maxDictionaryCount)
      .withQueryParam("minLength", minLength)
      .withQueryParam("maxLength", maxLength)
      .withSuccessResponse[WordObject](200)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 :  (success)
   * code 400 :  (Invalid term supplied.)
   * code 404 :  (No results.)
   *
   * @param hasDictionaryDef Only return words with dictionary definitions
   * @param includePartOfSpeech CSV part-of-speech values to include
   * @param excludePartOfSpeech CSV part-of-speech values to exclude
   * @param minCorpusCount Minimum corpus frequency for terms
   * @param maxCorpusCount Maximum corpus frequency for terms
   * @param minDictionaryCount Minimum dictionary count
   * @param maxDictionaryCount Maximum dictionary count
   * @param minLength Minimum word length
   * @param maxLength Maximum word length
   * @param sortBy Attribute to sort by
   * @param sortOrder Sort direction
   * @param limit Maximum number of results to return
   */
  def getRandomWords(hasDictionaryDef: Option[String] = None, includePartOfSpeech: Option[String] = None, excludePartOfSpeech: Option[String] = None, minCorpusCount: Option[Int] = None, maxCorpusCount: Option[Int] = None, minDictionaryCount: Option[Int] = None, maxDictionaryCount: Option[Int] = None, minLength: Option[Int] = None, maxLength: Option[Int] = None, sortBy: Option[String] = None, sortOrder: Option[String] = None, limit: Option[Int] = None): ApiRequest[Unit] =
    ApiRequest[Unit](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/randomWords", "application/json")
      .withQueryParam("hasDictionaryDef", hasDictionaryDef)
      .withQueryParam("includePartOfSpeech", includePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", excludePartOfSpeech)
      .withQueryParam("minCorpusCount", minCorpusCount)
      .withQueryParam("maxCorpusCount", maxCorpusCount)
      .withQueryParam("minDictionaryCount", minDictionaryCount)
      .withQueryParam("maxDictionaryCount", maxDictionaryCount)
      .withQueryParam("minLength", minLength)
      .withQueryParam("maxLength", maxLength)
      .withQueryParam("sortBy", sortBy)
      .withQueryParam("sortOrder", sortOrder)
      .withQueryParam("limit", limit)
      .withSuccessResponse[Unit](200)
      .withErrorResponse[Unit](400)
      .withErrorResponse[Unit](404)

  /**
   *
   *
   * Expected answers:
   * code 200 : DefinitionSearchResults (success)
   * code 400 :  (Invalid term supplied.)
   *
   * @param query Search term
   * @param findSenseForWord Restricts words and finds closest sense
   * @param includeSourceDictionaries Only include these comma-delimited source dictionaries
   * @param excludeSourceDictionaries Exclude these comma-delimited source dictionaries
   * @param includePartOfSpeech Only include these comma-delimited parts of speech
   * @param excludePartOfSpeech Exclude these comma-delimited parts of speech
   * @param minCorpusCount Minimum corpus frequency for terms
   * @param maxCorpusCount Maximum corpus frequency for terms
   * @param minLength Minimum word length
   * @param maxLength Maximum word length
   * @param expandTerms Expand terms
   * @param includeTags Return a closed set of XML tags in response
   * @param sortBy Attribute to sort by
   * @param sortOrder Sort direction
   * @param skip Results to skip
   * @param limit Maximum number of results to return
   */
  def reverseDictionary(query: String, findSenseForWord: Option[String] = None, includeSourceDictionaries: Option[String] = None, excludeSourceDictionaries: Option[String] = None, includePartOfSpeech: Option[String] = None, excludePartOfSpeech: Option[String] = None, minCorpusCount: Option[Int] = None, maxCorpusCount: Option[Int] = None, minLength: Option[Int] = None, maxLength: Option[Int] = None, expandTerms: Option[String] = None, includeTags: Option[String] = None, sortBy: Option[String] = None, sortOrder: Option[String] = None, skip: Option[String] = None, limit: Option[Int] = None): ApiRequest[DefinitionSearchResults] =
    ApiRequest[DefinitionSearchResults](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/reverseDictionary", "application/json")
      .withQueryParam("query", query)
      .withQueryParam("findSenseForWord", findSenseForWord)
      .withQueryParam("includeSourceDictionaries", includeSourceDictionaries)
      .withQueryParam("excludeSourceDictionaries", excludeSourceDictionaries)
      .withQueryParam("includePartOfSpeech", includePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", excludePartOfSpeech)
      .withQueryParam("minCorpusCount", minCorpusCount)
      .withQueryParam("maxCorpusCount", maxCorpusCount)
      .withQueryParam("minLength", minLength)
      .withQueryParam("maxLength", maxLength)
      .withQueryParam("expandTerms", expandTerms)
      .withQueryParam("includeTags", includeTags)
      .withQueryParam("sortBy", sortBy)
      .withQueryParam("sortOrder", sortOrder)
      .withQueryParam("skip", skip)
      .withQueryParam("limit", limit)
      .withSuccessResponse[DefinitionSearchResults](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 200 : WordSearchResults (success)
   * code 400 :  (Invalid query supplied.)
   *
   * @param query Search query
   * @param caseSensitive Search case sensitive
   * @param includePartOfSpeech Only include these comma-delimited parts of speech
   * @param excludePartOfSpeech Exclude these comma-delimited parts of speech
   * @param minCorpusCount Minimum corpus frequency for terms
   * @param maxCorpusCount Maximum corpus frequency for terms
   * @param minDictionaryCount Minimum number of dictionary entries for words returned
   * @param maxDictionaryCount Maximum dictionary definition count
   * @param minLength Minimum word length
   * @param maxLength Maximum word length
   * @param skip Results to skip
   * @param limit Maximum number of results to return
   */
  def searchWords(query: String, caseSensitive: Option[String] = None, includePartOfSpeech: Option[String] = None, excludePartOfSpeech: Option[String] = None, minCorpusCount: Option[Int] = None, maxCorpusCount: Option[Int] = None, minDictionaryCount: Option[Int] = None, maxDictionaryCount: Option[Int] = None, minLength: Option[Int] = None, maxLength: Option[Int] = None, skip: Option[Int] = None, limit: Option[Int] = None): ApiRequest[WordSearchResults] =
    ApiRequest[WordSearchResults](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/search/{query}", "application/json")
      .withQueryParam("caseSensitive", caseSensitive)
      .withQueryParam("includePartOfSpeech", includePartOfSpeech)
      .withQueryParam("excludePartOfSpeech", excludePartOfSpeech)
      .withQueryParam("minCorpusCount", minCorpusCount)
      .withQueryParam("maxCorpusCount", maxCorpusCount)
      .withQueryParam("minDictionaryCount", minDictionaryCount)
      .withQueryParam("maxDictionaryCount", maxDictionaryCount)
      .withQueryParam("minLength", minLength)
      .withQueryParam("maxLength", maxLength)
      .withQueryParam("skip", skip)
      .withQueryParam("limit", limit)
      .withPathParam("query", query)
      .withSuccessResponse[WordSearchResults](200)
      .withErrorResponse[Unit](400)

  /**
   *
   *
   * Expected answers:
   * code 0 : WordOfTheDay (success)
   *
   * @param date Fetches by date in yyyy-MM-dd
   */
  def getWordOfTheDay(date: Option[String] = None): ApiRequest[WordOfTheDay] =
    ApiRequest[WordOfTheDay](ApiMethods.GET, "https://api.wordnik.com/v4", "/words.json/wordOfTheDay", "application/json")
      .withQueryParam("date", date)
      .withDefaultSuccessResponse[WordOfTheDay]


}

